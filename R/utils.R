get_posit_user <- function(session = getDefaultReactiveDomain()) {
  if (!is.null(session) && !is.null(session$user)) {
    return(session$user)
  }
  
  env_uid <- Sys.getenv("posit_uid", unset = NA)
  if (!is.na(env_uid) && nzchar(env_uid)) {
    return(env_uid)
  }
  
  stop("Unable to determine user: no session user and posit_uid environment variable is not set.")
}

get_full_data <- function(gs_url) {
  
  # Read in the "Binary" questions from the Google Sheet
  binary_questions <- googlesheets4::read_sheet(
    ss = gs_url, 
    sheet = "Binary_questions"
  ) |> 
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_remove(.x, "Question_")), dplyr::starts_with("Question_"))
  
  
  # Read in the "Range" questions from the Google Sheet
  range_questions <- googlesheets4::read_sheet(
    ss = gs_url, 
    sheet = "Range_questions"
  ) |>  
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_remove(.x, "Question_")), dplyr::starts_with("Question_"))
  
  # Return the two data frames as a list
  list(
    binary = binary_questions, 
    range = range_questions
  )
  
}

report_table_height_script <- function(output_id, input_id) {
  # Create a safe function name by replacing hyphens with underscores
  safe_fn_name <- gsub("-", "_", output_id)
  
  tags$script(HTML(glue::glue("
    function reportHeight_{safe_fn_name}() {{
      var el = document.getElementById('{output_id}');
      if (el) {{
        var height = el.offsetHeight;
        Shiny.setInputValue('{input_id}', height);
      }}
    }}
    window.addEventListener('resize', reportHeight_{safe_fn_name});
    window.addEventListener('load', reportHeight_{safe_fn_name});
    setInterval(reportHeight_{safe_fn_name}, 1000);
  ")))
}

# Function to codify, compress, encrypt, and URL encode the data
base_encrypt <- function(codified_string, key, nonce) {
  compressed_string <- memCompress(charToRaw(codified_string), type = "gzip")
  encrypted_string <- sodium::data_encrypt(compressed_string, key, nonce)
  encoded_string <- base64enc::base64encode(encrypted_string)
  url_encoded_string <- URLencode(encoded_string, reserved = TRUE)
  return(url_encoded_string)
}

# Function to decode, decompress, decrypt, and URL decode the data
base_decrypt <- function(url_encoded_string, key, nonce) {
  encoded_string <- URLdecode(url_encoded_string)
  decoded_encrypted_string <- base64enc::base64decode(encoded_string)
  decrypted_compressed <- sodium::data_decrypt(decoded_encrypted_string, key, nonce)
  decompressed_string <- rawToChar(memDecompress(decrypted_compressed, type = "gzip"))
  return(decompressed_string)
}

encrypt_question_index <- function(data, key, nonce) {
  codified_string <- paste(
    data$Type, data$Round, data$Number, sep = "x", collapse = "/"
  )
  encoded_string <- base_encrypt(codified_string, key, nonce)
  return(encoded_string)
}

decrypt_question_index <- function(encoded_string, key, nonce) {
  decompressed_string <- base_decrypt(encoded_string, key, nonce)
  rows <- strsplit(decompressed_string, "/")[[1]]
  decompressed_question_index <- map_dfr(rows, ~ {
    values <- strsplit(.x, "x")[[1]]
    tibble(Type = as.character(values[1]), Round = as.integer(values[2]), Number = as.integer(values[3]))
  })
  return(decompressed_question_index)
}