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
    data$Type, data$Group, data$Number, sep = "x", collapse = "/"
  )
  encoded_string <- base_encrypt(codified_string, key, nonce)
  return(encoded_string)
}

decrypt_question_index <- function(encoded_string, key, nonce) {
  decompressed_string <- base_decrypt(encoded_string, key, nonce)
  rows <- strsplit(decompressed_string, "/")[[1]]
  decompressed_question_index <- map_dfr(rows, ~ {
    values <- strsplit(.x, "x")[[1]]
    tibble(Type = as.character(values[1]), Group = as.integer(values[2]), Number = as.numeric(values[3]))
  })
  return(decompressed_question_index)
}