get_current_data <- function(board) {
  
  # Read in the pins containing "binary" response data
  binary <- pins::pin_search(
    board = board, 
    search = "binary"
  ) |> 
    split(.$name) |> 
    purrr::map_dfr(
      function(x) pins::pin_read(board = board, name = x$name), 
      .id = "Pin"
    ) |> 
    tibble::as_tibble() |> 
    dplyr::select(
      User, 
      Group, 
      Question, 
      Response, 
      Confidence, 
      Truth, 
      Brier
    ) |> 
    dplyr::mutate(Confidence = as.numeric(
      stringr::str_replace(
        string = Confidence, 
        pattern = "%", 
        replacement = ""
      )
    ) / 100)
  
  # Read in the pins containing "range" response data
  range <- pins::pin_search(
    board = board, 
    search = "range"
  ) |> 
    split(.$name) |> 
    purrr::map_dfr(
      function(x) pins::pin_read(board = board, name = x$name), 
      .id = "Pin"
    ) |> 
    tibble::as_tibble() |> 
    dplyr::select(
      User, 
      Group, 
      Question, 
      Lower90, 
      Upper90, 
      Truth, 
      RelativeError
    )
  
  # Return a list of the binary & response data frames
  list(
    binary = binary, 
    range = range
  )
  
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

