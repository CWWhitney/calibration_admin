


get_current_data <- function(board) {
  
  # Read in the pins containing "binary" response data
  binary <- pins::pin_search(
    board = board, 
    search = "binary"
  ) %>% 
    split(.$name) %>% 
    purrr::map_dfr(
      function(x) pins::pin_read(board = board, name = x$name), 
      .id = "Pin"
    ) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(
      User, 
      Group, 
      Question, 
      Response, 
      Confidence, 
      Truth, 
      Brier
    ) %>% 
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
  ) %>% 
    split(.$name) %>% 
    purrr::map_dfr(
      function(x) pins::pin_read(board = board, name = x$name), 
      .id = "Pin"
    ) %>% 
    tibble::as_tibble() %>% 
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



