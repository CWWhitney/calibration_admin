

# Load global packages
library(pins)
library(purrr)
library(fs)

# Connect to the {pins} board containing the workshop user data
board <- pins::board_rsconnect(auth = "envvar")

# Load custom functions
fs::dir_ls("R") %>% 
  purrr::map(~ source(.x)) %>% 
  purrr::quietly()