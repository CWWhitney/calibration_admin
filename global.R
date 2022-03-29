

# Load global packages
library(pins)

# Connect to the {pins} board containing the workshop user data
board <- pins::board_rsconnect()

# Load custom functions
fs::dir_ls("R") %>% 
  purrr::map(~ source(.x)) %>% 
  purrr::quietly()