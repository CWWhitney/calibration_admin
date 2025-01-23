
# Load global packages
library(pins)
library(purrr)
library(fs)

# Connect to the {pins} board containing the workshop user data
board <- pins::board_connect(auth = "envvar")

# Load custom functions
fs::dir_ls("R") |> 
  purrr::map(~ source(.x)) |> 
  purrr::quietly()


# Define Google API authentication type
# If the Google Sheet is public, simply call `googlesheets4::gs4_death()` here
# to indicate that no authentication is necessary
googlesheets4::gs4_deauth()

# Define the URL of the Google Sheet
google_sheets_url <- "https://docs.google.com/spreadsheets/d/1yTboPXmDMF43YmjsuEH7bbPwcEj4fPfBD68rNWrOPSI/edit?usp=sharing"

questions_full <- get_full_data(
  gs_url = google_sheets_url
)

# Define the language questions will be asked in
language <- "German"

languages <- c(
  "German",
  "English",
  "Kiswahili",
  "Spanish",
  "Vietnamese",
  "Engkis"
)


question_sets_static <- data.frame(
  question_set_name = character(0),
  encrypted_question_set_code = character(0),
  help_videos_active = logical(0),
  group_1 = logical(0),
  group_2 = logical(0),
  group_3 = logical(0),
  group_4 = logical(0),
  group_5 = logical(0),
  group_6 = logical(0),
  group_7 = logical(0),
  group_8 = logical(0),
  group_9 = logical(0),
  group_10 = logical(0)
)


# Generate a 32-byte key for encryption
# sodium::random(32)
our_key <- as.raw(c(0x3f, 0x79, 0x6b, 0x5c, 0xe0, 0x40, 0x8e, 0x0b, 0x64, 
                    0x1c, 0xf6, 0xe7, 0x51, 0xaa, 0xd2, 0xe7, 0x7a, 0x4f, 0x38, 0x32, 
                    0x49, 0x3e, 0x3e, 0x29, 0xed, 0x18, 0xa9, 0xa2, 0x6c, 0x3b, 0x09, 
                    0x71))

# Generate a nonce
# sodium::random(24)
our_nonce <- as.raw(c(0x9a, 0xb0, 0x99, 0x9a, 0xbe, 0x10, 0x59, 0x0d, 0x06, 
                      0x6f, 0x29, 0x0a, 0xb8, 0xf7, 0xc3, 0xdb, 0xa8, 0x58, 0x90, 0x4c, 
                      0x0a, 0xc9, 0xae, 0xc2)) 

