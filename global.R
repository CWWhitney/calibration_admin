## Load Packages -------------------------------------------------------------
library(shiny)
library(DT)
library(rhandsontable) # excel like interactive tables
library(reactable)   # interactive tables
library(echarts4r)   # interactive charts

library(DBI)
library(pool)
library(RSQLite)

library(purrr)   # working with lists
library(dplyr)   # general data prep
library(stringr)   # working with strings

# SETUP ----------------------------------------------------------------------
# "srv/shiny-app-data/database/calibration.db"
#srv/shiny-app-data/database/question_sets.db


# Get the database path from environment variable or use default
db_path <- Sys.getenv("db_con_path", unset = "question_sets.db")

# Connect to the SQLite database
pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = db_path
)

# # Drop the table if it exists
DBI::dbExecute(pool, "DROP TABLE IF EXISTS range_responses;")
DBI::dbExecute(pool, "DROP TABLE IF EXISTS binary_responses;")
DBI::dbExecute(pool, "DROP TABLE IF EXISTS users_table;")
DBI::dbExecute(pool, "DROP TABLE IF EXISTS question_sets;")

# Create the user_responses table
DBI::dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS users_table (
    user_first_name TEXT,
    user_last_name TEXT,
    user_session TEXT UNIQUE,
    workshop_set TEXT,
    round_number INTEGER,
    question_number INTEGER,
    question_type TEXT,
    created TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")


# Create the user_responses table
DBI::dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS binary_responses (
    user_first_name TEXT,
    user_last_name TEXT,
    user_session TEXT,
    workshop_set TEXT,
    round_number INTEGER,
    question_number INTEGER,
    question_text TEXT,
    index_in_set INTEGER,
    response BOOLEAN,
    confidence TEXT,
    truth BOOLEAN,
    brier_score REAL,
    created TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

# Create the user_estimates table
DBI::dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS range_responses (
    user_first_name TEXT,
    user_last_name TEXT,
    user_session TEXT,
    workshop_set TEXT,
    round_number INTEGER,
    question_number INTEGER,
    question_text TEXT,
    index_in_set INTEGER,
    lower_90 REAL,
    upper_90 REAL,
    truth REAL,
    relative_error REAL,
    created TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

# Create the question_sets table
DBI::dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS question_sets (
    question_set_name TEXT UNIQUE,
    encrypted_question_set_code TEXT,
    question_set_active BOOLEAN,
    help_videos_active BOOLEAN,
    round_1 BOOLEAN,
    round_2 BOOLEAN,
    round_3 BOOLEAN,
    round_4 BOOLEAN,
    round_5 BOOLEAN,
    round_6 BOOLEAN,
    round_7 BOOLEAN,
    round_8 BOOLEAN,
    round_9 BOOLEAN,
    round_10 BOOLEAN,
    created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by TEXT
  );
")

load_question_sets <- function() {
  DBI::dbReadTable(pool, "question_sets")
}

load_users_table <- function() {
  DBI::dbReadTable(pool, "users_table")
}

delete_users_entry <- function(user_first_name, user_last_name, user_session) {
  DBI::dbExecute(
    pool,
    "DELETE FROM users_table WHERE user_first_name = ? AND user_last_name = ? AND user_session = ?", 
    params = list(user_first_name, user_last_name, user_session)
    )
  DBI::dbExecute(
    pool,
    "DELETE FROM binary_responses WHERE user_first_name = ? AND user_last_name = ? AND user_session = ?", 
    params = list(user_first_name, user_last_name, user_session)
  )
  DBI::dbExecute(
    pool,
    "DELETE FROM range_responses WHERE user_first_name = ? AND user_last_name = ? AND user_session = ?", 
    params = list(user_first_name, user_last_name, user_session)
  )
}

load_binary_responses <- function() {
  DBI::dbReadTable(pool, "binary_responses")
}

load_range_responses <- function() {
  DBI::dbReadTable(pool, "range_responses")
}

# Ensure the pool is closed when the app stops
onStop(function() {
  poolClose(pool)
})

# Sourcing uitls explicitly, since the R/ folder gets sourced after global.R
source("R/utils.R", encoding = "UTF-8")

googlesheets4::gs4_deauth()

questions_full <- get_full_data(
  gs_url = Sys.getenv("google_sheets_url")
) |> purrr::map(
  \(x) x |>
    dplyr::mutate(Number = as.integer(Number)) |>
    dplyr::mutate(Answer = as.character(Answer))
)


# Define the language questions will be asked in
language <- "English"

languages <- c(
  "English",
  "German",
  "Kiswahili",
  "Spanish",
  "Vietnamese",
  "Engkis"
)


## Define Google API authentication type
## If the Google Sheet is public, simply call `googlesheets4::gs4_death()` here
## to indicate that no authentication is necessary


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


# Build UI Theme ---------------------------------------------------------------
## Develop the Bootstrap theme for the app

app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  dark = "#153015",
  bg = "#153015", 
  fg = "#FFFFFF", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00",   # Bonn yellow
  warning = "#FBBA00", # Bonn yellow
  danger = "#FE6100",
  "body-bg" = "#153015",
  "navbar-bg" = "#153015",
  "navbar-light-color" = "white",
  "navbar-light-hover-color" = "white",
  "nav-tabs-link-active-color" = "white",
  "nav-link-color" = "white",
  "nav-link-hover-color" = "#FBBA00"
) |> bslib::bs_add_variables(
  # "modal-backdrop-opacity" = 1,
  # "modal-backdrop-bg" = "grey"
) 