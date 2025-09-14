library(DBI)
library(RSQLite)
library(lubridate)

# Get the database path from environment variable or use default
db_path <- Sys.getenv("db_con_path", unset = "question_sets.db")

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), db_path)

# Drop the table if it exists
dbExecute(con, "DROP TABLE IF EXISTS question_sets;")

# Create the question_sets table
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS question_sets (
    question_set_name TEXT,
    encrypted_question_set_code TEXT,
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
    created TIMESTAMP,
    created_by TEXT
  );
")

# Insert example data
dbExecute(con, "
  INSERT INTO question_sets (
    question_set_name,
    encrypted_question_set_code,
    help_videos_active,
    round_1, round_2, round_3, round_4, round_5,
    round_6, round_7, round_8, round_9, round_10,
    created, created_by
  ) VALUES (
    ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
  );
", params = list(
  "Set A",
  "0o0y1lQXAuI28JCv%2BVNr2hW84zfmpe3x4ZE7tvAOjxjlisVQAjRj4ZuVF8YG",
  TRUE,
  TRUE,
  FALSE,
  TRUE,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  Sys.time(),
  "admin"
))

# Disconnect
dbDisconnect(con)
