## UI 
ui <- function() {
  bslib::page_navbar(
    # Set Up Global UI Elements ----------------------------------------------
    title = "Calibration Admin Application",
    theme = app_theme,
    collapsible = TRUE,
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "style.css"
      )
    ),
    bslib::nav_panel(
      title = "Explanation",
      bslib::navset_card_tab(
        title = bslib::card_title("Overview"),
        bslib::card_body(
          p(stringi::stri_rand_lipsum(1)),
          p(stringi::stri_rand_lipsum(1)),
          p(stringi::stri_rand_lipsum(1))
        )
      )
    ),
    mod_question_selection_ui(
      "question_selection",
      tab_title = "Question Selection"
    ),
    mod_question_sets_display_ui(
      "question_sets_display",
      tab_title = "Available Sets"
    ),
    mod_users_table_ui(
      "users_table_display",
      tab_title = "Users"
    ),
    mod_responses_display_ui(
      "binary_responses_display",
      tab_title = "Binary Responses"
    ),
    mod_responses_display_ui(
      "range_responses_display",
      tab_title = "Range Responses"
    )
  )
}

