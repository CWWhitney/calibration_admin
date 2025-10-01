## SERVER 
server <- function(input, output, session) {
  
  #### Selection Screen ------------------------------------------------------
  question_selection_reactives <-
    mod_question_selection_server(
      "question_selection", 
      questions_full
    )
  #### Set Screen ------------------------------------------------------------
  mod_question_sets_display_server(
    "question_sets_display",
    trigger_refresh = question_selection_reactives$trigger_refresh
  )
  #### Users Display ----------------------------------------------------------
  mod_users_table_server(
    "users_table_display",
    loading_function = load_users_table,
    deleting_function = delete_users_entry
  )
  #### Binary Display ----------------------------------------------------------
  mod_responses_display_server(
    "binary_responses_display",
    loading_function = load_binary_responses,
    binary_or_range = "binary"
  )
  #### Range Display ----------------------------------------------------------
  mod_responses_display_server(
    "range_responses_display",
    loading_function = load_range_responses,
    binary_or_range = "range"
  )
  
  
  output$user_info <- renderPrint({
    list(
      username = session$user,
      groups = session$groups
    )
  })
}