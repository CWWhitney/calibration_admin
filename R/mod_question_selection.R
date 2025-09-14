##******************************************************************************
##*
##* This is the question_selection shiny module which allows users to select questions
##* from a predefined set, customize the selection, and save the selected set.
##*
##******************************************************************************


#' Question Selection UI
#'
#' A Shiny module UI for creating and customizing question sets from predefined binary and range questions.
#' Users can filter by language, choose which columns to display, assign questions to rounds, and save the configuration.
#'
#' @param id The namespace ID of the module.
#' @param tab_title The title of the tab panel where the module UI will be displayed.
#'
#' @return A `bslib::nav_panel` object for inclusion in a Shiny UI.
#'
#' @details
#' The UI includes:
#' - A sidebar for naming the question set, toggling help videos, activating rounds, and saving/resetting selections.
#' - Tabs for selected questions, binary questions, and range questions.
#' - A popover for modifying language and column display settings.
#'
#' @seealso [mod_question_selection_server()]
#'
#' @importFrom shiny NS textInput actionButton icon
#' @importFrom shinyWidgets checkboxGroupButtons pickerInput updateCheckboxGroupButtons
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom bslib nav_panel navset_card_tab sidebar nav_item card_title
#' @importFrom bsicons bs_icon
#'
#' @export
mod_question_selection_ui <- function(id, tab_title) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = tab_title,
    bslib::navset_card_tab(
      title = bslib::card_title("Question Set Creator"),
      
      # Sidebar -------------------------------------------------------------
      sidebar = bslib::sidebar(
        shiny::textInput(
          inputId = ns("question_set_name"),
          label = "Select Question Set Name"
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("help_videos_active"),
          label = "Help Videos:",
          choiceNames = "Show",
          choiceValues = "show_help",
          status = "warning"
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("checks_for_question_set"),
          label = "Activate Rounds:",
          choiceNames = "",
          choiceValues = "",
          direction = "vertical",
          status = "warning"
        ),
        shiny::actionButton(
          inputId = ns("save_question_set"),
          label = "Save Question Set",
          icon = shiny::icon("floppy-disk"),
          class = "btn btn-warning",
          width = "200px"
        ),
        shiny::actionButton(
          inputId = ns("load_question_selection"),
          label = "Load Question Set",
          icon = shiny::icon("folder-open"),
          class = "btn btn-warning",
          width = "200px"
        ),
        shiny::actionButton(
          inputId = ns("reset_question_selection"),
          label = "Reset Question Selection",
          icon = shiny::icon("arrow-rotate-left"),
          class = "btn btn-warning",
          width = "200px"
        )
      ),
      
      # Selected Questions Tab ----------------------------------------------
      bslib::nav_panel(
        title = "Selected Questions",
        report_table_height_script(ns("selected_questions_table"), ns("selected_questions_table_height")),
        report_table_height_script(ns("selected_rounds_table"), ns("selected_rounds_table_height")),
        bslib::layout_column_wrap(
          width = NULL,
          style = bslib::css(grid_template_columns = "3fr 1fr"),
          rhandsontable::rHandsontableOutput(ns("selected_questions_table")),
          rhandsontable::rHandsontableOutput(ns("selected_rounds_table"))
        )
      ),
      
      # Binary Questions Tab ------------------------------------------------
      bslib::nav_panel(
        title = "Binary",
        report_table_height_script(ns("binary_table"), ns("binary_table_height")),
        rhandsontable::rHandsontableOutput(ns("binary_table"))
      ),
      
      # Range Questions Tab -------------------------------------------------
      bslib::nav_panel(
        title = "Range",
        report_table_height_script(ns("range_table"), ns("range_table_height")),
        rhandsontable::rHandsontableOutput(ns("range_table"))
      ),
      
      # Display Settings Popover --------------------------------------------
      bslib::nav_item(
        bslib::popover(
          shiny::actionButton(ns("not_used"), label = bsicons::bs_icon("gear-fill")),
          title = "Modify Display",
          shinyWidgets::pickerInput(
            inputId = ns("language"),
            label = "Question Language",
            choices = languages
          ),
          shinyWidgets::pickerInput(
            inputId = ns("show_columns"),
            label = "Columns to display",
            choices = c("Answer", "Source_link", "Comments"),
            selected = "Answer",
            multiple = TRUE
          )
        )
      )
    )
  )
}



#' Question Selection Server
#'
#' A shiny module server function, which handles the logic for selecting questions.
#'
#' @param id The namespace id of the module.
#' @param questions_full Full set of questions
#' @param question_sets Reactive value to store question sets
#'
#' @return Server logic for the question selection module
#'
#' @seealso [mod_question_selection_ui()]
#'
#' @export
#'
#' @inherit mod_question_selection_ui title description details examples
mod_question_selection_server <- function(id, questions_full) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Reactive Values --------------------------------------------------------
    
    # Reactive values to store the table data
    binary_data <- shiny::reactiveVal()
    
    range_data <- shiny::reactiveVal()
    
    # Reactive value to store the selected questions data
    selected_questions <- shiny::reactiveVal(
      data.frame(
        Type = character(0),
        Round = integer(0),
        Number = integer(0),
        Question = character(0),
        Answer = character(0)
      )
    )
    
    # Reactive value to store the selected Round data
    selected_rounds <- shiny::reactiveVal(
      data.frame(
        Round = integer(0),
        N_binary = integer(0),
        N_range = integer(0)
      )
    )
    
    selected_round_choices <- reactiveVal(NULL)
    selected_round_active <- reactiveVal(NULL)
    
    trigger_refresh <- reactiveVal(0)
    
    
    ## Observe Language and Column Selection ---------------------------------
    shiny::observeEvent({
      input$language
      input$show_columns
    },
    {
      
      selected_language <- input$language
      show_columns <- input$show_columns
      
      binary_round <- if (!is.null(input$binary_table)) {
        rhandsontable::hot_to_r(input$binary_table) |> pull(Round)
      } else {
        NA_integer_
      }
      
      range_round <- if (!is.null(input$range_table)) {
        rhandsontable::hot_to_r(input$range_table) |> pull(Round)
      } else {
        NA_integer_
      }
      
      binary_data(
        questions_full$binary |>
          dplyr::mutate(Round = !!binary_round) |> 
          dplyr::select(Round, Number, Question = !!selected_language, !!show_columns)
      )
      
      range_data(
        questions_full$range |>
          dplyr::mutate(Round = !!range_round) |> 
          dplyr::select(Round, Number, Question = !!selected_language, !!show_columns)
        
      )
    })
    
    ## Selected Questions ----------------------------------------------------
    
    shiny::observeEvent(input$binary_table, {
      # Convert input and tag as Binary
      binary <- rhandsontable::hot_to_r(input$binary_table) |>
        dplyr::mutate(Type = "Binary")
      
      # Tag existing range data
      range <- range_data() |>
        dplyr::mutate(Type = "Range")
      
      # Combine, rank, and split back
      combined <- dplyr::bind_rows(binary, range) |>
        dplyr::mutate(Round = dplyr::dense_rank(Round))
      
      # Update reactive values
      binary_data(combined |> dplyr::filter(Type == "Binary") |> dplyr::select(-Type))
      range_data(combined |> dplyr::filter(Type == "Range") |> dplyr::select(-Type))
      
      # Update selected questions
      selected_questions(combined |> dplyr::filter(!is.na(Round)))
    })
    
    shiny::observeEvent(input$range_table, {
      # Convert input and tag as Binary
      range <- rhandsontable::hot_to_r(input$range_table) |>
        dplyr::mutate(Type = "Range")
      
      # Tag existing range data
      binary <- binary_data() |>
        dplyr::mutate(Type = "Binary")
      
      # Combine, rank, and split back
      combined <- dplyr::bind_rows(binary, range) |>
        dplyr::mutate(Round = dplyr::dense_rank(Round))
      
      # Update reactive values
      binary_data(combined |> dplyr::filter(Type == "Binary") |> dplyr::select(-Type))
      range_data(combined |> dplyr::filter(Type == "Range") |> dplyr::select(-Type))
      
    })
    
    observeEvent({
      binary_data()
      range_data()
    }, {
      binary <- binary_data() |>
        dplyr::mutate(Type = "Binary")
      
      range <- range_data() |>
        dplyr::mutate(Type = "Range")
      
      # Update selected questions
      selected_questions(
        dplyr::bind_rows(binary, range) |>
          dplyr::mutate(Round = dplyr::dense_rank(Round)) |>
          dplyr::filter(!is.na(Round))
      )
    })
    
    ## Load Question Set -------------------------------------------------------
    observeEvent(input$load_question_selection, {
      available_sets <- load_question_sets()$question_set_name
      
      showModal(modalDialog(
        title = "Load Question Set",
        shiny::selectInput(
          inputId = ns("selected_set_to_load"),
          label = "Choose a Question Set",
          choices = available_sets
        ),
        footer = tagList(
          actionButton(ns("confirm_load_set"), "Load", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    observeEvent(input$confirm_load_set, {
      removeModal()
      
      selected_language <- input$language
      show_columns <- input$show_columns
      selected_name <- input$selected_set_to_load
      all_sets <- load_question_sets()
      
      selected_row <- all_sets |>
        dplyr::filter(question_set_name == selected_name)
      
      selected_code <- selected_row$encrypted_question_set_code
      key <- our_key
      nonce <- our_nonce
      decoded_data <- decrypt_question_index(selected_code, key, nonce)
      
      # Extract round info
      binary_rounds <- decoded_data |>
        dplyr::filter(Type == "Binary") |>
        dplyr::select(Number, Round)
      
      range_rounds <- decoded_data |>
        dplyr::filter(Type == "Range") |>
        dplyr::select(Number, Round)
      
      # Update reactiveVals
      binary_data(
        questions_full$binary |>
          dplyr::mutate(Round = NA_integer_) |>
          dplyr::left_join(binary_rounds, by = "Number", suffix = c("", ".loaded")) |>
          dplyr::mutate(Round = coalesce(Round.loaded, Round)) |>
          dplyr::select(Round, Number, Question = !!selected_language, !!show_columns)
      )
      
      range_data(
        questions_full$range |>
          dplyr::mutate(Round = NA_integer_) |>
          dplyr::left_join(range_rounds, by = "Number", suffix = c("", ".loaded")) |>
          dplyr::mutate(Round = coalesce(Round.loaded, Round)) |>
          dplyr::select(Round, Number, Question = !!selected_language, !!show_columns)
      )
      
      # Update question set name input
      updateTextInput(session, "question_set_name", value = selected_name)
      
      
      # Extract active rounds
      defined_rounds <- selected_row |>
        dplyr::select(dplyr::starts_with("round_")) |>
        purrr::discard(~ is.na(.x))
      
      round_choices <- defined_rounds |> 
        names() |>
        stringr::str_remove("^round_") |>
        as.integer() |>
        sort()
      
      active_rounds <- 
        defined_rounds |> 
        purrr::keep(~ .x == 1) |> 
        names() |>
        stringr::str_remove("^round_") |>
        as.integer() |>
        sort()
      
      selected_round_choices(round_choices)
      selected_round_active(active_rounds)
      
      # Update help video checkbox
      help_active <- if (selected_row$help_videos_active == 1) "show_help" else NULL
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "help_videos_active",
        choiceNames = "Show",
        choiceValues = "show_help",
        selected = help_active,
        status = "warning"
      )
    })
    
    ## Reset Question Selection ----------------------------------------------
    observeEvent(input$reset_question_selection, {
      selected_language <- input$language
      show_columns <- input$show_columns
      
      binary_data(
        questions_full$binary |>
          dplyr::mutate(Round = NA_integer_) |>
          dplyr::select(Round, Number, Question = !!selected_language, !!show_columns)
      )
      
      range_data(
        questions_full$range |>
          dplyr::mutate(Round = NA_integer_) |>
          dplyr::select(Round, Number, Question = !!selected_language, !!show_columns)
      )
    })
    
    
    ## Selected Rounds -------------------------------------------------------
    
    # Observe changes in the selected questions and update the reactive value
    shiny::observeEvent(selected_questions(), {
      selected_rounds(
        selected_questions() |>
          dplyr::select(Round, Type) |>
          dplyr::group_by(Round, Type) |>
          dplyr::summarise(N_Round = dplyr::n()) |>
          tidyr::pivot_wider(
            names_from = Type,
            names_prefix = "N_",
            values_from = "N_Round"
          ) |>
          dplyr::select(dplyr::any_of(c(
            "Round", "N_Binary", "N_Range"
          )))
      )
    })
    
    
    shiny::observeEvent(input$checks_for_question_set,{
      selected_round_active(input$checks_for_question_set)
    })
    
    observeEvent(selected_rounds(),{
      
      shinyWidgets::updateCheckboxGroupButtons(
        inputId = "checks_for_question_set",
        choiceNames = c(selected_rounds() |> pull(Round) |> sort()),
        choiceValues = c(selected_rounds() |> pull(Round) |> sort()),
        selected = selected_round_active(),
        status = "warning"
      )
    })
    
    observeEvent(selected_round_active(), {
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "checks_for_question_set",
        choiceNames = selected_round_choices(),
        choiceValues = selected_round_choices(),
        selected = selected_round_active(),
        status = "warning"
      )
    })
    
    ## Encrypted Question Set Code -------------------------------------------
    
    # Reactive expression to generate the encrypted question set code
    encrypted_question_set_code <- shiny::reactive({
      req(selected_questions())
      key <- our_key
      nonce <- our_nonce
      selected_questions() |> 
        dplyr::select(Type, Round, Number) |> 
        encrypt_question_index(key, nonce)
    })
    
    
    ## Save Question Set -----------------------------------------------------
    shiny::observeEvent(input$save_question_set, {
      question_set_name <- input$question_set_name
      
      
      # Check if the question set name is at least 5 characters long
      if (nchar(question_set_name) < 5) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Error",
            "Question set name must be at least 5 characters long. Please use a different name.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        req(FALSE)
      }
      
      # Check for empty selected_rounds()
      if (nrow(selected_rounds()) == 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Error",
            "No rounds selected. Please select at least one round.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        req(FALSE)
      }
      
      # Check if the Round values of the selected_rounds() do not exceed a maximum of 10
      if (any(selected_rounds()$Round > 10)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Error",
            "Round values must not exceed a maximum of 10. Please adjust the rounds.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        req(FALSE)
      }
      
      question_sets <- load_question_sets()
      
      # Check if the question set name is already used
      if (question_set_name %in% question_sets$question_set_name) {
        showModal(
          modalDialog(
            title = "Confirm Overwrite",
            "Question set name already used. Do you want to overwrite the set?",
            footer = tagList(
              actionButton(ns("confirm_overwrite"), "Confirm", class = "btn-danger"),
              modalButton("Cancel")
            )
          )
        )
        
        req(FALSE)
      }
      
      # Function to determine round value
      determine_round_value <- function(round_number) {
        if (round_number %in% selected_round_active()) {
          TRUE
        } else if (round_number %in% selected_rounds()$Round) {
          FALSE
        } else {
          NA
        }
      }
      
      # Insert the new question set into the database
      DBI::dbExecute(pool, "
  INSERT INTO question_sets (
    question_set_name,
    encrypted_question_set_code,
    help_videos_active,
    round_1, round_2, round_3, round_4, round_5,
    round_6, round_7, round_8, round_9, round_10,
    created, created_by
  ) VALUES (
    ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
  )
", params = list(
  question_set_name,
  encrypted_question_set_code(),
  "show_help" %in% input$help_videos_active,
  determine_round_value(1),
  determine_round_value(2),
  determine_round_value(3),
  determine_round_value(4),
  determine_round_value(5),
  determine_round_value(6),
  determine_round_value(7),
  determine_round_value(8),
  determine_round_value(9),
  determine_round_value(10),
  Sys.time(),
  get_posit_user()
))
      
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Success",
          "Question set saved successfully.",
          easyClose = TRUE,
          footer = NULL
        )
      )
      
      trigger_refresh(trigger_refresh() + 1)
    })
    
    observeEvent(input$confirm_overwrite, {
      
      question_set_name <- input$question_set_name
      
      # Function to determine round value
      determine_round_value <- function(round_number) {
        if (round_number %in% selected_round_active()) {
          TRUE
        } else if (round_number %in% selected_rounds()$Round) {
          FALSE
        } else {
          NA
        }
      }
      
      # Update the question set in the database
      DBI::dbExecute(pool, "
    UPDATE question_sets SET
      encrypted_question_set_code = ?,
      help_videos_active = ?,
      round_1 = ?, round_2 = ?, round_3 = ?, round_4 = ?, round_5 = ?,
      round_6 = ?, round_7 = ?, round_8 = ?, round_9 = ?, round_10 = ?,
      created = ?, created_by = ?
    WHERE question_set_name = ?
  ", params = list(
    encrypted_question_set_code(),
    "show_help" %in% input$help_videos_active,
    determine_round_value(1),
    determine_round_value(2),
    determine_round_value(3),
    determine_round_value(4),
    determine_round_value(5),
    determine_round_value(6),
    determine_round_value(7),
    determine_round_value(8),
    determine_round_value(9),
    determine_round_value(10),
    Sys.time(),
    get_posit_user(),
    question_set_name
  ))
      
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Success",
          "Question set saved successfully.",
          easyClose = TRUE,
          footer = NULL
        )
      )
      
      trigger_refresh(trigger_refresh() + 1)
    })
    
   
    ## Render Tables ---------------------------------------------------------
    
    # Render the binary table
    output$binary_table <- rhandsontable::renderRHandsontable({
      
      req(input$binary_table_height)
      
      rhandsontable(
        binary_data(),
        readOnly = TRUE,
        contextMenu = FALSE,
        rowHeaders = NULL,
        height = input$binary_table_height, 
        stretchH = "all"
      ) |>
        rhandsontable::hot_col("Number", format = "0") |> 
        rhandsontable::hot_col("Round", readOnly = FALSE) |>
        hot_cols(columnSorting = TRUE, colWidths = c(30, 30, 200, 30, 100, 100))
    })
    
    # Render the range table
    output$range_table <- rhandsontable::renderRHandsontable({
      
      req(input$range_table_height)
      
      rhandsontable::rhandsontable(
        range_data(), 
        readOnly = TRUE,
        contextMenu = FALSE,
        rowHeaders = NULL,
        height = input$range_table_height,
        stretchH = "all"
      ) |>
        rhandsontable::hot_col("Number", format = "0") |> 
        rhandsontable::hot_col("Round", readOnly = FALSE) |>
        hot_cols(columnSorting = TRUE, colWidths = c(30, 30, 200, 30, 100, 100))
    })
    
    
    
    # Render the selected questions table
    output$selected_questions_table <- rhandsontable::renderRHandsontable({
      req(selected_questions())
      
      rhandsontable(
        selected_questions(),
        readOnly = TRUE,
        contextMenu = FALSE,
        rowHeaders = NULL,
        height = input$selected_questions_table_height, 
        stretchH = "all"
      ) |>
        hot_cols(columnSorting = TRUE, colWidths = c(30, 30, 200, 30, 100, 100))
    })
    
    # Render the selected rounds table
    output$selected_rounds_table <- rhandsontable::renderRHandsontable({
      req(selected_rounds())
      
      rhandsontable(
        selected_rounds(), 
        readOnly = TRUE,
        contextMenu = FALSE,
        rowHeaders = NULL,
        height = input$selected_rounds_table_height, 
        stretchH = "all"
      ) |> 
        hot_cols(columnSorting = TRUE)
    })
    
    
    return(
      list(trigger_refresh = trigger_refresh)
    )
  })
}