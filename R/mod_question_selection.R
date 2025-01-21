# Define the UI for the module
mod_question_selection_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectInput(ns("language"), "Select Language", choices = languages)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3, 
        shiny::textInput(
          ns("question_set_name"), 
          "Select Question Set Name"
        ),
        rhandsontable::rHandsontableOutput(ns("selected_groups_table")),
        shiny::textOutput(ns("question_set_code")),
        shiny::fluidRow(
          shiny::actionButton(
            class = "btn btn-success", 
            inputId = ns("save_question_set"), 
            label = "Save Question Set", 
            icon = shiny::icon("floppy-disk"), 
            width = "200px"
          ),
          shiny::actionButton(
            class = "btn btn-success", 
            inputId = ns("reset_question_selection"), 
            label = "Reset Question Selection", 
            icon = shiny::icon("arrow-rotate-left"), 
            width = "200px"
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12, 
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Selected Questions",
            rhandsontable::rHandsontableOutput(ns("selected_questions_table"))
          ),
          shiny::tabPanel(
            title = "Binary", 
            shinyWidgets::pickerInput(
              ns("binary_columns"),
              label = "Show Columns",
              multiple = TRUE,
              selected = "Answer",
              choices = c("Answer", "Source_link", "Comments"
                          # ,
                          # "worldwide_use", 
                          # "europe_use", "germany_use", "kenya_use", "vietnam_use", "costarica_use", 
                          # "note_please_ignore"
              )
            ),
            rhandsontable::rHandsontableOutput(ns("binary_table"))
          ), 
          shiny::tabPanel(
            title = "Range", 
            shinyWidgets::pickerInput(
              ns("range_columns"),
              label = "Show Columns",
              multiple = TRUE,
              selected = "Answer",
              choices = c("Answer", "Source_link", "Comments"
                          # ,
                          # "worldwide_use", 
                          # "europe_use", "germany_use", "kenya_use", "vietnam_use", "costarica_use", 
                          # "note_please_ignore", "old_number"
              )
            ),
            rhandsontable::rHandsontableOutput(ns("range_table"))
          )
        )
      )
    )
  )
}

# Define the server logic for the module
mod_question_selection_server <- function(id, questions_full, question_sets) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store the table data
    binary_data <- shiny::reactiveVal(
      questions_full$binary |>
        dplyr::mutate(Number = as.integer(Number)) |> 
        dplyr::mutate(Group = NA_integer_)
    )
    
    range_data <- shiny::reactiveVal(
      questions_full$range |>
        dplyr::mutate(Number = as.integer(Number)) |> 
        dplyr::mutate(Group = NA_integer_)
    )
    
    observeEvent(input$reset_question_selection, {
      selected_language <- input$language
      
      binary_columns <- input$binary_columns
      
      range_columns <- input$range_columns
      
      binary_data(
        questions_full$binary |>
          dplyr::select(Number, !!selected_language, !!binary_columns) |>
          dplyr::mutate(Group = NA_integer_)
      )
      
      range_data(
        questions_full$range |>
          dplyr::select(Number, !!selected_language, !!range_columns) |>
          dplyr::mutate(Group = NA_integer_)
      )
    })
    
    
    # Observe changes in the language selection
    shiny::observeEvent({
      input$language
      input$binary_columns
      input$range_columns
    },
    {
      selected_language <- input$language
      
      binary_columns <- input$binary_columns
      
      range_columns <- input$range_columns
      
      binary_group <- if (!is.null(input$binary_table)) {
        rhandsontable::hot_to_r(input$binary_table) |> pull(Group)
      } else {
        NA_integer_
      }
      
      range_group <- if (!is.null(input$range_table)) {
        rhandsontable::hot_to_r(input$range_table) |> pull(Group)
      } else {
        NA_integer_
      }
      
      binary_data(
        questions_full$binary |>
          dplyr::select(Number, !!selected_language, !!binary_columns) |>
          dplyr::mutate(Group = !!binary_group)
      )
      
      range_data(
        questions_full$range |>
          dplyr::select(Number, !!selected_language, !!range_columns) |>
          dplyr::mutate(Group = !!range_group)
      )
    })
    
    # Render the binary table
    output$binary_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(binary_data(), readOnly = TRUE) |>
        rhandsontable::hot_col("Group", readOnly = FALSE) |>
        hot_cols(columnSorting = TRUE)
    })
    
    # Render the range table
    output$range_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(range_data(), readOnly = TRUE) |>
        rhandsontable::hot_col("Group", readOnly = FALSE) |>
        hot_cols(columnSorting = TRUE)
    })
    
    # Reactive value to store the selected questions data
    selected_questions <- shiny::reactiveVal(
      data.frame(
        Type = character(0),
        Group = integer(0),
        Number = double(0)
      )
    )
    
    # Observe changes in the binary table and update the reactive value
    shiny::observeEvent(input$binary_table, {
      
      binary_data(
        rhandsontable::hot_to_r(input$binary_table) |>
          dplyr::mutate(Type = "Binary") |> 
          dplyr::bind_rows(
            range_data() |>
              select(Number, Group) |> 
              dplyr::mutate(Type = "Range")
          ) |> 
          mutate(Group = dense_rank(Group))|>
          dplyr::filter(Type == "Binary") |> 
          dplyr::select(-Type)
      )
      
      range_data(
        range_data() |> 
          dplyr::mutate(Type = "Range") |> 
          dplyr::bind_rows(
            binary_data() |>
              select(Number, Group) |> 
              dplyr::mutate(Type = "Binary")
          ) |> 
          mutate(Group = dense_rank(Group))|>
          dplyr::filter(Type == "Range") |> 
          dplyr::select(-Type)
      )
      
      binary_selected <- binary_data() |>
        dplyr::select(Group, Number) |>
        dplyr::filter(!is.na(Group)) |>
        dplyr::mutate(Type = "Binary")
      
      range_selected <- range_data() |>
        dplyr::select(Group, Number) |>
        dplyr::filter(!is.na(Group)) |>
        dplyr::mutate(Type = "Range")
      
      
      selected_questions(
        dplyr::bind_rows(binary_selected, range_selected) |>
          dplyr::select(Type, Group, Number) |> 
          dplyr::group_by(Type, Group)
      )
    })
    
    
    
    # Observe changes in the range table and update the reactive value
    shiny::observeEvent(input$range_table, {
      
      range_data(
        rhandsontable::hot_to_r(input$range_table) |>
          dplyr::mutate(Type = "Range") |> 
          dplyr::bind_rows(
            binary_data() |>
              select(Number, Group) |> 
              dplyr::mutate(Type = "Binary")
            ) |> 
          mutate(Group = dense_rank(Group))|>
          dplyr::filter(Type == "Range") |> 
          dplyr::select(-Type)
      )
      
      binary_data(
        binary_data() |> 
          dplyr::mutate(Type = "Binary") |> 
          dplyr::bind_rows(
            range_data() |>
              select(Number, Group) |> 
              dplyr::mutate(Type = "Range")
          ) |> 
          mutate(Group = dense_rank(Group))|>
          dplyr::filter(Type == "Binary") |> 
          dplyr::select(-Type)
      )
      
      binary_selected <- binary_data() |>
        dplyr::select(Group, Number) |>
        dplyr::filter(!is.na(Group)) |>
        dplyr::mutate(Type = "Binary")
      
      range_selected <- range_data() |>
        dplyr::select(Group, Number) |>
        dplyr::filter(!is.na(Group)) |>
        dplyr::mutate(Type = "Range")
      
      
      selected_questions(
        dplyr::bind_rows(binary_selected, range_selected) |>
          dplyr::select(Type, Group, Number) |> 
          dplyr::group_by(Type, Group)
      )
    })
    
    # Render the selected questions table
    output$selected_questions_table <- rhandsontable::renderRHandsontable({
      req(selected_questions())
      rhandsontable::rhandsontable(selected_questions(), readOnly = TRUE) |>
        hot_cols(columnSorting = TRUE)
    })
    
    # Reactive value to store the selected group data
    selected_groups <- shiny::reactiveVal(
      data.frame(
        Group = integer(0),
        N_binary = integer(0),
        N_range = integer(0)
      )
    )
    
    # Observe changes in the selected questions and update the reactive value
    shiny::observeEvent(selected_questions(), {
      selected_groups(
        selected_questions() |> 
          dplyr::select(Group, Type) |> 
          dplyr::group_by(Group, Type) |> 
          dplyr::summarise(N_Group = dplyr::n()) |> 
          tidyr::pivot_wider(names_from = Type, names_prefix = "N_", values_from = "N_Group") |> 
          dplyr::select(dplyr::any_of(c("Group", "N_Binary", "N_Range")))
      )
    })
    
    # Render the selected groups table
    output$selected_groups_table <- rhandsontable::renderRHandsontable({
      req(selected_groups())
      rhandsontable::rhandsontable(selected_groups(), readOnly = TRUE) |>
        hot_cols(columnSorting = TRUE)
    })
    
    # Observe event for saving question set
    shiny::observeEvent(input$save_question_set, {
      question_set_name <- input$question_set_name
      
      # Check if the question set name is already used
      if (question_set_name %in% question_sets()$question_set_name) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Error",
            "Question set name already used. Please use a different name.",
            easyClose = TRUE,
            footer = NULL
          )
        )
      } else {
        # Add the new question set to the global data frame
        question_sets(
          rbind(
            question_sets(),
            data.frame(
              question_set_name = question_set_name,
              encrypted_question_set_code = encrypted_question_set_code(),
              stringsAsFactors = FALSE
            )
          )
        )
        shiny::showNotification("Question set saved successfully.", type = "message")
      }
    })
    
    # Reactive expression to generate the encrypted question set code
    encrypted_question_set_code <- shiny::reactive({
      req(selected_questions())
      key <- our_key
      nonce <- our_nonce
      encrypt_question_index(selected_questions(), key, nonce)
    })
    
    # Render the question set code
    output$question_set_code <- shiny::renderText({
      encrypted_question_set_code()
    })
    
  })
}