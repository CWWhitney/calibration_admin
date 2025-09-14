#' Usera Table Display UI
#'
#' A shiny module UI that displays a filterable table of user data with delete functionality.
#'
#' @param id The namespace id of the module.
#' @param tab_title Title for the tab panel.
#'
#' @export
mod_users_table_ui <- function(id, tab_title) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = tab_title,
    bslib::navset_card_tab(
      title = bslib::card_title(
        tab_title,
        actionButton(
          ns("reload"), label = bsicons::bs_icon("arrow-clockwise"),
          class = "btn-warning"
        )
      ),
      bslib::card_body(
        DT::dataTableOutput(ns("users_table"))
      )
    )
  )
}


#' Usera Table Display Server
#'
#' A shiny module server function to display, refresh, and delete entries from a user table.
#'
#' @param id The namespace id of the module.
#' @param loading_function A function that loads the user data.
#'
#' @export
mod_users_table_server <- function(id, loading_function, deleting_function) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    users_data <- reactiveVal(loading_function())
    selected_row_to_delete <- reactiveVal()
    
    # Refresh logic
    observe({
      input$reload
      invalidateLater(60000, session)
      users_data(loading_function())
    })
    
    output$users_table <- DT::renderDataTable({
      data <- users_data()
      
      # Add delete button column
      data$delete_button <- sprintf(
        '<button class="btn btn-danger delete-btn" data-row="%s">Delete</button>',
        seq_len(nrow(data))
      )
      
      DT::datatable(
        data,
        escape = FALSE,
        selection = "none",
        options = list(dom = 't')
      )
    })
    
    # Handle delete button click
    observeEvent(input$users_table_cell_clicked, {
      info <- input$users_table_cell_clicked
      data <- users_data()
      
      if (!is.null(info$value) && grepl("delete-btn", info$value)) {
        showModal(
          modalDialog(
            title = "Confirm Deletion",
            "Are you sure you want to delete all of this users data,
            including the responses that were given by them?",
            footer = tagList(
              actionButton(ns("confirm_delete"), "Delete", class = "btn-danger"),
              modalButton("Cancel")
            )
          )
        )
        selected_row_to_delete(info$row)
        req(FALSE)
      }
    })
    
    observeEvent(input$confirm_delete, {
      removeModal()
      
      data <- users_data()
      row <- selected_row_to_delete()
      user <- data[row, ]  # Adjust this to match your unique identifier column
      
      deleting_function(user_first_name = user$user_first_name, user_last_name = user$user_last_name, user_session = user$user_session)
      
      users_data(loading_function())
    })
  })
}
