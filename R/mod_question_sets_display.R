##******************************************************************************
##*
##* This is the question_sets_display shiny module which displays a table of available question sets.
##*
##******************************************************************************

#' Question Sets Display UI
#'
#' A shiny module, which displays a table of available question sets.
#'
#' @param id The namespace id of the module.
#'
#' @return An HTML element for use in a UI.
#'
#' @seealso [mod_question_sets_display_server()]
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'    shiny::shinyApp(
#'      ui = fluidPage(
#'        mod_question_sets_display_ui(
#'          id = "mod_question_sets_display"
#'        )
#'      ),
#'      server = function(input, output, session) {
#'        mod_question_sets_display_server(
#'          id ="mod_question_sets_display"
#'        )
#'      }
#'    )
#'  
#'  }
mod_question_sets_display_ui <- function(id, tab_title) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = tab_title,
    bslib::navset_card_tab(
      title = bslib::card_title(
        "Question Set Display", 
        actionButton(
          ns("reload"), label = bsicons::bs_icon("arrow-clockwise"),
          class = "btn-warning"
        )
      ),
      bslib::card_body(
        DT::dataTableOutput(ns("question_sets_table"))
      )
    )
  )
}




#' Question Sets Display Server
#'
#' A shiny module server function, which handles the logic for displaying and decoding question sets.
#'
#' @param id The namespace id of the module.
#'
#' @return `list` of `shiny::reactive({})`s
#'
#' @seealso [mod_question_sets_display_ui()]
#'
#' @export
#'
#' @inherit mod_question_sets_display_ui title description details examples
mod_question_sets_display_server <- function(id, trigger_refresh) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_question_set_code <- shiny::reactiveVal()
    selected_row_to_delete <- shiny::reactiveVal()
    question_sets <- reactiveVal(load_question_sets())
    
    # Refresh button
    observe({
      input$reload
      trigger_refresh()
      invalidateLater(60000, session)
      
      question_sets(load_question_sets())
    })
    
    # Create a proxy for the DataTable
    proxy <- DT::dataTableProxy("question_sets_table")
    
    output$question_sets_table <- DT::renderDataTable({
      data <- question_sets()
      round_cols <- grep("^round_", names(data), value = TRUE) |> 
        c("help_videos_active")
      
      # Convert round columns to HTML buttons
      for (col in round_cols) {
        data[[col]] <- ifelse(
          data[[col]],
          sprintf('<div style="text-align:center;"><button class="btn btn-default action-button btn-secondary" data-col="%s" data-value="1">✔</button></div>', col),
          sprintf('<div style="text-align:center;"><button class="btn btn-default action-button btn-outline-secondary" data-col="%s" data-value="0">✖</button></div>', col)
        )
      }
      
      # Wrap encrypted code column
      data$encrypted_question_set_code <- 
        glue::glue(
          '<div style="max-width:200px; white-space:normal; word-wrap:break-word;" data-value="{code}">{code}</div>',
          code = data$encrypted_question_set_code
        )
      
      
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
    
    
    
    
    # Observe cell clicks
    observeEvent(input$question_sets_table_cell_clicked, {
      info <- input$question_sets_table_cell_clicked
      data <- question_sets()
      
      if (!is.null(info$value) && grepl("delete-btn", info$value)) {
        showModal(
          modalDialog(
            title = "Confirm Deletion",
            "Are you sure you want to delete this question set?",
            footer = tagList(
              actionButton(ns("confirm_delete"), "Delete", class = "btn-danger"),
              modalButton("Cancel")
            )
          )
        )
        
        # Store the row index to delete
        selected_row_to_delete(info$row)
        req(FALSE)
      }
      
      
      # Handle button toggle
      col_name <- names(data)[info$col]
      
      if (!is.null(info$value) && stringr::str_detect(col_name, "(^round_[0-9]+$)|(^help_videos_active$)")) {
        # Extract current value from the HTML
        current_value <- if (grepl('data-value="1"', info$value)) TRUE else FALSE
        new_value <- !current_value
        
        data[info$row, col_name] <- new_value
        question_sets(data)
        
        # Update DB
        DBI::dbExecute(pool, sprintf(
          "UPDATE question_sets SET %s = ? WHERE question_set_name = ?",
          col_name
        ), params = list(as.integer(new_value), data$question_set_name[info$row]))
        
        # Reload to reflect changes
        question_sets(load_question_sets())
      }
    })
    
    observeEvent(input$confirm_delete, {
      removeModal()
      
      data <- question_sets()
      row <- selected_row_to_delete()
      question_set_name <- data$question_set_name[row]
      
      DBI::dbExecute(pool, "DELETE FROM question_sets WHERE question_set_name = ?", 
                     params = list(question_set_name))
      
      question_sets(load_question_sets())
    })
    
    
    
  })
}