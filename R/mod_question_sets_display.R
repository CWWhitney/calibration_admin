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
mod_question_sets_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("question_sets_table")),
    rhandsontable::rHandsontableOutput(ns("decoded_questions_table"))
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
mod_question_sets_display_server <- function(id, question_sets) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Reactive value to store the selected question set code
      selected_question_set_code <- shiny::reactiveVal()
      
      
      ## Render Tables --------------------------------------------------------
      
      # Create a proxy for the DataTable
      proxy <- DT::dataTableProxy("question_sets_table")
      
      # Render the question sets table
      output$question_sets_table <- DT::renderDataTable({
        DT::datatable(
          question_sets(),
          selection = "none"
        )
      })
      
      ## Observe Events -------------------------------------------------------
      
      # Observe button clicks in the question sets table
      shiny::observeEvent(input$question_sets_table_cell_clicked, {
        info <- input$question_sets_table_cell_clicked
        if (!is.null(info$value) && info$col == 2) { 
          selected_question_set_code(info$value)
        }
        if (!is.null(info$value) && info$col > 2) { 
          question_sets_cur <- question_sets()
          question_sets_cur[info$row, info$col] <- !info$value
          question_sets(question_sets_cur)
          
        }
      })
      
      # Render the decoded question set table
      output$decoded_questions_table <- rhandsontable::renderRHandsontable({
        req(selected_question_set_code())
        key <- our_key
        nonce <- our_nonce
        decoded_data <- decrypt_question_index(selected_question_set_code(), key, nonce)
        rhandsontable::rhandsontable(decoded_data, readOnly = TRUE) |>
          hot_cols(columnSorting = TRUE)
      })
    }
  )
}