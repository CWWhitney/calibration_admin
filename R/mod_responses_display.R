#' Binary Responses Display UI
#'
#' A shiny module UI that displays a filterable table of binary responses.
#'
#' @param id The namespace id of the module.
#' @param tab_title Title for the tab panel.
#'
#' @export
mod_responses_display_ui <- function(id, tab_title) {
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
        DT::dataTableOutput(ns("responses_table"))
      )
    )
  )
}


#' Binary Responses Display Server
#'
#' A shiny module server function to display and refresh binary responses.
#'
#' @param id The namespace id of the module.
#' @param trigger_refresh A reactive trigger to refresh the data.
#'
#' @export
mod_responses_display_server <- function(id, loading_function) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    responses <- reactiveVal(loading_function())
    
    # Refresh logic
    observe({
      input$reload
      invalidateLater(60000, session)
      responses(loading_function())
    })
    
    output$responses_table <- DT::renderDataTable({
      DT::datatable(
        responses(),
        filter = "top",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE
      )
    })
  })
}
