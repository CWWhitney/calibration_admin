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
      bslib::nav_panel(
        title = "Table",
        bslib::card_body(
          DT::dataTableOutput(ns("responses_table"))
        )
      ),
      bslib::nav_panel(
        title = "Graph",
        bslib::card_body(
          shinyWidgets::pickerInput(
            ns("workshop_set"),
            label = "Workshop Set",
            choices = c(),
            multiple = TRUE
          ),
          echarts4r::echarts4rOutput(outputId = ns("chart"))
        )
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
mod_responses_display_server <- function(id, loading_function, binary_or_range) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    responses <- reactiveVal(loading_function())
    
    # Refresh logic
    observe({
      input$reload
      invalidateLater(60000, session)
      responses(loading_function())
    })
    
    observe({
      req(responses()$workshop_set)
      
      shinyWidgets::updatePickerInput(
        inputId = "workshop_set",
        choices = responses()$workshop_set |> 
          unique() |> 
          sort()
      )
    })
    
    output$responses_table <- DT::renderDataTable({
      DT::datatable(
        responses(),
        filter = "top",
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = "csv",
              exportOptions = list(
                modifier = list(
                  search = "applied"
                )
              )
            )
          )
        ),
        rownames = FALSE
      )
    }, server = FALSE)
    
    output$chart <- echarts4r::renderEcharts4r({
      
      # Require that the "binary" data has been retrieved from the {pins} board
      shiny::req(responses())
      
      
      if(binary_or_range == "binary") {
        
        individual <- responses() |> 
          dplyr::filter(workshop_set %in% input$workshop_set) |> 
          dplyr::mutate(Correct = ifelse(
            stringr::str_sub(response, start = 1L, end = 1L) == truth, TRUE, FALSE
          )) |>
          dplyr::group_by(user_session, round_number, workshop_set) |>
          dplyr::summarise(
            Actual = sum(Correct), 
            Predicted = confidence |> stringr::str_replace("%", "") |> as.numeric() |> sum() / 100, 
            Total = dplyr::n(), 
            .groups = "drop"
          )
        
        group <- individual |>
          dplyr::group_by(round_number) |>
          dplyr::summarise(
            Group_Pct_Actual = sum(Actual) / (dplyr::n() * mean(Total)), 
            Group_Pct_Predicted = sum(Predicted) / (dplyr::n() * mean(Total)), 
            Adjustment_Needed = Group_Pct_Actual - Group_Pct_Predicted, 
            .groups = "drop"
          )
        
        graph_list <- list(
          individual = individual, 
          group = group
        )
        
        # Create an interactive chart containing the group "binary" aggregated data
        graph <- graph_list |>
          purrr::pluck("group") |>
          dplyr::mutate(
            round_number = paste0("Round ", round_number)
          ) |>tidyr::drop_na() |> ### TODO // remove
          echarts4r::e_charts(round_number) |>
          echarts4r::e_bar(Group_Pct_Actual, name = "Actual % Correct") |>
          echarts4r::e_line(
            Group_Pct_Predicted, 
            name = "Predicted % Correct", 
            symbol = "circle", 
            symbolSize = 20
          ) |>
          echarts4r::e_y_axis(
            formatter = echarts4r::e_axis_formatter(
              style = "percent", 
              digits = 0
            )
          ) |>
          echarts4r::e_color(background = "white") |>
          echarts4r::e_tooltip(
            trigger = "axis", 
            formatter = echarts4r::e_tooltip_pointer_formatter(
              style = "percent", 
              digits = 1
            )
          ) |>
          echarts4r::e_toolbox_feature(feature = "saveAsImage")
        
      }
      
      if(binary_or_range == "range") {
        
        calculate_range_adjustment <- function(group_pct) {
          
          inv_norm <- qnorm(
            group_pct / 2 + 0.5, 
            mean = 0, 
            sd = 1
          )
          
          (3.29 / inv_norm) / 2
          
        }
        
        individual <- responses() |> 
          dplyr::filter(workshop_set %in% input$workshop_set) |> 
          dplyr::mutate(Bounded = dplyr::case_when(
            truth >= lower_90 & truth <= upper_90 ~ TRUE, 
            TRUE ~ FALSE
          )) |>
          dplyr::group_by(user_session, round_number, workshop_set) |>
          dplyr::summarise(
            Bounded = sum(Bounded), 
            Total = dplyr::n(), 
            .groups = "drop"
          )
        
        
        group <- individual |>
          dplyr::group_by(round_number) |>
          dplyr::summarise(
            Group_Pct = sum(Bounded) / (dplyr::n() * mean(Total)), 
            Adjustment_Needed = calculate_range_adjustment(group_pct = Group_Pct), 
            .groups = "drop"
          )
        
        graph_list <- list(
          individual = individual, 
          group = group
        )
        
        # Create an interactive chart containing the group "binary" aggregated data
        graph <- graph_list |>
          purrr::pluck("group") |>
          dplyr::mutate(
            Group = paste0("Round ", round_number), 
            Target = 0.90
          ) |>tidyr::drop_na() |> ### TODO // remove
          echarts4r::e_charts(Group) |>
          echarts4r::e_bar(Group_Pct, name = "Actual % Correct") |>
          echarts4r::e_line(
            Target, 
            name = "Target % Correct", 
            symbol = "circle", 
            symbolSize = 20
          ) |>
          echarts4r::e_y_axis(
            formatter = echarts4r::e_axis_formatter(
              style = "percent", 
              digits = 0
            )
          ) |>
          echarts4r::e_color(background = "white") |>
          echarts4r::e_tooltip(
            trigger = "axis", 
            formatter = echarts4r::e_tooltip_pointer_formatter(
              style = "percent", 
              digits = 1
            )
          ) |>
          echarts4r::e_toolbox_feature(feature = "saveAsImage")
        
      }
      
      graph
    })
  })
}
