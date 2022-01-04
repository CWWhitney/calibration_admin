library(shiny)
library(shinydashboard)
library(dplyr)
library(reactable)
library(stringr)
library(purrr)
library(echarts4r)

source("global.R")

ui <- shinydashboard::dashboardPage(
  
  shinydashboard::dashboardHeader(
    title = "Calibrator: Admin"
  ),
  
  shinydashboard::dashboardSidebar(
    
    shiny::br(), 
    
    shiny::actionButton(
      class = "btn btn-success", 
      inputId = "refresh_btn", 
      label = "Refresh Data", 
      icon = shiny::icon("sync"), 
      width = "200px"
    ), 
    
    shiny::uiOutput(outputId = "select_student_picker"), 
    
    shiny::hr(), 
    
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text = "Binary", 
        icon = shiny::icon("check-circle"), 
        
        shinydashboard::menuSubItem(
          text = "Raw Data", 
          tabName = "binary_raw_screen", 
          icon = shiny::icon("table")
        ), 
        
        shinydashboard::menuSubItem(
          text = "Analysis", 
          tabName = "binary_analysis_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      ), 
      
      shinydashboard::menuItem(
        text = "Range", 
        icon = shiny::icon("sort-numeric-down"), 
        
        shinydashboard::menuSubItem(
          text = "Raw Data", 
          tabName = "range_raw_screen", 
          icon = shiny::icon("table")
        ), 
        
        shinydashboard::menuSubItem(
          text = "Analysis", 
          tabName = "range_analysis_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      )
    )
    
  ),
  
  shinydashboard::dashboardBody(
    
    shinydashboard::tabItems(
      
      shinydashboard::tabItem(
        tabName = "binary_raw_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            reactable::reactableOutput(outputId = "binary_raw_tbl")
          )
        )
      ), 
      
      shinydashboard::tabItem(
        tabName = "binary_analysis_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            shiny::tabsetPanel(
              
              shiny::tabPanel(
                title = "Individual", 
                shiny::br(), 
                reactable::reactableOutput(outputId = "individual_binary_tbl"), 
                
                shiny::br(), 
                
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_binary_individual", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
                
              ), 
              
              shiny::tabPanel(
                title = "Group", 
                shiny::br(), 
                reactable::reactableOutput(outputId = "group_binary_tbl"), 
                
                shiny::br(), 
                
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_binary_group", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
              )
              
            ), 
            
            shiny::hr(), 
            
            echarts4r::echarts4rOutput(outputId = "group_binary_chart")
            
          )
        )
      ), 
      
      shinydashboard::tabItem(
        tabName = "range_raw_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            reactable::reactableOutput(outputId = "range_raw_tbl")
          )
        )
      ), 
      
      shinydashboard::tabItem(
        tabName = "range_analysis_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            shiny::tabsetPanel(
              
              shiny::tabPanel(
                title = "Individual", 
                shiny::br(), 
                reactable::reactableOutput(outputId = "individual_range_tbl"), 
                
                shiny::br(), 
                
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_range_individual", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
                
              ), 
              
              shiny::tabPanel(
                title = "Group", 
                shiny::br(), 
                reactable::reactableOutput(outputId = "group_range_tbl"), 
                
                shiny::br(), 
                
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_range_group", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
              )
              
            ), 
            
            shiny::hr(), 
            
            echarts4r::echarts4rOutput(outputId = "group_range_chart")
            
          )
        )
      )
      
    )
    
  ), 
  
  skin = "green"
)



server <- function(input, output, session) {
  
  rctv <- shiny::reactiveValues()

  rctv$current_data <- get_current_data(board = board)

  shiny::modalDialog(
    title = "Welcome, Admin!", 
    shiny::HTML(
      glue::glue(
        "<iframe width='560' height='315'", 
        "src='https://www.youtube.com/embed/WE5gT_LlV24'", 
        "title='YouTube video player' frameborder='0' allow='accelerometer;", 
        "autoplay; clipboard-write; encrypted-media; gyroscope;", 
        "picture-in-picture' allowfullscreen></iframe>", 
        .sep = " "
      )
    ), 
    "This app contains the live results of each student in the current workshop."
  ) %>%
    shiny::showModal()
  
  # When the "Refresh Data" button is clicked, re-read the pinned data
  shiny::observeEvent(input$refresh_btn, {
    
    shiny::showNotification(
      ui = "Please Wait...", 
      closeButton = FALSE, 
      id = "wait_notification", 
      type = "warning"
    )
    
    Sys.sleep(1)
    
    rctv$current_data <- get_current_data(board = board)
    
    shiny::removeNotification(id = "wait_notification")
    
  })
  
  
  
  # Create a dynamic drop-down list of each student in the workshop
  output$select_student_picker <- shiny::renderUI({
    
    shiny::req(rctv$current_data)
    
    shiny::selectInput(
      inputId = "choose_student", 
      label = "Select a Student", 
      choices = c("All", unique(
        rctv$current_data$binary$user, 
        rctv$current_data$range$user
      )), 
      selected = "All"
    )
    
  })
  

  output$binary_raw_tbl <- reactable::renderReactable({

    shiny::req(rctv$current_data$binary)
    shiny::req(input$choose_student)

    data <- rctv$current_data$binary
    
    if (input$choose_student != "All") {
      
      data <- rctv$current_data$binary %>% 
        dplyr::filter(user == input$choose_student)
      
    }
    
    data %>%
      reactable::reactable(
        columns = list(
          Confidence = reactable::colDef(
            format = reactable::colFormat(percent = TRUE, digits = 0)
          ), 
          Truth = reactable::colDef(cell = function(value) {
            if (value == "T") "TRUE" else "FALSE"
          }), 
          Brier = reactable::colDef(
            format = reactable::colFormat(digits = 3)
          )
        )
      )

  })

  output$range_raw_tbl <- reactable::renderReactable({

    shiny::req(
      rctv$current_data$range, 
      input$choose_student
    )

    data <- rctv$current_data$range
    
    if (input$choose_student != "All") {
      
      data <- rctv$current_data$range %>% 
        dplyr::filter(user == input$choose_student)
      
    }
    
    data %>%
      reactable::reactable(
        columns = list(
          Lower90 = reactable::colDef(name = "Lower Bound"), 
          Upper90 = reactable::colDef(name = "Upper Bound"), 
          RelativeError = reactable::colDef(
            name = "Relative Error", 
            format = reactable::colFormat(digits = 2)
          )
        )
      )

  })
  
  output$individual_binary_tbl <- reactable::renderReactable({
    
    shiny::req(rctv$current_data$binary)
    
    rctv$current_data$binary %>% 
      aggregate_binary() %>% 
      purrr::pluck("individual") %>% 
      reactable::reactable(
        filterable = TRUE, 
        columns = list(
          Actual = reactable::colDef(filterable = FALSE), 
          Predicted = reactable::colDef(filterable = FALSE), 
          Total = reactable::colDef(filterable = FALSE)
        )
      )
    
  })
  
  output$individual_range_tbl <- reactable::renderReactable({
    
    shiny::req(rctv$current_data$range)
    
    rctv$current_data$range %>% 
      aggregate_range() %>% 
      purrr::pluck("individual") %>% 
      reactable::reactable(
        filterable = TRUE, 
        columns = list(
          Bounded = reactable::colDef(filterable = FALSE),
          Total = reactable::colDef(filterable = FALSE)
        )
      )
    
  })
  
  output$group_binary_tbl <- reactable::renderReactable({
    
    shiny::req(rctv$current_data$binary)
    
    rctv$current_data$binary %>% 
      aggregate_binary() %>% 
      purrr::pluck("group") %>% 
      reactable::reactable(
        filterable = TRUE, 
        columns = list(
          Group_Pct_Actual = reactable::colDef(
            name = "Actual % Correct", 
            format = reactable::colFormat(percent = TRUE, digits = 2)
          ), 
          Group_Pct_Predicted = reactable::colDef(
            name = "Predicted % Correct", 
            format = reactable::colFormat(percent = TRUE, digits = 2)
          ), 
          Adjustment_Needed = reactable::colDef(
            name = "Adjustment Needed", 
            format = reactable::colFormat(percent = TRUE, digits = 2)
          )
        )
      )
    
  })
  
  output$group_range_tbl <- reactable::renderReactable({
    
    shiny::req(rctv$current_data$range)
    
    rctv$current_data$range %>% 
      aggregate_range() %>% 
      purrr::pluck("group") %>% 
      reactable::reactable(
        filterable = TRUE, 
        columns = list(
          Group_Pct = reactable::colDef(
            name = "Actual % Correct", 
            format = reactable::colFormat(percent = TRUE, digits = 2)
          ), 
          Adjustment_Needed = reactable::colDef(
            name = "Adjustment Needed", 
            format = reactable::colFormat(digits = 2)
          )
        )
      )
    
  })
  
  
  output$group_binary_chart <- echarts4r::renderEcharts4r({
    
    shiny::req(rctv$current_data$binary)
    
    rctv$current_data$binary %>% 
      aggregate_binary() %>% 
      purrr::pluck("group") %>% 
      dplyr::mutate(
        Group = paste0("Group ", Group)
      ) %>% tidyr::drop_na() %>%  ### TODO // remove
      echarts4r::e_charts(Group) %>% 
      echarts4r::e_bar(Group_Pct_Actual, name = "Actual % Correct") %>% 
      echarts4r::e_line(
        Group_Pct_Predicted, 
        name = "Predicted % Correct", 
        symbol = "circle", 
        symbolSize = 20
      ) %>% 
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter(
          style = "percent", 
          digits = 0
        )
      ) %>% 
      echarts4r::e_tooltip(
        trigger = "axis", 
        formatter = echarts4r::e_tooltip_pointer_formatter(
          style = "percent", 
          digits = 1
        )
      ) %>% 
      echarts4r::e_toolbox_feature(feature = "saveAsImage")
    
  })
  
  
  output$group_range_chart <- echarts4r::renderEcharts4r({
    
    shiny::req(rctv$current_data$range)
    
    rctv$current_data$range %>% 
      aggregate_range() %>% 
      purrr::pluck("group") %>% 
      dplyr::mutate(
        Group = paste0("Group ", Group), 
        Target = 0.90
      ) %>% tidyr::drop_na() %>%  ### TODO // remove
      echarts4r::e_charts(Group) %>% 
      echarts4r::e_bar(Group_Pct, name = "Actual % Correct") %>% 
      echarts4r::e_line(
        Target, 
        name = "Target % Correct", 
        symbol = "circle", 
        symbolSize = 20
      ) %>% 
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter(
          style = "percent", 
          digits = 0
        )
      ) %>% 
      echarts4r::e_tooltip(
        trigger = "axis", 
        formatter = echarts4r::e_tooltip_pointer_formatter(
          style = "percent", 
          digits = 1
        )
      ) %>% 
      echarts4r::e_toolbox_feature(feature = "saveAsImage")
        
  })
  
  
  
  
  output$download_binary_individual <- shiny::downloadHandler(
    
    filename = function() {
      
      paste0("calibration_binary_individual_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      rctv$current_data$binary %>% 
        aggregate_binary() %>% 
        purrr::pluck("individual") %>% 
        write.csv(file)
      
    }
    
  )
  
  output$download_binary_group <- shiny::downloadHandler(
    
    filename = function() {
      
      paste0("calibration_binary_group_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      rctv$current_data$binary %>% 
        aggregate_binary() %>% 
        purrr::pluck("group") %>% 
        write.csv(file)
      
    }
    
  )
  
  
  output$download_range_individual <- shiny::downloadHandler(
    
    filename = function() {
      
      paste0("calibration_range_individual_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      rctv$current_data$range %>% 
        aggregate_range() %>% 
        purrr::pluck("individual") %>% 
        write.csv(file)
      
    }
    
  )
  
  output$download_range_group <- shiny::downloadHandler(
    
    filename = function() {
      
      paste0("calibration_range_group_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      rctv$current_data$range %>% 
        aggregate_range() %>% 
        purrr::pluck("group") %>% 
        write.csv(file)
      
    }
    
  )
  
}

shinyApp(ui, server)
