### CALIBRATION APP
### UNIVERSITY OF BONN
### DEVELOPED BY: KETCHBROOK ANALYTICS (MTHOMAS@KETCHBROOKANALYTICS.COM)

# 1.0 SETUP ----

## 1.1 Load Packages ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(reactable)
library(stringr)
library(purrr)
library(echarts4r)

## 1.2 Setup Environment
# Run "global.R" script to load shared objects across all sessions
source("global.R")


# 2.0 UI ----
ui <- shinydashboard::dashboardPage(
  
  ## 2.1 Header ----
  shinydashboard::dashboardHeader(
    title = "Calibrator: Admin"
  ),
  
  ## 2.2 Sidebar ----
  shinydashboard::dashboardSidebar(
    
    shiny::br(), 
    
    ### 2.2.1 "Refresh" Button ----
    # Create a button to refresh the {pins} data from RStudio Connect and 
    # retrieve the most up-to-date data from the workshop
    shiny::actionButton(
      class = "btn btn-success", 
      inputId = "refresh_btn", 
      label = "Refresh Data", 
      icon = shiny::icon("sync"), 
      width = "200px"
    ), 
    
    ### 2.2.2 "Select Student" Drop-down ----
    # Drop-down menu for selecting an individual workshop student to filter the 
    # app data by
    shiny::uiOutput(outputId = "select_student_picker"), 
    
    shiny::hr(), 
    
    ### 2.2.3 Sidebar Menu Items ----
    shinydashboard::sidebarMenu(
      
      # Create the "Binary" menu item
      shinydashboard::menuItem(
        text = "Binary", 
        icon = shiny::icon("check-circle"), 
        
        # Create the "Binary: Raw Data" menu sub-item
        shinydashboard::menuSubItem(
          text = "Raw Data", 
          tabName = "binary_raw_screen", 
          icon = shiny::icon("table")
        ), 
        
        # Create the "Binary: Analysis" menu sub-item
        shinydashboard::menuSubItem(
          text = "Analysis", 
          tabName = "binary_analysis_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      ), 
      
      # Create the "Range" menu item
      shinydashboard::menuItem(
        text = "Range", 
        icon = shiny::icon("sort-numeric-down"), 
        
        # Create the "Range: Raw Data" menu sub-item
        shinydashboard::menuSubItem(
          text = "Raw Data", 
          tabName = "range_raw_screen", 
          icon = shiny::icon("table")
        ), 
        
        # Create the "Range: Analysis" menu sub-item
        shinydashboard::menuSubItem(
          text = "Analysis", 
          tabName = "range_analysis_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      )
    )
    
  ),
  
  ## 2.3 Body ----
  shinydashboard::dashboardBody(
    
    shinydashboard::tabItems(
      
      ### 2.3.1 "Binary: Raw Data" Screen----
      shinydashboard::tabItem(
        tabName = "binary_raw_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            # Display the {reactable} table containing the raw "binary" data
            reactable::reactableOutput(outputId = "binary_raw_tbl")
          )
        )
      ), 
      
      ### 2.3.2 "Binary: Analysis" Screen ----
      shinydashboard::tabItem(
        tabName = "binary_analysis_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            # Create a tab box for displaying either the "Individual" or "Group" 
            # table
            shiny::tabsetPanel(
              
              #### 2.3.2a "Binary: Individual" Table ----
              shiny::tabPanel(
                title = "Individual", 
                shiny::br(), 
                
                # Display the {reactable} table containing the individual 
                # "binary" data
                reactable::reactableOutput(outputId = "individual_binary_tbl"), 
                
                shiny::br(), 
                
                # Create a button to download the data in the individual 
                # "binary" table
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_binary_individual", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
                
              ), 
              
              #### 2.3.2b "Binary: Group" Table ----
              shiny::tabPanel(
                title = "Group", 
                shiny::br(), 
                
                # Display the {reactable} table containing the group "binary" 
                # data
                reactable::reactableOutput(outputId = "group_binary_tbl"), 
                
                shiny::br(), 
                
                # Create a button to download the data in the group "binary" 
                # table
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_binary_group", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
              )
              
            ), 
            
            shiny::hr(), 
            
            #### 2.3.2c "Binary: Group" Chart ----
            echarts4r::echarts4rOutput(outputId = "group_binary_chart")
            
          )
        )
      ), 
      
      ### 2.3.3 "Range: Raw Data" Screen ----
      shinydashboard::tabItem(
        tabName = "range_raw_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            # Display the {reactable} table containing the raw "range" data
            reactable::reactableOutput(outputId = "range_raw_tbl")
          )
        )
      ), 
      
      ### 2.3.4 "Range: Analysis" Screen ----
      shinydashboard::tabItem(
        tabName = "range_analysis_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            # Create a tab box for displaying either the "Individual" or "Group" 
            # table
            shiny::tabsetPanel(
              
              #### 2.3.4a "Range: Individual" Table ----
              shiny::tabPanel(
                title = "Individual", 
                shiny::br(), 
                
                # Display the {reactable} table containing the group "binary" data
                reactable::reactableOutput(outputId = "individual_range_tbl"), 
                
                shiny::br(), 
                
                # Create a button to download the data in the individual 
                # "range" table
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_range_individual", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
                
              ), 
              
              #### 2.3.4b "Range: Group" Table ----
              shiny::tabPanel(
                title = "Group", 
                shiny::br(), 
                reactable::reactableOutput(outputId = "group_range_tbl"), 
                
                shiny::br(), 
                
                # Create a button to download the data in the group 
                # "range" table
                shiny::downloadButton(
                  class = "btn btn-warning", 
                  outputId = "download_range_group", 
                  label = "Download Data", 
                  icon = shiny::icon("download")
                )
              )
              
            ), 
            
            shiny::hr(), 
            
            #### 2.3.4c "Range: Group" Chart ----
            echarts4r::echarts4rOutput(outputId = "group_range_chart")
            
          )
        )
      )
      
    )
    
  ), 
  
  # Define the overall color theme for the dashboard
  skin = "green"
)


# 3.0 SERVER ----
server <- function(input, output, session) {
  
  ## 3.1 Initialize ReactiveValues ----
  # Create a `reactiveValues` object that holds our reactive objects
  rctv <- shiny::reactiveValues()

  ## 3.2 Get Initial Data ----
  # Download the {pins} data from the current workshop board
  rctv$current_data <- get_current_data(board = board)

  ## 3.3 Welcome Modal ----
  # On app launch, display a pop-up modal welcoming the admin user
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
  
  ## 3.4 Refresh Data ----
  # When the "Refresh Data" button is clicked...
  shiny::observeEvent(input$refresh_btn, {
    
    # Display a notification in the bottom right-hand corner of the page 
    shiny::showNotification(
      ui = "Please Wait...", 
      closeButton = FALSE, 
      id = "wait_notification", 
      type = "warning"
    )
    
    # Force a 1-second pause (to guarantee the notification is displayed)
    Sys.sleep(1)
    
    # Re-download the {pins} data from the workshop board
    rctv$current_data <- get_current_data(board = board)
    
    # Remove the notification
    shiny::removeNotification(id = "wait_notification")
    
  })
  
  
  
  ## 3.5 Reactive Drop-Down Picker ----
  # Create a dynamic drop-down list of each student in the workshop
  output$select_student_picker <- shiny::renderUI({
    
    # Require that the data has been retrieved from the {pins} board
    shiny::req(rctv$current_data)
    
    # Build the drop-down widget containing the individual students & an "All" 
    # option
    shiny::selectInput(
      inputId = "choose_student", 
      label = "Select a Student", 
      choices = c("All", unique(
        rctv$current_data$binary$user, 
        rctv$current_data$range$user
      )), 
      selected = "All"   # start with "All" selected by default
    )
    
  })
  
  ## 3.6 Interactive Tables & Charts ----
  
  ### 3.6.1 Binary Raw Table ----
  output$binary_raw_tbl <- reactable::renderReactable({

    # Require that the "binary" data has been retrieved from the {pins} board, 
    # and that a valid selection from the "Students" drop-down has been made
    shiny::req(
      rctv$current_data$binary, 
      input$choose_student
    )

    # Capture all of the current "binary" data for (possible) filtering
    data <- rctv$current_data$binary
    
    # Filter the "binary" data for the selected student
    if (input$choose_student != "All") {
      
      data <- rctv$current_data$binary %>% 
        dplyr::filter(user == input$choose_student)
      
    }
    
    # Create the interactive {reactable} table holding the "binary" raw data
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

  ### 3.6.2 Range Raw Table ----
  output$range_raw_tbl <- reactable::renderReactable({

    # Require that the "range" data has been retrieved from the {pins} board, 
    # and that a valid selection from the "Students" drop-down has been made
    shiny::req(
      rctv$current_data$range, 
      input$choose_student
    )

    # Capture all of the current "range" data for (possible) filtering
    data <- rctv$current_data$range
    
    # Filter the "range" data for the selected student
    if (input$choose_student != "All") {
      
      data <- rctv$current_data$range %>% 
        dplyr::filter(user == input$choose_student)
      
    }
    
    # Create the interactive {reactable} table holding the "range" raw data
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
  
  ### 3.6.3 Individual Binary Table ----
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
  
  ### 3.6.4 Individual Range Table ----
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
  
  ### 3.6.5 Group Binary Table ----
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
  
  ### 3.6.6 Group Range Table ----
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
  
  ### 3.6.7 Group Binary Chart ----
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
  
  ### 3.6.8 Group Range Chart ----
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
  
  ## 3.7 Data Download Handlers ----
  
  ### 3.7.1 Download Binary Individual Data ----
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
  
  ### 3.7.2 Download Binary Group Data ----
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
  
  ### 3.7.3 Download Range Individual Data ----
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
  
  ### 3.7.4 Download Range Group Data ----
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
