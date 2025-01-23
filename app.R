### CALIBRATION ADMIN APP
### UNIVERSITY OF BONN
### DEVELOPED BY: KETCHBROOK ANALYTICS (MTHOMAS@KETCHBROOKANALYTICS.COM)

# SETUP ----------------------------------------------------------------------

## Load Packages -------------------------------------------------------------
library(shiny)
library(shinydashboard)   # dashboard layout
library(dplyr)   # general data prep
library(rhandsontable) # excel like interactive tables
library(reactable)   # interactive tables
library(stringr)   # working with strings
library(purrr)   # working with lists
library(echarts4r)   # interactive charts
library(fs)
library(pins)

## Setup Environment ---------------------------------------------------------
# Run "global.R" script to load shared objects across all sessions
source("global.R")


# UI -------------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(
  
  ## Header ------------------------------------------------------------------
  shinydashboard::dashboardHeader(
    title = "Calibrator: Admin"
  ),
  
  ## Sidebar -----------------------------------------------------------------
  shinydashboard::dashboardSidebar(
    
    shiny::br(), 
    
    ### "Refresh" ------------------------------------------------------------
    # Create a button to refresh the {pins} data from RStudio Connect and 
    # retrieve the most up-to-date data from the workshop
    shiny::actionButton(
      class = "btn btn-success", 
      inputId = "refresh_btn", 
      label = "Refresh Data", 
      icon = shiny::icon("sync"), 
      width = "200px"
    ), 
    
    ### "Select Student" ----------------------------------------------------
    # Drop-down menu for selecting an individual workshop student to filter the 
    # app data by
    shiny::uiOutput(outputId = "select_student_picker"), 
    
    shiny::hr(), 
    
    ### Sidebar Menu Items ---------------------------------------------------
    shinydashboard::sidebarMenu(
      
      # Create the "Question" menu item
      shinydashboard::menuItem(
        text = "Question", 
        icon = shiny::icon("clipboard-question"), 
        
        # Create the "Selection Screen" menu sub-item
        shinydashboard::menuSubItem(
          text = "Selection Screen", 
          tabName = "question_selection_screen", 
          icon = shiny::icon("table")
        ), 
        
        # Create the "Set Screen" menu sub-item
        shinydashboard::menuSubItem(
          text = "Set Screen", 
          tabName = "question_set_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      ),
      
      # Create the "Binary" menu item
      shinydashboard::menuItem(
        text = "Binary", 
        icon = shiny::icon("check-circle"), 
        
        # Create the "Raw Data Screen" menu sub-item
        shinydashboard::menuSubItem(
          text = "Raw Data Screen", 
          tabName = "binary_raw_screen", 
          icon = shiny::icon("table")
        ), 
        
        # Create the "Analysis Screen" menu sub-item
        shinydashboard::menuSubItem(
          text = "Analysis Screen", 
          tabName = "binary_analysis_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      ), 
      
      # Create the "Range" menu item
      shinydashboard::menuItem(
        text = "Range", 
        icon = shiny::icon("sort-numeric-down"), 
        
        # Create the "Raw Data Screen" menu sub-item
        shinydashboard::menuSubItem(
          text = "Raw Data Screen", 
          tabName = "range_raw_screen", 
          icon = shiny::icon("table")
        ), 
        
        # Create the "Analysis Screen" menu sub-item
        shinydashboard::menuSubItem(
          text = "Analysis Screen", 
          tabName = "range_analysis_screen", 
          icon = shiny::icon("chart-bar")
        )
        
      )
    )
    
  ),
  
  ## Body --------------------------------------------------------------------
  shinydashboard::dashboardBody(
    
    shinydashboard::tabItems(
      
      ### Question -----------------------------------------------------------
      
      #### Selection Screen --------------------------------------------------
      shinydashboard::tabItem(
        tabName = "question_selection_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            # Display the {reactable} table containing the raw "binary" data
            mod_question_selection_ui("mod_question_selection")
          )
        )
      ), 
      
      #### Set Screen --------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "question_set_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12,
            mod_question_sets_display_ui("mod_question_sets_display")
          )
        )
      ),
      
      ### Binary -------------------------------------------------------------
      
      #### Raw Data Screen ---------------------------------------------------
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
      
      #### Analysis Screen ---------------------------------------------------
      shinydashboard::tabItem(
        tabName = "binary_analysis_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            # Create a tab box for displaying either the "Individual" or "Group" 
            # table
            shiny::tabsetPanel(
              
              ##### Individual Table ------------------------------------------
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
              
              ##### Group Table -----------------------------------------------
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
            
            ##### Group Chart ------------------------------------------------
            echarts4r::echarts4rOutput(outputId = "group_binary_chart")
            
          )
        )
      ), 
      
      ### Range --------------------------------------------------------------
      
      #### Raw Data Screen ---------------------------------------------------
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
      
      #### Analysis Screen ---------------------------------------------------
      shinydashboard::tabItem(
        tabName = "range_analysis_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            
            # Create a tab box for displaying either the "Individual" or "Group" 
            # table
            shiny::tabsetPanel(
              
              ##### Individual Table ------------------------------------------
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
              
              ##### Group Table -----------------------------------------------
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
            
            ##### Group Chart ------------------------------------------------
            echarts4r::echarts4rOutput(outputId = "group_range_chart")
            
          )
        )
      )
      
    )
    
  ), 
  
  # Define the overall color theme for the dashboard
  skin = "green"
)

# SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Initialize ReactiveValues ------------------------------------------------
  # Create a `reactiveValues` object that holds our reactive objects
  rctv <- shiny::reactiveValues()
  
  ## Get Initial Data ---------------------------------------------------------
  # Download the {pins} data from the current workshop board
  # rctv$current_data <- get_current_data(board = board)
  
  ## Welcome Modal ------------------------------------------------------------
  # On app launch, display a pop-up modal welcoming the admin user
  shiny::modalDialog(
    title = "Welcome, Admin!", 
    shiny::HTML(
      glue::glue(
        "<iframe width='560' height='315'", 
        "src='https://www.youtube.com/embed/7P2YI9-smfU'", 
        "title='YouTube video player' frameborder='0' allow='accelerometer;", 
        "autoplay; clipboard-write; encrypted-media; gyroscope;", 
        "picture-in-picture' allowfullscreen></iframe>", 
        .sep = " "
      )
    ), 
    "This app contains the live results of each student in the current workshop."
  ) |>
    shiny::showModal()
  
  ## "Refresh" -----------------------------------------------------------------
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
    # rctv$current_data <- get_current_data(board = board)
    
    # Remove the notification
    shiny::removeNotification(id = "wait_notification")
    
  })
  
  ## "Select Student" ---------------------------------------------------------
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
        rctv$current_data$binary$User, 
        rctv$current_data$range$User
      )), 
      selected = "All"   # start with "All" selected by default
    )
    
  })
  
  ## Interactive Tables & Charts ----------------------------------------------
  
  ### Question ---------------------------------------------------------------
  
  question_sets <- reactiveVal(
    question_sets_static
  )
  
  #### Selection Screen ------------------------------------------------------
  mod_question_selection_server(
    "mod_question_selection", 
    questions_full, 
    question_sets
  )
  
  #### Set Screen ------------------------------------------------------------
  mod_question_sets_display_server(
    "mod_question_sets_display", 
    question_sets
  )
  
  ### Binary ------------------------------------------------------------------
  
  #### Raw Data Screen -------------------------------------------------------
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
      
      data <- rctv$current_data$binary |> 
        dplyr::filter(User == input$choose_student)
      
    }
    
    # Create the interactive {reactable} table holding the "binary" raw data
    data |>
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
  
  #### Analysis Screen -------------------------------------------------------
  
  ##### Individual Table -----------------------------------------------------
  output$individual_binary_tbl <- reactable::renderReactable({
    
    # Require that the "binary" data has been retrieved from the {pins} board
    shiny::req(rctv$current_data$binary)
    
    # Capture all of the current "binary" data for (possible) filtering
    data <- rctv$current_data$binary |> 
      aggregate_binary() |> 
      purrr::pluck("individual")
    
    # Filter the "binary" data for the selected student
    if (input$choose_student != "All") {
      
      data <- data |> 
        dplyr::filter(User == input$choose_student)
      
    }
    
    # Create an interactive {reactable} table holding the individual "binary"
    # aggregated data
    reactable::reactable(
      data, 
      filterable = TRUE, 
      columns = list(
        Actual = reactable::colDef(filterable = FALSE), 
        Predicted = reactable::colDef(filterable = FALSE), 
        Total = reactable::colDef(filterable = FALSE)
      )
    )
    
  })
  
  ##### Group Table ----------------------------------------------------------
  output$group_binary_tbl <- reactable::renderReactable({
    
    # Require that the "binary" data has been retrieved from the {pins} board
    shiny::req(rctv$current_data$binary)
    
    # Create an interactive {reactable} table holding the group "binary"
    # aggregated data
    rctv$current_data$binary |> 
      aggregate_binary() |> 
      purrr::pluck("group") |> 
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
  
  ##### Group Chart ----------------------------------------------------------
  output$group_binary_chart <- echarts4r::renderEcharts4r({
    
    # Require that the "binary" data has been retrieved from the {pins} board
    shiny::req(rctv$current_data$binary)
    
    # Create an interactive chart containing the group "binary" aggregated data
    rctv$current_data$binary |> 
      aggregate_binary() |> 
      purrr::pluck("group") |> 
      dplyr::mutate(
        Group = paste0("Group ", Group)
      ) |> tidyr::drop_na() |>  ### TODO // remove
      echarts4r::e_charts(Group) |> 
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
      echarts4r::e_tooltip(
        trigger = "axis", 
        formatter = echarts4r::e_tooltip_pointer_formatter(
          style = "percent", 
          digits = 1
        )
      ) |> 
      echarts4r::e_toolbox_feature(feature = "saveAsImage")
    
  })
  
  ### Range -------------------------------------------------------------------
  
  #### Raw Data Screen -------------------------------------------------------
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
      
      data <- rctv$current_data$range |> 
        dplyr::filter(User == input$choose_student)
      
    }
    
    # Create the interactive {reactable} table holding the "range" raw data
    data |>
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
  
  #### Analysis Screen -------------------------------------------------------
  
  ##### Individual Table -----------------------------------------------------
  output$individual_range_tbl <- reactable::renderReactable({
    
    # Require that the "range" data has been retrieved from the {pins} board
    shiny::req(rctv$current_data$range)
    
    # Capture all of the current "range" data for (possible) filtering
    data <- rctv$current_data$range |> 
      aggregate_range() |> 
      purrr::pluck("individual")
    
    # Filter the "range" data for the selected student
    if (input$choose_student != "All") {
      
      data <- data |> 
        dplyr::filter(User == input$choose_student)
      
    }
    
    # Create an interactive {reactable} table holding the individual "range"
    # aggregated data
    reactable::reactable(
      data, 
      filterable = TRUE, 
      columns = list(
        Bounded = reactable::colDef(filterable = FALSE),
        Total = reactable::colDef(filterable = FALSE)
      )
    )
    
  })
  
  ##### Group Table ----------------------------------------------------------
  output$group_range_tbl <- reactable::renderReactable({
    
    # Require that the "range" data has been retrieved from the {pins} board
    shiny::req(rctv$current_data$range)
    
    # Create an interactive {reactable} table holding the group "range"
    # aggregated data
    rctv$current_data$range |> 
      aggregate_range() |> 
      purrr::pluck("group") |> 
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
  
  ##### Group Chart ----------------------------------------------------------
  output$group_range_chart <- echarts4r::renderEcharts4r({
    
    # Require that the "range" data has been retrieved from the {pins} board
    shiny::req(rctv$current_data$range)
    
    # Create an interactive chart containing the group "range" aggregated data
    rctv$current_data$range |> 
      aggregate_range() |> 
      purrr::pluck("group") |> 
      dplyr::mutate(
        Group = paste0("Group ", Group), 
        Target = 0.90
      ) |> tidyr::drop_na() |>  ### TODO // remove
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
      echarts4r::e_tooltip(
        trigger = "axis", 
        formatter = echarts4r::e_tooltip_pointer_formatter(
          style = "percent", 
          digits = 1
        )
      ) |> 
      echarts4r::e_toolbox_feature(feature = "saveAsImage")
    
  })
  
  ## Data Download Handlers --------------------------------------------------
  
  ### Download Binary Individual Data ----------------------------------------
  output$download_binary_individual <- shiny::downloadHandler(
    
    filename = function() {
      
      # Name the downloaded file
      paste0("calibration_binary_individual_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      # Write the data out to a .csv for download
      rctv$current_data$binary |> 
        aggregate_binary() |> 
        purrr::pluck("individual") |> 
        write.csv(file)
      
    }
    
  )
  
  ### Download Binary Group Data ---------------------------------------------
  output$download_binary_group <- shiny::downloadHandler(
    
    filename = function() {
      
      # Name the downloaded file
      paste0("calibration_binary_group_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      # Write the data out to a .csv for download
      rctv$current_data$binary |> 
        aggregate_binary() |> 
        purrr::pluck("group") |> 
        write.csv(file)
      
    }
    
  )
  
  ### Download Range Individual Data -----------------------------------------
  output$download_range_individual <- shiny::downloadHandler(
    
    filename = function() {
      
      # Name the downloaded file
      paste0("calibration_range_individual_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      # Write the data out to a .csv for download
      rctv$current_data$range |> 
        aggregate_range() |> 
        purrr::pluck("individual") |> 
        write.csv(file)
      
    }
    
  )
  
  ### Download Range Group Data ----------------------------------------------
  output$download_range_group <- shiny::downloadHandler(
    
    filename = function() {
      
      # Name the downloaded file
      paste0("calibration_range_group_", Sys.Date(), ".csv")
      
    }, 
    
    content = function(file) {
      
      # Write the data out to a .csv for download
      rctv$current_data$range |> 
        aggregate_range() |> 
        purrr::pluck("group") |> 
        write.csv(file)
      
    }
    
  )
  
}

shinyApp(ui, server)
