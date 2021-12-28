library(shiny)
library(shinydashboard)
library(dplyr)

source("global.R")

ui <- shinydashboard::dashboardPage(
  
  shinydashboard::dashboardHeader(
    title = "Calibrator: Admin"
  ),
  
  shinydashboard::dashboardSidebar(
    
    shiny::uiOutput(outputId = "select_student_picker"),
    
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
          text = "Charts", 
          tabName = "binary_charts_screen", 
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
          text = "Charts", 
          tabName = "range_charts_screen", 
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
            reactable::reactableOutput(outputId = "tmp_binary")
          )
        )
      ), 
      
      shinydashboard::tabItem(
        tabName = "binary_charts_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            shiny::p("Placeholder")
          )
        )
      ), 
      
      shinydashboard::tabItem(
        tabName = "range_raw_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            reactable::reactableOutput(outputId = "tmp_range")
          )
        )
      ), 
      
      shinydashboard::tabItem(
        tabName = "range_charts_screen", 
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            shiny::p("Placeholder")
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
    "This app contains the live results of each student in the current workshop."
  ) %>%
    shiny::showModal()
  
  
  
  # Create a dynamic drop-down list of each student in the workshop
  output$select_student_picker <- shiny::renderUI({
    
    shiny::req(rctv$current_data)
    
    shiny::selectInput(
      inputId = "choose_student", 
      label = "Select a Student", 
      choices = c("All", unique(
        rctv$current_data$binary$User, 
        rctv$current_data$range$User
      )), 
      selected = "All"
    )
    
  })
  

  output$tmp_binary <- reactable::renderReactable({

    shiny::req(rctv$current_data$binary)

    rctv$current_data$binary %>%
      reactable::reactable()

  })

  output$tmp_range <- reactable::renderReactable({

    shiny::req(rctv$current_data$range)

    rctv$current_data$range %>%
      reactable::reactable()

  })
  
}

shinyApp(ui, server)
