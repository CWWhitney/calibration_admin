library(shiny)
library(dplyr)

source("global.R")

ui <- shiny::fluidPage(
  
  shiny::fluidPage(
    
    shiny::fluidRow(
      
      
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  rctv <- shiny::reactiveValues()
  
  rctv$current_data <- get_current_data(board = board)
  
  shiny::modalDialog(
    title = "Welcome, Admin!", 
    "This app contains the live results of each student in the current workshop."
  ) %>% 
    shiny::showModal()
  
}

shinyApp(ui, server)