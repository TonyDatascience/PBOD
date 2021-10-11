if (interactive()) {
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shinyjs)
  shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Set up shinyjs
      
      fileInput("NewDiseaseFile", "Choose .XLSX File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".xlsx")),
      actionButton("btn", "Click me"),
      disabled(
        textInput("element", NULL, "I was born disabled")
      )
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        enable("element")
      })
    }
  )
}

library(shiny)
disabled(numericInput("num", NULL, 5), dateInput("date", NULL))