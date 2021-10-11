library(shiny)
file.choose2 <- function(...) {
  pathname <- NULL;
  tryCatch({
    pathname <- file.choose();
  }, error = function(ex) {
  })
  pathname;
}
ui <- fluidPage(
  
  headerPanel("Example"),
  
  sidebarPanel(
    textInput("path", "File:"),
    actionButton("browse", "Browse"),
    tags$br(),
    actionButton("upload", "Upload Data")
  ),
  
  mainPanel(
    verbatimTextOutput('content')
  )
)

#shinyServer(function(input, output, session) {
  server <- function(session,input, output) {
  observe({
    
    if (input$browse == 0) return()
    
    updateTextInput(session, "path",  value = file.choose2())
  })
  
  contentInput <- reactive({ 
    
    if(input$upload == 0) return()
    
    isolate({
      writeLines(paste(readLines(input$path), collapse = "\n"))
    })
  })
  
  output$content <- renderPrint({
    contentInput()
  })
  
  }
  shinyApp(ui, server)
  