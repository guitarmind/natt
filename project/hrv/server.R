
library(shiny)
library(shinydashboard)

server <- function(input, output, session) {
  output$diagnosePlot <- renderPlot({
    input$run_btton
    
    dist <- isolate(rnorm(20))
    hist(dist)
  })
}
