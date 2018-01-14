
library(shiny)
library(shinydashboard)

server <- function(input, output, session) {

  diagnosis <- eventReactive(input$run_btton, {
    return(NULL)
  })
  
  output$diagnosePlot <- renderPlot({
    input$run_btton
    
    data = diagnosis()
    
    validate(
      need(input$input_heart > 0 && input$input_heart < 100, "HEART needs in between 1 and 99!"),
      need(input$input_sex > 0 && input$input_sex < 100, "SEX needs in between 1 and 99!"),
      need(input$input_health > 0 && input$input_health < 100, "HEALTH needs in between 1 and 99!"),
      need(input$input_fight > 0 && input$input_fight < 100, "FIGHT needs in between 1 and 99!"),
      need(input$input_vital > 0 && input$input_vital < 100, "VITAL needs in between 1 and 99!")
    )
    
    dist <- isolate(rnorm(20))
    hist(dist)
  })
}
