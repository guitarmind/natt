
library(shiny)
library(shinydashboard)

server <- function(input, output, session) {

  diagnosis <- eventReactive(input$run_btton, {
    validate(
      need(input$input_heart > 0 && input$input_heart < 100, "HEART needs in between 1 and 99!"),
      need(input$input_sex > 0 && input$input_sex < 100, "SEX needs in between 1 and 99!"),
      need(input$input_health > 0 && input$input_health < 100, "HEALTH needs in between 1 and 99!"),
      need(input$input_fight > 0 && input$input_fight < 100, "FIGHT needs in between 1 and 99!"),
      need(input$input_vital > 0 && input$input_vital < 100, "VITAL needs in between 1 and 99!")
    )
    
    heart = diagnosis_rules("heart", input$input_heart)
    sex = diagnosis_rules("sex", input$input_sex)
    health = diagnosis_rules("health", input$input_health)
    fight = diagnosis_rules("fight", input$input_fight)
    vital = diagnosis_rules("vital", input$input_vital)
    
    result = list(heart=heart,
                  sex=sex,
                  health=health,
                  fight=fight,
                  vital=vital)
    return(result)
  }, ignoreNULL = TRUE)
  
  output$diagnoseRadar <- renderChartJSRadar({
    input$run_btton

    isolate({
      labs <- c("HEART", "HEALTH", "SEX", "FIGHT", "VITAL")
      scores <- list(
        "Patient" = c(input$input_heart,
                      input$input_sex,
                      input$input_health,
                      input$input_fight,
                      input$input_vital)
      )
      
      chartJSRadar(scores = scores, labs = labs,
                   maxScale = 99,
                   labelSize = 18,
                   colMatrix = grDevices::col2rgb(c("blue", "orange", "green")))
    })
  })
  
  output$diagnoseText <- renderText({
    input$run_btton
    
    isolate({
      data = diagnosis()
      
      paste(enc2native("心跳速率: "), paste(data$heart))
      paste(enc2native("副交感神經: "), paste(data$sex))
      paste(enc2native("自律神經整體活性: "), paste(data$health))
      paste(enc2native("交感神經: "), paste(data$fight))
      # paste(enc2native("總體神經功能: "), paste(data$vital))
    })
  })
}
