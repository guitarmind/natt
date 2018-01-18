
source("deps.R")

server <- function(input, output, session) {
  
  proxy = dataTableProxy("diagnoseTable")
  
  observeEvent(input$clear_btton, {
    updateTextInput(session, "input_heart", value = "")
    updateTextInput(session, "input_sex", value = "")
    updateTextInput(session, "input_health", value = "")
    updateTextInput(session, "input_fight", value = "")
    updateTextInput(session, "input_vital", value = "")
    
    replaceData(proxy, data.frame(), rownames = FALSE)
  }, ignoreNULL = TRUE)
  
  diagnosis <- eventReactive(input$run_btton, {
    validate(
      need(as.integer(input$input_heart) > 0 && as.integer(input$input_heart) < 100, enc2utf8("心跳速率(HEART)須介於1至99之間!")),
      need(as.integer(input$input_sex) > 0 && as.integer(input$input_sex) < 100, enc2utf8("副交感神經(SEX)須介於1至99之間!")),
      need(as.integer(input$input_health) > 0 && as.integer(input$input_health) < 100, enc2utf8("自律神經整體活性(HEALTH)須介於1至99之間!")),
      need(as.integer(input$input_fight) > 0 && as.integer(input$input_fight) < 100, enc2utf8("交感神經(FIGHT)須介於1至99之間!")),
      need(as.integer(input$input_vital) > 0 && as.integer(input$input_vital) < 100, enc2utf8("整體神經功能(VITAL)須介於1至99之間!"))
    )
    
    heart = diagnosis_rules("heart", as.integer(input$input_heart))
    sex = diagnosis_rules("sex", as.integer(input$input_sex))
    health = diagnosis_rules("health", as.integer(input$input_health))
    fight = diagnosis_rules("fight", as.integer(input$input_fight))
    vital = diagnosis_rules("vital", as.integer(input$input_vital))
    
    result = rbindlist(list(heart=heart,
                            sex=sex,
                            health=health,
                            fight=fight,
                            vital=vital))
    result = cbind(c(enc2utf8("心跳速率(HEART)"),
                     enc2utf8("副交感神經(SEX)"),
                     enc2utf8("自律神經整體活性(HEALTH)"),
                     enc2utf8("交感神經(FIGHT)"),
                     enc2utf8("總體神經功能(VITAL)")), result)
    names(result) = c("項目", "程度", "分析")
    
    # print(result)
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
                   responsive = F,
                   colMatrix = grDevices::col2rgb(c("#00ccff", "orange", "#00cc00")),
                   borderWidth = 10)
    })
  })
  
  output$diagnoseTable <- renderDataTable({
    data = diagnosis()
    datatable(data, options = list(searching = F,
                                   paging = F,
                                   ordering = F,
                                   info = F,
                                   autoWidth = F,
                                   columnDefs = list(
                                     list(width = '120px', targets = c(0)),
                                     list(width = '50px', targets = c(1)),
                                     list(className = 'dt-center', targets = 0:1)
                                   )), style = 'default', rownames = NULL) %>%
      formatStyle("程度",
                  color = styleEqual(
                    c("嚴重", "注意", "正常", "健康"),
                    c("white", "black", "white", "white")
                  ),
                  backgroundColor = styleEqual(
                    c("嚴重", "注意", "正常", "健康"),
                    c("red", "yellow", "green", "blue")
                  ),
                  fontWeight = 'bold')
  })
}
