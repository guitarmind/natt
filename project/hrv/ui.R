
source("deps.R")

ui <- dashboardPage(
  dashboardHeader(title = enc2utf8("心率變異度分析 Heart Rate Variability (HRV) Diagnosis"),
                  titleWidth = 550),
  dashboardSidebar(disable=T),
  dashboardBody(
    tags$style(type='text/css', '
                  #run_btton { width: 45%; margin-top: 25px; color: white; }
                  #clear_btton { width: 45%; margin-top: 25px; margin-right: 5px; color: white; }
                  .shiny-output-error-validation {  color: red; }
               '),
    tags$script(
      'Shiny.addCustomMessageHandler("refocus",
        function(element_id) {
          document.getElementById(element_id).focus();
        }
      );'
    ),
    fluidPage(sidebarLayout(
      sidebarPanel(
        width = 3,
        textInput(inputId = "input_heart",
                  label = enc2utf8("心跳速率(HEART):"),
                  value = "",
                  width = "95%"),
        textInput(inputId = "input_sex",
                  label = enc2utf8("副交感神經(SEX):"),
                  value = "",
                  width = "95%"),
        textInput(inputId = "input_health",
                  label = enc2utf8("自律神經整體活性(HEALTH):"),
                  value = "",
                  width = "95%"),
        textInput(inputId = "input_fight",
                  label = enc2utf8("交感神經(FIGHT):"),
                  value = "",
                  width = "95%"),
        textInput(inputId = "input_vital",
                  label = enc2utf8("整體神經功能(VITAL):"),
                  value = "",
                  width = "95%"),
        div(actionButton("clear_btton", enc2utf8("清除"),
                         icon = icon("trash"),
                         class = "btn-primary"),
            actionButton("run_btton", enc2utf8("診斷"),
                         icon = icon("refresh"),
                         class = "btn-primary"),
            style = "text-align: right")
      ),
      mainPanel(
        width = 9,
        column(
          width = 12,
          align="center",
          wellPanel(
            h3(enc2utf8("診斷結果"),
               style = "text-align: center;"),
            chartJSRadarOutput("diagnoseRadar"),
            # chartJSRadarOutput("diagnoseRadar", width = "300", height = "200"),
            style = "background-color: #fcfcfc;"
          ),
          wellPanel(
            dataTableOutput('diagnoseTable'),
            style = "height: 100%;"
          )
        )
      )
    ), title = enc2utf8("五力圖數值診斷"))
  )
)
