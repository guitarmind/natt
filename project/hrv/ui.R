
library(shiny)
library(shinydashboard)
library(radarchart)

ui <- dashboardPage(
  dashboardHeader(title = enc2native("心率變異度分析 Heart Rate Variability (HRV) Diagnosis"),
                  titleWidth = 550),
  dashboardSidebar(disable=T),
  dashboardBody(
    tags$style(type='text/css', '
                  #run_btton { width: 45%; margin-top: 25px; color: white; }
                  .shiny-output-error-validation {  color: red; }
               '),
    fluidPage(sidebarLayout(
      sidebarPanel(
        width = 3,
        numericInput(inputId = "input_heart",
                     label = "心跳速率(HEART):",
                     value = 0,
                     min = 1, max = 99,
                     width = "95%"),
        numericInput(inputId = "input_sex",
                     label = "副交感神經(SEX):",
                     value = 0,
                     min = 1, max = 99,
                     width = "95%"),
        numericInput(inputId = "input_health",
                     label = "自律神經整體活性(HEALTH):",
                     value = 0,
                     min = 1, max = 99,
                     width = "95%"),
        numericInput(inputId = "input_fight",
                     label = "交感神經(FIGHT):",
                     value = 0,
                     min = 1, max = 99,
                     width = "95%"),
        numericInput(inputId = "input_vital",
                     label = "整體神經功能(VITAL):",
                     value = 0,
                     min = 1, max = 99,
                     width = "95%"),
        div(actionButton("run_btton", "診斷",
                         icon = icon("refresh"),
                         class = "btn-primary"),
            style = "text-align: right")
      ),
      mainPanel(
        width = 9,
        wellPanel(
          h3("診斷結果",
             style = "text-align: center"),
          # plotOutput("diagnosePlot")
          chartJSRadarOutput("diagnoseRadar", width = "450", height = "300"),
          textOutput("diagnoseText"),
          style = "background-color: #fcfcfc;"
        )
      )
    ), title = enc2native("五力圖數值診斷"))
  )
)

