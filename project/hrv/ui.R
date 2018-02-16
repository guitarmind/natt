
source("deps.R")

body <- dashboardBody(tags$style(type='text/css', '
                        #run_button, #login_button { width: 45%; margin-top: 25px; color: white; }
                        #clear_button { width: 45%; margin-top: 25px; margin-right: 5px; color: white; }
                        .shiny-output-error-validation {  color: red; }
                      '),
                      uiOutput("body"))

ui <- dashboardPage(
  dashboardHeader(title = enc2utf8("心率變異度分析 Heart Rate Variability (HRV) Diagnosis"),
                  titleWidth = 550),
  dashboardSidebar(disable=T),
  body
)
