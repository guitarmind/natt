
library(shiny)
library(shinydashboard)

# Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.setlocale(locale = "Chinese (Traditional)_Taiwan.950")

dbHeader <- dashboardHeader(title = "心率變異頻譜分析(HRV)")

ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(disable=T),
  dashboardBody()
)
