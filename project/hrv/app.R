
library(shiny)


# manually set fixed port for local test
# options(shiny.port = 1221)
# options(encoding = 'UTF-8')

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
