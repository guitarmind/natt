
library(shiny)


# manually set fixed port for local test
# options(shiny.port = 1221)

options(shiny.usecairo = FALSE)

# source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
