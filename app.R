Sys.setenv(TZ = "Europe/Rome")

library(shiny)
library(readxl)

# app.R
source("R/app_ui.R")
source("R/app_server.R")

# abilita temi automatici per plot
if (requireNamespace("thematic", quietly = TRUE)) thematic::thematic_shiny()

shiny::shinyApp(ui = app_ui(), server = app_server)

