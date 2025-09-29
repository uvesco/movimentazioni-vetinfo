Sys.setenv(TZ = "Europe/Rome")             # imposta il fuso orario a Roma

library(shiny)                               # carica il pacchetto Shiny
library(readxl)                              # pacchetto per leggere file Excel
library(dplyr)                               # pacchetto per la manipolazione dei dati
library(bsicons)
library(DT)

# importa le definizioni di interfaccia e server
source("R/app_ui.R")                        # definizione dell'interfaccia
source("R/app_server.R")                    # logica del server

# abilita temi automatici per i grafici se disponibile
if (requireNamespace("thematic", quietly = TRUE)) thematic::thematic_shiny()

# avvia l'applicazione Shiny con l'UI e il server
shiny::shinyApp(ui = app_ui(), server = app_server)

