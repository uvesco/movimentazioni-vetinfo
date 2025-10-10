# R/app_server.R
app_server <- function(input, output, session) {                               # funzione server principale

        # Importazione dati --------------------------------                 # sezione di importazione
        animali <- mod_upload_movimentazioni_server("upload_mov")           # reactive del modulo di upload

        # dati_statici
        df_specie <- read.csv("data_static/specie.csv", stringsAsFactors = FALSE)  # tabella specie statiche


        gruppo <- reactive({                                                  # determina il gruppo di specie
                req(animali())                                               # assicura che i dati siano presenti
                df <- animali()                                              # recupera il dataframe caricato
                determinare_gruppo(df, df_specie)                            # applica la funzione di classificazione
        })

        file_check <- mod_file_check_server("file_check", animali, gruppo)  # verifica struttura del file

        st_import <- mod_standardize_server("df_standard", animali, gruppo)
        
        # crea due nuovi tab in caso animali() != "vuoto" e non sia NULL

        tabs_inserite <- reactiveVal(FALSE)                                  # memorizza se i tab sono stati aggiunti

        observe({                                                            # osserva cambiamenti nei dati
                req(animali())                                              # esegue solo se dati presenti
                if (gruppo() != "vuoto" && !tabs_inserite() && file_check() == T) {             # se gruppo valido e tab non ancora inserite

                                insertTab(                                  # aggiunge tab "Elaborazione"
                                        inputId = "tabs", target = "input", position = "after",
                                        tab = tabPanel(title = "Elaborazione", value = "elaborazione",
                                                       textOutput("gruppo_tab"))
                                        )

                                insertTab(                                  # aggiunge tab "Output"
                                        inputId = "tabs", target = "elaborazione", position = "after",
                                        tab = tabPanel(
                                                title = "Output", value = "output",
                                                DT::DTOutput("tabella_output")
                                        )
                                )

                                tabs_inserite(TRUE)                         # segna che i tab sono stati inseriti

                } else if (gruppo() == "vuoto" && tabs_inserite()) {      # se file vuoto rimuove le tab aggiunte
                        removeTab("tabs", "elaborazione")                # rimuove tab "Elaborazione"
                        removeTab("tabs", "output")                      # rimuove tab "Output"
                        tabs_inserite(FALSE)                                # aggiorna lo stato
                }
        })

        output$gruppo_tab <- renderText(gruppo())                           # stampa il gruppo nella tab
        
        # tabella finale completa con possibilità di download in Excel
        output$tabella_output <- DT::renderDT({
                df <- st_import()
                req(df)
                DT::datatable(
                        df,
                        extensions = "Buttons",
                        options = list(
                                dom = "Bfrtip",
                                buttons = list(list(
                                        extend = "excel",
                                        filename = paste0("movimentazioni_", format(Sys.Date(), "%Y%m%d"))
                                )),
                                pageLength = 8,
                                lengthMenu = c(8, 15, 25),
                                scrollX = TRUE
                        ),
                        filter = "top",
                        rownames = FALSE
                )
        }, server = FALSE)
        

        # messaggio sul tipo di file importato

        output$tipo_file <- renderText({                                    # messaggio informativo sul file caricato
                df <- animali()                                            # recupera i dati
                if (is.null(df)) {                                         # nessun file caricato
                        "File non ancora caricato"
                } else {
                        tryCatch({                                         # gestisce eventuali errori
                                if (gruppo() == "vuoto") {               # file vuoto
                                        return(paste0("File vuoto: ", colnames(df)[1]))
                                } else {                                   # file corretto
                                        paste("File importato correttamente.")
                                }
                        }, error = function(e) {                           # eventuali errori di lettura
                                paste("Errore nel file:", e$message)
                        })
                }
        })


        # ottieni il numero di righe dei dati importati

        output$n_animali <- renderUI({                                   # mostra numero di righe caricate
                df <- animali()                                           # ottiene i dati
                req(df)                                                   # si assicura che esistano
                div(bs_icon("info-circle-fill"), em("Informazioni"), br(),
                "Gruppo specie: ", gruppo(), br(),          # restituisce il conteggio
                "Numero di animali importati: ", nrow(df))          # restituisce il conteggio
        })
        

        
}
