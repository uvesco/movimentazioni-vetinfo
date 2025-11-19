# R/app_server.R
app_server <- function(input, output, session) {                               # funzione server principale

        # Importazione dati --------------------------------                 # sezione di importazione
        upload <- mod_upload_movimentazioni_server("upload_mov")            # reattivi del modulo di upload
        animali <- upload$animali                                            # dataframe standardizzato
        gruppo <- upload$gruppo                                              # gruppo determinato dal file
        stato_upload <- upload$status                                        # stato del caricamento per i messaggi
        file_check <- upload$file_check                                      # verifica struttura del file (colonne)

        # genenera il data.frame standardizzato delle province e dei comuni (riuniti in una lista) con le malattie significative per gruppo di malattie (dal file mod_standard_data.R) importando tutti i files che sono caricati nella cartella
        #2do: aggiungere il caricamento manuale di files extra da parte dell'utente
        malattie <- mod_import_malattie("df_standard", gruppo)
        st_import <- malattie
        
        
        
        # Gestione tab dinamici --------------------------------              # sezione di gestione tab dinamici
        # crea due nuovi tab quando sono presenti dati validi
        tabs_inserite <- reactiveVal(FALSE)                                  # memorizza se i tab sono stati aggiunti

        observe({                                                            # osserva cambiamenti nei dati
                req(animali())                                              # esegue solo se dati presenti
                req(gruppo())                                               # richiede il gruppo determinato
                if (nrow(animali()) > 0 && !tabs_inserite() && isTRUE(file_check())) {          # se dati validi e tab non ancora inserite

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

                } else if (nrow(animali()) == 0 && tabs_inserite()) {      # se file vuoto rimuove le tab aggiunte
                        removeTab("tabs", "elaborazione")                # rimuove tab "Elaborazione"
                        removeTab("tabs", "output")                      # rimuove tab "Output"
                        tabs_inserite(FALSE)                                # aggiorna lo stato
                }
        })

        output$gruppo_tab <- renderText({                                   # stampa il gruppo nella tab
                req(gruppo())                                              # attende il valore del gruppo
                gruppo()
        })
        
        # tabella finale completa con possibilità di download in Excel
        output$tabella_output <- DT::renderDT({
                grp <- gruppo()
                req(grp)                                                 # richiede che il gruppo sia definito
                
                tryCatch({
                        malattie_data <- st_import()                     # ottiene i dati delle malattie
                        if (is.null(malattie_data)) return(NULL)         # verifica che ci siano dati
                        
                        # estrae i dati delle province per il gruppo corrente
                        df <- malattie_data[[grp]][["province"]]
                        if (is.null(df)) return(NULL)                    # verifica che il dataframe esista
                        
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
                }, error = function(e) {
                        message("Errore in tabella_output: ", e$message)
                        NULL                                             # ritorna NULL in caso di errore
                })
        }, server = FALSE)
        

        # messaggio sul tipo di file importato

        output$tipo_file <- renderText({                                    # messaggio informativo sul file caricato
                stato <- stato_upload()
                if (!is.null(stato$message) && stato$type %in% c("error", "empty")) {
                        return(stato$message)
                }

                df <- animali()                                            # recupera i dati
                if (is.null(df)) {                                         # nessun file caricato
                        return("File non ancora caricato")
                }

                tryCatch({                                                 # gestisce eventuali errori
                        grp <- gruppo()                                    # recupera il gruppo
                        req(grp)
                        if (nrow(df) == 0) {                               # file vuoto
                                return("File vuoto per i parametri selezionati")
                        } else {                                           # file corretto
                                paste("File importato correttamente per il gruppo", grp)
                        }
                }, error = function(e) {                                   # eventuali errori di lettura
                        paste("Errore nel file:", e$message)
                })
        })


        # ottieni il numero di righe dei dati importati

        output$n_animali <- renderUI({                                   # mostra numero di righe caricate
                df <- animali()                                           # ottiene i dati
                grp <- gruppo()                                           # ottiene il gruppo
                req(df, grp)                                              # si assicura che esistano
                div(
                        bs_icon("info-circle-fill"), em("Informazioni"), br(),
                        h4("Animali movimentati"),
                        "Gruppo specie: ", grp, br(),          # restituisce il conteggio
                        "Numero di animali importati: ", nrow(df)
                )          # restituisce il conteggio
        })
        
        # titolo per la sezione malattie (mostrato solo quando gruppo è definito)
        output$titolo_malattie <- renderUI({
                grp <- gruppo()
                req(grp)                                                 # richiede che il gruppo sia definito
                h4("Malattie")
        })
        
        # mostra le malattie importate
        output$malattie_importate <- renderTable({
                grp <- gruppo()
                req(grp)                                                 # richiede che il gruppo sia definito
                
                tryCatch({
                        malattie_data <- malattie()                      # ottiene i dati delle malattie
                        if (is.null(malattie_data)) return(NULL)         # verifica che ci siano dati
                        
                        df_malattie <- malattie_data[["metadati"]]       # estrae i metadati
                        if (is.null(df_malattie) || nrow(df_malattie) == 0) return(NULL)
                        
                        # filtra per il gruppo corrente
                        df_filtrato <- df_malattie[df_malattie$specie == grp, c("malattia", "riferimento", "data_inizio", "data_fine")]
                        
                        # formatta le date in formato dd/mm/yyyy
                        if (nrow(df_filtrato) > 0) {
                                df_filtrato$data_inizio <- format(df_filtrato$data_inizio, "%d/%m/%Y")
                                df_filtrato$data_fine <- format(df_filtrato$data_fine, "%d/%m/%Y")
                        }
                        
                        df_filtrato                                      # ritorna la tabella (anche se vuota)
                }, error = function(e) {
                        message("Errore in malattie_importate: ", e$message)
                        NULL                                             # ritorna NULL in caso di errore
                })
        })

        
}
