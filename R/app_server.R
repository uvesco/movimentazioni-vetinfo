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
        
        # Pipeline controlli --------------------------------                  # sezione pipeline validazioni
        pipeline <- mod_pipeline_controlli_server(
                "pipeline",
                animali = animali,
                gruppo = gruppo,
                malattie_data = malattie
        )
        
        
        
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
        
        # tabella malattie completa con possibilità di download in Excel
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
        
        # tabella animali movimentati completa dopo merge con le malattie
        output$tabella_animali_movimentati <- DT::renderDT({
                df <- pipeline$dati_processati()                           # ottiene i dati processati
                req(df)                                                    # richiede che esistano
                
                DT::datatable(
                        df,
                        extensions = "Buttons",
                        options = list(
                                dom = "Bfrtip",
                                buttons = list(list(
                                        extend = "excel",
                                        filename = paste0("animali_movimentati_", format(Sys.Date(), "%Y%m%d"))
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
        
        # ---- Outputs for Controllo Manuale tab ----
        
        # Table: provenienza non trovata
        output$tabella_provenienza_non_trovata <- DT::renderDT({
                req(pipeline$df_provenienza_non_trovati())
                df <- pipeline$df_provenienza_non_trovati()
                
                if (nrow(df) == 0) {
                        return(NULL)
                }
                
                DT::datatable(
                        df,
                        options = list(
                                pageLength = 10,
                                scrollX = TRUE
                        ),
                        rownames = FALSE
                )
        })
        
        # Download handler: provenienza non trovata
        output$download_provenienza_non_trovata <- downloadHandler(
                filename = function() {
                        paste0("provenienza_non_trovata_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
                },
                content = function(file) {
                        req(pipeline$df_provenienza_non_trovati())
                        df <- pipeline$df_provenienza_non_trovati()
                        openxlsx::write.xlsx(df, file)
                }
        )
        
        # Table: nascita non trovata
        output$tabella_nascita_non_trovata <- DT::renderDT({
                req(pipeline$df_nascita_non_trovati())
                df <- pipeline$df_nascita_non_trovati()
                
                if (nrow(df) == 0) {
                        return(NULL)
                }
                
                DT::datatable(
                        df,
                        options = list(
                                pageLength = 10,
                                scrollX = TRUE
                        ),
                        rownames = FALSE
                )
        })
        
        # Download handler: nascita non trovata
        output$download_nascita_non_trovata <- downloadHandler(
                filename = function() {
                        paste0("nascita_non_trovata_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
                },
                content = function(file) {
                        req(pipeline$df_nascita_non_trovati())
                        df <- pipeline$df_nascita_non_trovati()
                        openxlsx::write.xlsx(df, file)
                }
        )
        
        # ---- Outputs for Provenienze tab ----
        
        output$ui_provenienze <- renderUI({
                req(pipeline$animali_provenienza_non_indenni())
                liste_malattie <- pipeline$animali_provenienza_non_indenni()
                
                if (length(liste_malattie) == 0) {
                        return(div(
                                class = "alert alert-info",
                                p("Nessuna movimentazione proveniente da zone non indenni per le malattie considerate")
                        ))
                }
                
                # Create UI elements for each disease
                ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        div(
                                h4(nome_malattia),
                                DT::DTOutput(output_id),
                                downloadButton(download_id, paste("Scarica", nome_malattia)),
                                hr()
                        )
                })
                
                do.call(tagList, ui_elements)
        })
        
        # Dynamically create output renderers for provenance tables
        observe({
                req(pipeline$animali_provenienza_non_indenni())
                liste_malattie <- pipeline$animali_provenienza_non_indenni()
                
                lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        # Create table output
                        output[[output_id]] <- DT::renderDT({
                                df <- liste_malattie[[nome_malattia]]
                                DT::datatable(
                                        df,
                                        options = list(
                                                pageLength = 10,
                                                scrollX = TRUE
                                        ),
                                        rownames = FALSE
                                )
                        })
                        
                        # Create download handler
                        output[[download_id]] <- downloadHandler(
                                filename = function() {
                                        paste0("provenienza_", 
                                               gsub("[^a-zA-Z0-9]", "_", nome_malattia), 
                                               "_", 
                                               format(Sys.Date(), "%Y%m%d"), 
                                               ".xlsx")
                                },
                                content = function(file) {
                                        df <- liste_malattie[[nome_malattia]]
                                        openxlsx::write.xlsx(df, file)
                                }
                        )
                })
        })
        
        # ---- Outputs for Nascite tab ----
        
        output$ui_nascite <- renderUI({
                req(pipeline$animali_nascita_non_indenni())
                liste_malattie <- pipeline$animali_nascita_non_indenni()
                
                if (length(liste_malattie) == 0) {
                        return(div(
                                class = "alert alert-info",
                                p("Nessuna movimentazione di animali nati in zone non indenni per le malattie considerate")
                        ))
                }
                
                # Create UI elements for each disease
                ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        div(
                                h4(nome_malattia),
                                DT::DTOutput(output_id),
                                downloadButton(download_id, paste("Scarica", nome_malattia)),
                                hr()
                        )
                })
                
                do.call(tagList, ui_elements)
        })
        
        # Dynamically create output renderers for birth tables
        observe({
                req(pipeline$animali_nascita_non_indenni())
                liste_malattie <- pipeline$animali_nascita_non_indenni()
                
                lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        # Create table output
                        output[[output_id]] <- DT::renderDT({
                                df <- liste_malattie[[nome_malattia]]
                                DT::datatable(
                                        df,
                                        options = list(
                                                pageLength = 10,
                                                scrollX = TRUE
                                        ),
                                        rownames = FALSE
                                )
                        })
                        
                        # Create download handler
                        output[[download_id]] <- downloadHandler(
                                filename = function() {
                                        paste0("nascita_", 
                                               gsub("[^a-zA-Z0-9]", "_", nome_malattia), 
                                               "_", 
                                               format(Sys.Date(), "%Y%m%d"), 
                                               ".xlsx")
                                },
                                content = function(file) {
                                        df <- liste_malattie[[nome_malattia]]
                                        openxlsx::write.xlsx(df, file)
                                }
                        )
                })
        })

        
}
