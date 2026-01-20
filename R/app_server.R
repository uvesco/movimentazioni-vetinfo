# =============================================================================
# SERVER PRINCIPALE DELL'APPLICAZIONE
# =============================================================================
# Questo file contiene la logica server dell'applicazione Shiny.
# 
# FLUSSO DATI:
# 1. Upload file movimentazioni → mod_upload_movimentazioni_server
# 2. Importazione dati malattie → mod_import_malattie
# 3. Pipeline controlli → mod_pipeline_controlli_server
# 4. Generazione tab dinamici e outputs
# 
# TAB DINAMICI:
# - Elaborazione/Output: sempre mostrati dopo caricamento file valido
# - Controllo Manuale: mostrato solo se ci sono animali con dati non validi
# - Provenienze/Nascite: mostrati dopo caricamento file valido
# =============================================================================

app_server <- function(input, output, session) {
        
        # =====================================================================
        # SEZIONE 1: IMPORTAZIONE DATI
        # =====================================================================
        # Questa sezione gestisce il caricamento e la standardizzazione dei 
        # file movimentazioni e l'importazione dei dati sulle malattie
        
        # Modulo upload: gestisce il caricamento file e la standardizzazione
        upload <- mod_upload_movimentazioni_server("upload_mov")
        animali <- upload$animali                    # Dataframe animali standardizzato
        gruppo <- upload$gruppo                      # Gruppo specie (bovini/ovicaprini)
        stato_upload <- upload$status                # Stato caricamento per messaggi
        file_check <- upload$file_check              # Verifica validità file (colonne)

        # Modulo malattie: genera i dataframe con status sanitario per provincia/comune
        # Importa tutti i file .xlsx dalla cartella data_static/malattie
        # TODO: aggiungere caricamento manuale di file extra da parte dell'utente
        malattie <- mod_import_malattie("df_standard", gruppo)
        st_import <- malattie
        
        # =====================================================================
        # SEZIONE 2: PIPELINE CONTROLLI
        # =====================================================================
        # Questa sezione esegue tutti i controlli sugli animali:
        # - Classificazione Italia/Estero
        # - Estrazione provincia nascita e comune provenienza
        # - Merge con dati malattie
        # - Identificazione animali con dati non validi o da zone non indenni
        
        pipeline <- mod_pipeline_controlli_server(
                "pipeline",
                animali = animali,                   # Dataframe animali caricato
                gruppo = gruppo,                     # Gruppo specie corrente
                malattie_data = malattie             # Dati malattie per il merge
        )
        
        # =====================================================================
        # SEZIONE 3: GESTIONE TAB DINAMICI
        # =====================================================================
        # I tab vengono inseriti/rimossi dinamicamente in base ai dati caricati:
        # - Elaborazione: info sul gruppo specie (sempre dopo upload)
        # - Output: dataset completo scaricabile (sempre dopo upload)
        # - Provenienze: animali da zone non indenni (sempre dopo upload)
        # - Nascite: animali nati in zone non indenni (sempre dopo upload)
        # - Controllo Manuale: solo se ci sono animali con dati non validi
        
        # Variabili reattive per tracciare lo stato dei tab inseriti
        tabs_inserite <- reactiveVal(FALSE)          # Tab base (Elaborazione/Output)
        tabs_disease_inserite <- reactiveVal(FALSE)  # Tab malattie (Provenienze/Nascite)
        tab_controllo_inserita <- reactiveVal(FALSE) # Tab Controllo Manuale
        
        # Osservatore per inserimento tab base dopo caricamento file valido
        observe({
                req(animali())
                req(gruppo())
                
                if (nrow(animali()) > 0 && !tabs_inserite() && isTRUE(file_check())) {
                        # Inserisce tab "Elaborazione" dopo "input"
                        insertTab(
                                inputId = "tabs", 
                                target = "input", 
                                position = "after",
                                tab = tabPanel(
                                        title = "Elaborazione", 
                                        value = "elaborazione",
                                        h3("Informazioni elaborazione"),
                                        textOutput("gruppo_tab"),
                                        hr(),
                                        p("I dati sono stati elaborati correttamente.")
                                )
                        )

                        # Inserisce tab "Output" dopo "elaborazione"
                        # Mostra il dataset completo (animali + malattie merge)
                        insertTab(
                                inputId = "tabs", 
                                target = "elaborazione", 
                                position = "after",
                                tab = tabPanel(
                                        title = "Output", 
                                        value = "output",
                                        h3("Dataset completo movimentazioni"),
                                        p("Tabella con tutti i dati animali e lo stato sanitario delle zone di provenienza e nascita."),
                                        DT::DTOutput("tabella_output")
                                )
                        )

                        tabs_inserite(TRUE)

                } else if (nrow(animali()) == 0 && tabs_inserite()) {
                        # Se file vuoto, rimuove i tab
                        removeTab("tabs", "elaborazione")
                        removeTab("tabs", "output")
                        tabs_inserite(FALSE)
                }
        })
        
        # Osservatore per inserimento tab Provenienze e Nascite dopo caricamento
        observe({
                req(animali())
                req(gruppo())
                req(tabs_inserite())  # Aspetta che i tab base siano inseriti
                
                if (nrow(animali()) > 0 && !tabs_disease_inserite() && isTRUE(file_check())) {
                        # Inserisce tab "Provenienze" dopo "output"
                        insertTab(
                                inputId = "tabs",
                                target = "output",
                                position = "after",
                                tab = tabPanel(
                                        title = "Provenienze",
                                        value = "provenienze",
                                        h3("Animali provenienti da zone non indenni"),
                                        p("Elenco degli animali che provengono da comuni/zone non indenni per le malattie considerate."),
                                        uiOutput("ui_provenienze")
                                )
                        )
                        
                        # Inserisce tab "Nascite" dopo "provenienze"
                        insertTab(
                                inputId = "tabs",
                                target = "provenienze",
                                position = "after",
                                tab = tabPanel(
                                        title = "Nascite",
                                        value = "nascite",
                                        h3("Animali nati in zone non indenni"),
                                        p("Elenco degli animali nati in province non indenni per le malattie considerate."),
                                        uiOutput("ui_nascite")
                                )
                        )
                        
                        tabs_disease_inserite(TRUE)
                        
                } else if ((is.null(animali()) || nrow(animali()) == 0) && tabs_disease_inserite()) {
                        removeTab("tabs", "provenienze")
                        removeTab("tabs", "nascite")
                        tabs_disease_inserite(FALSE)
                }
        })
        
        # Osservatore per inserimento tab Controllo Manuale 
        # (solo se ci sono animali con dati non validi)
        observe({
                # Attende che la pipeline abbia processato i dati
                req(pipeline$df_provenienza_non_trovati)
                req(pipeline$df_nascita_non_trovati)
                
                # Verifica se ci sono animali con dati non validi
                df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
                df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
                
                ha_problemi <- (nrow(df_prov) > 0 || nrow(df_nasc) > 0)
                
                if (ha_problemi && !tab_controllo_inserita() && tabs_inserite()) {
                        # Inserisce tab "Controllo Manuale" dopo "input"
                        insertTab(
                                inputId = "tabs",
                                target = "input",
                                position = "after",
						tab = tabPanel(
								title = "Controllo Manuale",
								value = "controllo_manuale",
								h3("Animali con dati geografici non validi"),
								p("Questa sezione mostra gli animali italiani per cui non è stato 
								   possibile identificare correttamente i dati geografici."),
								hr(),
								
								h4("Animali di provenienza nazionale con codice stabilimento di origine non mappabile"),
								uiOutput("ui_provenienza_non_trovata"),
								
								hr(),
								
								h4("Animali nati in Italia con provincia nel marchio auricolare non mappabile"),
								uiOutput("ui_nascita_non_trovata")
						)
				)
                        tab_controllo_inserita(TRUE)
                        
                } else if (!ha_problemi && tab_controllo_inserita()) {
                        # Rimuove il tab se non ci sono più problemi
                        removeTab("tabs", "controllo_manuale")
                        tab_controllo_inserita(FALSE)
                }
        })

        # =====================================================================
        # SEZIONE 4: OUTPUT ELABORAZIONE
        # =====================================================================
        
        # Mostra il gruppo specie nel tab Elaborazione
        output$gruppo_tab <- renderText({
                req(gruppo())
                paste("Gruppo specie:", gruppo())
        })
        
        # =====================================================================
        # SEZIONE 5: OUTPUT - DATASET COMPLETO
        # =====================================================================
        # Mostra il dataset completo con tutti i dati animali e malattie
        # Questo è il merge finale: animali + status sanitario provenienza + nascita
        
        output$tabella_output <- DT::renderDT({
                # Ottiene i dati processati dalla pipeline (merge completo)
                df <- pipeline$dati_processati()
                req(df)
                
                DT::datatable(
                        df,
                        extensions = "Buttons",
                        options = list(
                                dom = "Bfrtip",
                                buttons = list(list(
                                        extend = "excel",
                                        filename = paste0("movimentazioni_complete_", format(Sys.Date(), "%Y%m%d"))
                                )),
                                pageLength = 10,
                                lengthMenu = c(10, 25, 50, 100),
                                scrollX = TRUE
                        ),
                        filter = "top",
                        rownames = FALSE
                )
        }, server = FALSE)

        # Download debug: dataset completo con tutti i merge (include animali esteri)
        output$download_debug_dataset <- downloadHandler(
                filename = function() {
                        paste0("movimentazioni_complete_debug_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
                },
                content = function(file) {
                        df <- pipeline$dati_processati()
                        if (is.null(df)) {
                                shiny::showNotification(
                                        "Nessun dato disponibile per il download.",
                                        type = "warning",
                                        duration = 6,
                                        session = session
                                )
                                stop("Nessun dato disponibile per il download.")
                        }
                        if (nrow(df) == 0) {
                                shiny::showNotification(
                                        "Nessun dato disponibile per il download.",
                                        type = "warning",
                                        duration = 6,
                                        session = session
                                )
                                stop("Nessun dato disponibile per il download.")
                        }
                        tryCatch(
                                openxlsx::write.xlsx(df, file),
                                error = function(e) {
                                        shiny::showNotification(
                                                paste0("Errore durante l'esportazione del file debug: ", e$message),
                                                type = "error",
                                                duration = 8,
                                                session = session
                                        )
                                        stop(e$message)
                                }
                        )
                }
        )

        # =====================================================================
        # SEZIONE 6: MESSAGGI E INFO TAB INPUT
        # =====================================================================

        # Messaggio informativo sul file caricato
        output$tipo_file <- renderText({
                stato <- stato_upload()
                if (!is.null(stato$message) && stato$type %in% c("error", "empty")) {
                        return(stato$message)
                }

                df <- animali()
                if (is.null(df)) {
                        return("File non ancora caricato")
                }

                tryCatch({
                        grp <- gruppo()
                        req(grp)
                        if (nrow(df) == 0) {
                                return("File vuoto per i parametri selezionati")
                        } else {
                                paste("File importato correttamente per il gruppo", grp)
                        }
                }, error = function(e) {
                        paste("Errore nel file:", e$message)
                })
        })

        # Mostra numero di animali e info gruppo
        output$n_animali <- renderUI({
                df <- animali()
                grp <- gruppo()
                req(df, grp)
                div(
                        bs_icon("info-circle-fill"), em("Informazioni"), br(),
                        h4("Animali movimentati"),
                        "Gruppo specie: ", grp, br(),
                        "Numero di animali importati: ", nrow(df)
                )
        })
        
        # Titolo sezione malattie (mostrato dopo caricamento)
        output$titolo_malattie <- renderUI({
                grp <- gruppo()
                req(grp)
                h4("Malattie rilevanti per il gruppo specie")
        })
        
        # Tabella malattie importate e rilevanti per il gruppo
        output$malattie_importate <- renderTable({
                grp <- gruppo()
                req(grp)
                
                tryCatch({
                        malattie_data <- malattie()
                        if (is.null(malattie_data)) return(NULL)
                        
                        df_malattie <- malattie_data[["metadati"]]
                        if (is.null(df_malattie) || nrow(df_malattie) == 0) return(NULL)
                        
                        # Filtra per il gruppo corrente
                        df_filtrato <- df_malattie[df_malattie$specie == grp, 
                                                   c("malattia", "riferimento", "data_inizio", "data_fine")]
                        
                        # Formatta le date
                        if (nrow(df_filtrato) > 0) {
                                df_filtrato$data_inizio <- format(df_filtrato$data_inizio, "%d/%m/%Y")
                                df_filtrato$data_fine <- format(df_filtrato$data_fine, "%d/%m/%Y")
                        }
                        
                        df_filtrato
                }, error = function(e) {
                        message("Errore in malattie_importate: ", e$message)
                        NULL
                })
        })
        
        # Riepilogo controlli manuali e zone non indenni
        output$riepilogo_controlli <- renderUI({
                df <- animali()
                if (is.null(df)) {
                        return(NULL)
                }
                
                conta_animali <- function(lista) {
                        if (length(lista) == 0) {
                                return(0)
                        }
                        ids <- unlist(lapply(lista, function(df_item) {
                                if (!"capo_identificativo" %in% names(df_item)) {
                                        return(character(0))
                                }
                                df_item$capo_identificativo
                        }))
                        ids <- ids[!is.na(ids)]
                        length(unique(ids))
                }
                
                df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
                df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
                prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
                nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
                
                manuale_nascita <- nrow(df_nasc)
                manuale_provenienza <- nrow(df_prov)
                nati_non_indenni <- conta_animali(nasc_non_indenni)
                provenienti_non_indenni <- conta_animali(prov_non_indenni)
                
                riga_colore <- function(testo, valore) {
                        colore <- ifelse(valore == 0, "green", "red")
                        div(style = paste0("color: ", colore, ";"), paste(testo, valore))
                }
                
                div(
                        h4("Riepilogo controlli"),
                        riga_colore("Animali da controllare manualmente per nascita:", manuale_nascita),
                        riga_colore("Animali da controllare manualmente per provenienza:", manuale_provenienza),
                        riga_colore("Animali nati in province non indenni (per qualsiasi malattia):", nati_non_indenni),
                        riga_colore("Animali provenienti da province non indenni (per qualsiasi malattia):", provenienti_non_indenni)
                )
		})
        
        # =====================================================================
        # SEZIONE 7: OUTPUT CONTROLLO MANUALE
        # =====================================================================
        # Tabelle e download per animali con dati geografici non validi
        
        output$ui_provenienza_non_trovata <- renderUI({
                req(pipeline$df_provenienza_non_trovati())
                df <- pipeline$df_provenienza_non_trovati()
                
                if (nrow(df) == 0) {
                        return(div(style = "color: green;", 
                                   "Non ci sono animali di provenienza nazionale con codice stabilimento di origine non mappabile"))
                }
                
                tagList(
                        DT::DTOutput("tabella_provenienza_non_trovata"),
                        downloadButton("download_provenienza_non_trovata", "Scarica Excel")
                )
        })
        
        # Tabella: animali con comune provenienza non trovato
        output$tabella_provenienza_non_trovata <- DT::renderDT({
                req(pipeline$df_provenienza_non_trovati())
                df <- pipeline$df_provenienza_non_trovati()
                
                if (nrow(df) == 0) return(NULL)
                
                DT::datatable(
                        df,
                        options = list(pageLength = 10, scrollX = TRUE),
                        rownames = FALSE
                )
        })
        
        # Download: animali con comune provenienza non trovato
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
        
        output$ui_nascita_non_trovata <- renderUI({
                req(pipeline$df_nascita_non_trovati())
                df <- pipeline$df_nascita_non_trovati()
                
                if (nrow(df) == 0) {
                        return(div(style = "color: green;", 
                                   "Non ci sono animali nati in Italia con provincia nel marchio auricolare non mappabile"))
                }
                
                tagList(
                        DT::DTOutput("tabella_nascita_non_trovata"),
                        downloadButton("download_nascita_non_trovata", "Scarica Excel")
                )
        })
        
        # Tabella: animali con provincia nascita non trovata
        output$tabella_nascita_non_trovata <- DT::renderDT({
                req(pipeline$df_nascita_non_trovati())
                df <- pipeline$df_nascita_non_trovati()
                
                if (nrow(df) == 0) return(NULL)
                
                DT::datatable(
                        df,
                        options = list(pageLength = 10, scrollX = TRUE),
                        rownames = FALSE
                )
        })
        
        # Download: animali con provincia nascita non trovata
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
        
        # =====================================================================
        # SEZIONE 8: OUTPUT PROVENIENZE
        # =====================================================================
        # Tabelle dinamiche per animali da zone non indenni (per provenienza)
        
        output$ui_provenienze <- renderUI({
                req(pipeline$animali_provenienza_non_indenni())
                liste_malattie <- pipeline$animali_provenienza_non_indenni()
                
                # Se non ci sono animali da zone non indenni
                if (length(liste_malattie) == 0) {
                        return(div(
                                class = "alert alert-success",
                                icon("check-circle"),
                                " Nessuna movimentazione proveniente da zone non indenni per le malattie considerate."
                        ))
                }
                
                # Crea UI per ogni malattia
                ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        div(
                                h4(nome_malattia),
                                p(paste("Animali:", nrow(liste_malattie[[nome_malattia]]))),
                                DT::DTOutput(output_id),
                                downloadButton(download_id, paste("Scarica", nome_malattia)),
                                hr()
                        )
                })
                
                do.call(tagList, ui_elements)
        })
        
        # Crea dinamicamente i render e download per ogni malattia (provenienze)
        observe({
                req(pipeline$animali_provenienza_non_indenni())
                liste_malattie <- pipeline$animali_provenienza_non_indenni()
                
                lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        # Render tabella
                        output[[output_id]] <- DT::renderDT({
                                df <- liste_malattie[[nome_malattia]]
                                DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
                        })
                        
                        # Download handler
                        output[[download_id]] <- downloadHandler(
                                filename = function() {
                                        paste0("provenienza_", gsub("[^a-zA-Z0-9]", "_", nome_malattia), 
                                               "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
                                },
                                content = function(file) {
                                        df <- liste_malattie[[nome_malattia]]
                                        openxlsx::write.xlsx(df, file)
                                }
                        )
                })
        })
        
        # =====================================================================
        # SEZIONE 9: OUTPUT NASCITE
        # =====================================================================
        # Tabelle dinamiche per animali nati in zone non indenni
        
        output$ui_nascite <- renderUI({
                req(pipeline$animali_nascita_non_indenni())
                liste_malattie <- pipeline$animali_nascita_non_indenni()
                
                # Se non ci sono animali nati in zone non indenni
                if (length(liste_malattie) == 0) {
                        return(div(
                                class = "alert alert-success",
                                icon("check-circle"),
                                " Nessuna movimentazione di animali nati in zone non indenni per le malattie considerate."
                        ))
                }
                
                # Crea UI per ogni malattia
                ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        div(
                                h4(nome_malattia),
                                p(paste("Animali:", nrow(liste_malattie[[nome_malattia]]))),
                                DT::DTOutput(output_id),
                                downloadButton(download_id, paste("Scarica", nome_malattia)),
                                hr()
                        )
                })
                
                do.call(tagList, ui_elements)
        })
        
        # Crea dinamicamente i render e download per ogni malattia (nascite)
        observe({
                req(pipeline$animali_nascita_non_indenni())
                liste_malattie <- pipeline$animali_nascita_non_indenni()
                
                lapply(names(liste_malattie), function(nome_malattia) {
                        output_id <- paste0("tabella_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
                        # Render tabella
                        output[[output_id]] <- DT::renderDT({
                                df <- liste_malattie[[nome_malattia]]
                                DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
                        })
                        
                        # Download handler
                        output[[download_id]] <- downloadHandler(
                                filename = function() {
                                        paste0("nascita_", gsub("[^a-zA-Z0-9]", "_", nome_malattia), 
                                               "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
                                },
                                content = function(file) {
                                        df <- liste_malattie[[nome_malattia]]
                                        openxlsx::write.xlsx(df, file)
                                }
                        )
                })
        })

}
