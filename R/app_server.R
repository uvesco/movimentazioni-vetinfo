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
        file_name <- upload$file_name                # Nome file caricato

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

        crea_lotto_id <- function(df) {
                if (is.null(df) || nrow(df) == 0) {
                        return(character(0))
                }
                n_righe <- nrow(df)
                normalizza_valore <- function(x) {
                        if (is.list(x)) {
                                x <- vapply(x, function(item) {
                                        if (length(item) == 0 || all(is.na(item))) {
                                                return("")
                                        }
                                        as.character(item[1])
                                }, character(1))
                        } else {
                                x <- as.character(x)
                        }
                        x[is.na(x)] <- ""
                        rep_len(x, n_righe)
                }
                valori <- lapply(
                        list(
                                dest_stabilimento_cod = df$dest_stabilimento_cod,
                                orig_stabilimento_cod = df$orig_stabilimento_cod,
                                ingresso_data = df$ingresso_data,
                                ingresso_motivo = df$ingresso_motivo
                        ),
                        normalizza_valore
                )
                do.call(paste, c(valori, sep = "|"))
        }
        
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
                        # Inserisce tab "Sommario" dopo "input"
                        insertTab(
                                inputId = "tabs", 
                                target = "input", 
                                position = "after",
                        tab = tabPanel(
                                        title = "Sommario", 
                                        value = "sommario",
                                        h3("Provenienza"),
                                        h4("Internazionali"),
                                        tableOutput("sommario_internazionali"),
                                        h4("Regioni"),
                                        tableOutput("sommario_provenienze_regioni"),
                                        h4("Province"),
                                        tableOutput("sommario_provenienze_province"),
                                        hr(),
                                        h3("Nascita"),
                                        h4("Paesi"),
                                        tableOutput("sommario_nascita_paesi"),
                                        h4("Province"),
                                        tableOutput("sommario_nascita_province"),
                                        hr(),
                                        h3("Destinazioni"),
                                        tableOutput("sommario_importatori")
                                )
                        )

                        # Inserisce tab "Dataset" dopo "sommario"
                        # Mostra il dataset completo (animali + malattie merge)
                        insertTab(
                                inputId = "tabs", 
                                target = "sommario", 
                                position = "after",
                                tab = tabPanel(
                                        title = "Dataset", 
                                        value = "dataset",
                                        h3("Dataset completo movimentazioni"),
                                        p("Download del dataset elaborato in formato Excel."),
                                        downloadButton("download_dataset", "Scarica dataset elaborato (.xlsx)")
                                )
                        )

                        tabs_inserite(TRUE)

                } else if (nrow(animali()) == 0 && tabs_inserite()) {
                        # Se file vuoto, rimuove i tab
                        removeTab("tabs", "sommario")
                        removeTab("tabs", "dataset")
                        tabs_inserite(FALSE)
                }
        })
        
        # Osservatore per inserimento tab "Non Indenni" (unione di Provenienze e Nascite)
        observe({
                req(animali())
                req(gruppo())
                req(tabs_inserite())  # Aspetta che i tab base siano inseriti
                
                if (nrow(animali()) > 0 && !tabs_disease_inserite() && isTRUE(file_check())) {
                        # Inserisce tab "Non Indenni" dopo "dataset"
                        insertTab(
                                inputId = "tabs",
                                target = "dataset",
                                position = "after",
                                tab = tabPanel(
                                        title = "Non Indenni",
                                        value = "non_indenni",
                                        
                                        # A) Diagnostica riepilogativa in cima
                                        uiOutput("ui_diagnostica_non_indenni"),
                                        
                                        hr(),
                                        
                                        # B) Tabella riepilogativa per stabilimento destinazione
                                        h3("Riepilogo per stabilimento di destinazione"),
                                        p("Numero di animali a rischio per ogni stabilimento di destinazione, suddivisi per malattia e tipo di rischio (provenienza/nascita)."),
                                        tableOutput("tabella_riepilogo_non_indenni"),
                                        
                                        # Pulsante BDN per tutti gli animali a rischio
                                        uiOutput("ui_bdn_non_indenni_all"),
                                        
                                        hr(),
                                        
                                        # C.1) Sezione Provenienze con download buttons
                                        h3("Animali provenienti da zone non indenni"),
                                        p("Scarica il dataset filtrato per gli animali provenienti da comuni/zone non indenni per ciascuna malattia."),
                                        uiOutput("ui_provenienze_download"),
                                        
                                        hr(),
                                        
                                        # C.2) Sezione Nascite con download buttons
                                        h3("Animali nati in zone non indenni"),
                                        p("Scarica il dataset filtrato per gli animali nati in province non indenni per ciascuna malattia."),
                                        uiOutput("ui_nascite_download")
                                )
                        )
                        
                        tabs_disease_inserite(TRUE)
                        
                } else if ((is.null(animali()) || nrow(animali()) == 0) && tabs_disease_inserite()) {
                        removeTab("tabs", "non_indenni")
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
								
								# Pulsante BDN per esportazione controllo manuale
								uiOutput("ui_bdn_controllo_manuale"),
								
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
        # SEZIONE 4: OUTPUT SOMMARIO
        # =====================================================================
        
        output$sommario_internazionali <- renderTable({
                df <- pipeline$dati_processati()
                req(df)
                
                lot_id <- crea_lotto_id(df)
                is_italia <- df$orig_italia == TRUE
                is_estero <- df$orig_italia == FALSE
                
                animali_italia <- sum(is_italia, na.rm = TRUE)
                animali_estero <- sum(is_estero, na.rm = TRUE)
                animali_tot <- nrow(df)
                
                lotti_italia <- length(unique(lot_id[is_italia & !is.na(is_italia)]))
                lotti_estero <- length(unique(lot_id[is_estero & !is.na(is_estero)]))
                lotti_tot <- length(unique(lot_id))
                
                data.frame(
                        Categoria = c("Italia", "Estero", "Totale"),
                        Lotti = c(lotti_italia, lotti_estero, lotti_tot),
                        Animali = c(animali_italia, animali_estero, animali_tot),
                        check.names = FALSE
                )
        }, rownames = FALSE)
        
        output$sommario_provenienze_regioni <- renderTable({
                df <- pipeline$dati_processati()
                req(df)
                
                lot_id <- crea_lotto_id(df)
                regioni <- sort(unique(df_regioni$DEN_REG))
                animali <- vapply(regioni, function(reg) {
                        idx <- !is.na(df$orig_reg_nome) & df$orig_reg_nome == reg
                        sum(idx)
                }, integer(1))
                lotti <- vapply(regioni, function(reg) {
                        idx <- !is.na(df$orig_reg_nome) & df$orig_reg_nome == reg
                        length(unique(lot_id[idx]))
                }, integer(1))
                
                data.frame(
                        Regione = regioni,
                        Lotti = lotti,
                        Animali = animali,
                        check.names = FALSE
                )
        }, rownames = FALSE)
        
        output$sommario_provenienze_province <- renderTable({
                df <- pipeline$dati_processati()
                req(df)
                
                lot_id <- crea_lotto_id(df)
                province <- sort(unique(na.omit(df$orig_uts_nome)))
                if (length(province) == 0) {
                        return(data.frame())
                }
                
                regioni <- tapply(df$orig_reg_nome, df$orig_uts_nome, function(x) {
                        val <- unique(na.omit(x))
                        if (length(val) == 0) NA else val[1]
                })
                
                animali <- vapply(province, function(prov) {
                        idx <- !is.na(df$orig_uts_nome) & df$orig_uts_nome == prov
                        sum(idx)
                }, integer(1))
                lotti <- vapply(province, function(prov) {
                        idx <- !is.na(df$orig_uts_nome) & df$orig_uts_nome == prov
                        length(unique(lot_id[idx]))
                }, integer(1))
                
                risultato <- data.frame(
                        Regione = unname(regioni[province]),
                        Provincia = province,
                        Lotti = lotti,
                        Animali = animali,
                        check.names = FALSE
                )
                
                risultato[order(-risultato$Animali), , drop = FALSE]
        }, rownames = FALSE)
        
        output$sommario_nascita_paesi <- renderTable({
                df <- pipeline$dati_processati()
                req(df)
                
                totale_animali <- nrow(df)
                codici <- toupper(substr(as.character(df$capo_identificativo), 1, 2))
                codici[is.na(codici) | nchar(codici) < 2] <- "N/D"
                nomi <- df_stati$Descrizione[match(codici, df_stati$Codice)]
                paesi <- ifelse(!is.na(nomi), nomi, codici)
                
                conteggi <- sort(table(paesi), decreasing = TRUE)
                if (length(conteggi) == 0) {
                        return(data.frame())
                }
                percentuali <- if (totale_animali > 0) round(100 * conteggi / totale_animali, 1) else 0
                
                data.frame(
                        Paese = names(conteggi),
                        Animali = as.integer(conteggi),
                        Percentuale = paste0(percentuali, "%"),
                        check.names = FALSE
                )
        }, rownames = FALSE)
        
        output$sommario_nascita_province <- renderTable({
                df <- pipeline$dati_processati()
                req(df)
                
                totale_animali <- nrow(df)
                province <- df$nascita_uts_nome
                province <- province[!is.na(province)]
                conteggi <- sort(table(province), decreasing = TRUE)
                if (length(conteggi) == 0) {
                        return(data.frame())
                }
                percentuali <- if (totale_animali > 0) round(100 * conteggi / totale_animali, 1) else 0
                
                data.frame(
                        Provincia = names(conteggi),
                        Animali = as.integer(conteggi),
                        Percentuale = paste0(percentuali, "%"),
                        check.names = FALSE
                )
        }, rownames = FALSE)
        
        output$sommario_importatori <- renderTable({
                df <- pipeline$dati_processati()
                req(df)
                
                lot_id <- crea_lotto_id(df)
                dest_cod <- as.character(df$dest_stabilimento_cod)
                dest_com <- as.character(df$dest_com)
                dest_cod[is.na(dest_cod)] <- "N/D"
                dest_com[is.na(dest_com)] <- "N/D"
                dest_key <- paste(dest_cod, dest_com, sep = "|")
                
                gruppi <- split(seq_len(nrow(df)), dest_key)
                righe <- lapply(gruppi, function(idx) {
                        totale <- length(idx)
                        estero <- sum(df$orig_italia[idx] == FALSE, na.rm = TRUE)
                        lotti <- length(unique(lot_id[idx]))
                        percentuale <- if (totale > 0) round(100 * estero / totale, 1) else 0
                        data.frame(
                                `Codice destinazione` = dest_cod[idx][1],
                                `Comune destinazione` = dest_com[idx][1],
                                Lotti = lotti,
                                Animali = totale,
                                `Percentuale da estero` = paste0(percentuale, "%"),
                                check.names = FALSE
                        )
                })
                
                risultato <- do.call(rbind, righe)
                risultato[order(-risultato$Animali), , drop = FALSE]
        }, rownames = FALSE)
        
        # =====================================================================
        # SEZIONE 5: OUTPUT - DATASET COMPLETO
        # =====================================================================
        # Mostra il dataset completo con tutti i dati animali e malattie
        # Questo è il merge finale: animali + status sanitario provenienza + nascita
        
        output$download_dataset <- downloadHandler(
                filename = function() {
                        nome <- file_name()
                        if (is.null(nome) || is.na(nome) || nome == "") {
                                return(paste0("movimentazioni_elab_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
                        }
                        nome <- sub("\\.[^.]+$", "", nome)
                        paste0(nome, "_elab.xlsx")
                },
                content = function(file) {
                        df <- pipeline$dati_processati()
                        if (is.null(df) || nrow(df) == 0) {
                                shiny::showNotification(
                                        "Nessun dato disponibile per il download.",
                                        type = "warning",
                                        duration = 6,
                                        session = session
                                )
                                stop("Nessun dato disponibile per il download.")
                        }
                        openxlsx::write.xlsx(df, file)
                }
        )

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
                
                # data_inizio <- NA
                # data_fine <- NA
                # if ("ingresso_data" %in% names(df)) {
                #         date_vals <- parse_ingresso_date(df$ingresso_data)
                #         valid_dates <- !is.na(date_vals)
                #         if (any(valid_dates)) {
                #                 years <- suppressWarnings(as.integer(format(date_vals, "%Y")))
                #                 valid_dates <- valid_dates & !is.na(years) & years >= 1900 & years <= 2100
                #         }
                #         if (any(valid_dates)) {
                #                 data_inizio <- format(min(date_vals[valid_dates]), "%d/%m/%Y")
                #                 data_fine <- format(max(date_vals[valid_dates]), "%d/%m/%Y")
                #         }
                # }
                # data_inizio_label <- ifelse(is.na(data_inizio), "N/D", data_inizio)
                # data_fine_label <- ifelse(is.na(data_fine), "N/D", data_fine)
                data_inizio <- min(as.Date(df$ingresso_data, format = "%d/%m/%Y"), na.rm = TRUE)
                data_fine <- max(as.Date(df$ingresso_data, format = "%d/%m/%Y"), na.rm = TRUE)
                data_inizio_label <- ifelse(is.finite(data_inizio), format(data_inizio, "%d/%m/%Y"), "N/D")
                data_fine_label <- ifelse(is.finite(data_fine), format(data_fine, "%d/%m/%Y"), "N/D")
                
                div(
                        bs_icon("info-circle-fill"), em("Informazioni"), br(),
                        h4("Periodo"),
                        "Data inizio: ", data_inizio_label, br(),
                        "Data fine: ", data_fine_label, br(),
                        h4("Animali movimentati"),
                        "Gruppo specie: ", grp, br(),
                        "Animali movimentati: ", nrow(df)
                )
        })
        
        # Titolo sezione malattie (mostrato dopo caricamento)
        output$titolo_malattie <- renderUI({
                grp <- gruppo()
                req(grp)
                h4("Malattie considerate per il gruppo specie")
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
        # SEZIONE 7.5: BDN EXPORT PER CONTROLLO MANUALE
        # =====================================================================
        # Pulsante per scaricare i codici degli animali con dati non mappabili
        
        output$ui_bdn_controllo_manuale <- renderUI({
                req(pipeline$df_provenienza_non_trovati)
                req(pipeline$df_nascita_non_trovati)
                
                df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
                df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
                
                # Verifica se ci sono animali da esportare
                ha_animali <- (nrow(df_prov) > 0 || nrow(df_nasc) > 0)
                
                if (!ha_animali) {
                        return(NULL)
                }
                
                # Conta totale animali unici
                codici_prov <- if (nrow(df_prov) > 0 && "capo_identificativo" %in% names(df_prov)) {
                        as.character(df_prov$capo_identificativo)
                } else {
                        character(0)
                }
                
                codici_nasc <- if (nrow(df_nasc) > 0 && "capo_identificativo" %in% names(df_nasc)) {
                        as.character(df_nasc$capo_identificativo)
                } else {
                        character(0)
                }
                
                codici_totali <- unique(c(codici_prov, codici_nasc))
                codici_totali <- codici_totali[!is.na(codici_totali) & codici_totali != ""]
                n_animali <- length(codici_totali)
                
                if (n_animali == 0) {
                        return(NULL)
                }
                
                # Crea il pulsante BDN
                div(
                        style = "margin: 20px 0; padding: 15px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;",
                        h4("Esportazione per BDN - Interrogazione \"Capi da file\""),
                        p("Scarica i codici identificativi di tutti gli animali con dati geografici non mappabili ",
                          "(sia codice stabilimento che provincia marchio auricolare)."),
                        p(strong(paste0("Totale animali: ", n_animali))),
                        tags$ul(
                                tags$li("Codifica ANSI (Windows-1252) con interruzioni di linea Windows (CRLF)"),
                                tags$li("Un codice per riga, massimo 255 per file"),
                                tags$li("≤255 animali: download diretto file .txt"),
                                tags$li(">255 animali: download file .zip con multipli .txt")
                        ),
                        downloadButton("download_bdn_controllo_manuale", "Scarica per BDN", 
                                       icon = icon("download"),
                                       class = "btn-warning")
                )
        })
        
        # Download handler per BDN controllo manuale
        output$download_bdn_controllo_manuale <- downloadHandler(
                filename = function() {
                        req(pipeline$df_provenienza_non_trovati)
                        req(pipeline$df_nascita_non_trovati)
                        
                        df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
                        df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
                        
                        # Combina i codici
                        codici_prov <- if (nrow(df_prov) > 0 && "capo_identificativo" %in% names(df_prov)) {
                                as.character(df_prov$capo_identificativo)
                        } else {
                                character(0)
                        }
                        
                        codici_nasc <- if (nrow(df_nasc) > 0 && "capo_identificativo" %in% names(df_nasc)) {
                                as.character(df_nasc$capo_identificativo)
                        } else {
                                character(0)
                        }
                        
                        codici_totali <- unique(c(codici_prov, codici_nasc))
                        codici_totali <- codici_totali[!is.na(codici_totali) & codici_totali != ""]
                        n_animali <- length(codici_totali)
                        
                        if (n_animali <= 255) {
                                # File singolo .txt
                                paste0("bdn_controllo_manuale_", format(Sys.Date(), "%Y%m%d"), ".txt")
                        } else {
                                # File ZIP con multipli .txt
                                paste0("bdn_export_controllo_manuale_", format(Sys.Date(), "%Y%m%d"), ".zip")
                        }
                },
                content = function(file) {
                        req(pipeline$df_provenienza_non_trovati)
                        req(pipeline$df_nascita_non_trovati)
                        
                        df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
                        df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
                        
                        tryCatch({
                                # Combina i due dataframe in una lista fittizia per usare le funzioni esistenti
                                # Creiamo una struttura simile a liste_malattie
                                lista_combined <- list()
                                
                                if (nrow(df_prov) > 0) {
                                        lista_combined[["Codice_stabilimento_non_mappabile"]] <- df_prov
                                }
                                
                                if (nrow(df_nasc) > 0) {
                                        lista_combined[["Provincia_marchio_non_mappabile"]] <- df_nasc
                                }
                                
                                if (length(lista_combined) == 0) {
                                        stop("Nessun animale da esportare")
                                }
                                
                                # Conta animali per determinare il tipo di esportazione
                                n_animali <- conta_animali_da_esportare(lista_combined)
                                
                                if (n_animali <= 255) {
                                        # Crea file .txt singolo per download diretto
                                        txt_path <- crea_txt_bdn_export(lista_combined, tipo = "controllo_manuale")
                                        file.copy(txt_path, file, overwrite = TRUE)
                                        unlink(txt_path)
                                } else {
                                        # Crea file ZIP con multipli .txt
                                        zip_path <- crea_zip_bdn_export(lista_combined, tipo = "controllo_manuale")
                                        file.copy(zip_path, file, overwrite = TRUE)
                                        unlink(zip_path)
                                }
                        }, error = function(e) {
                                showNotification(
                                        paste("Errore nella creazione del file:", e$message),
                                        type = "error",
                                        duration = 10
                                )
                        })
                }
        )
        
        # =====================================================================
        # SEZIONE 8: OUTPUT NON INDENNI (TAB UNIFICATO)
        # =====================================================================
        # A) Diagnostica riepilogativa: verde se nessun animale a rischio, rosso con conteggi
        
        output$ui_diagnostica_non_indenni <- renderUI({
                prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
                nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
                
                # Conta animali unici per categoria
                conta_animali <- function(lista) {
                        if (length(lista) == 0) return(0)
                        ids <- unlist(lapply(lista, function(df_item) {
                                if (!"capo_identificativo" %in% names(df_item)) return(character(0))
                                df_item$capo_identificativo
                        }))
                        ids <- ids[!is.na(ids)]
                        length(unique(ids))
                }
                
                n_prov <- conta_animali(prov_non_indenni)
                n_nasc <- conta_animali(nasc_non_indenni)
                
                # Se non ci sono animali a rischio
                if (n_prov == 0 && n_nasc == 0) {
                        return(div(
                                class = "alert alert-success",
                                role = "alert",
                                icon("check-circle"),
                                strong(" Nessun animale a rischio: "),
                                "Non sono presenti animali provenienti o nati in zone non indenni per le malattie considerate."
                        ))
                }
                
                # Altrimenti mostra alert rosso con conteggi
                tagList(
                        if (n_prov > 0) {
                                div(
                                        class = "alert alert-danger",
                                        role = "alert",
                                        icon("exclamation-triangle"),
                                        strong(paste(" Animali provenienti da zone non indenni:", n_prov)),
                                        " - Controllare la sezione 'Provenienze' per i dettagli."
                                )
                        },
                        if (n_nasc > 0) {
                                div(
                                        class = "alert alert-danger",
                                        role = "alert",
                                        icon("exclamation-triangle"),
                                        strong(paste(" Animali nati in zone non indenni:", n_nasc)),
                                        " - Controllare la sezione 'Nascite' per i dettagli."
                                )
                        }
                )
        })
        
        # B) Tabella riepilogativa per stabilimento destinazione
        output$tabella_riepilogo_non_indenni <- renderTable({
                req(pipeline$dati_processati())
                req(malattie())
                req(gruppo())
                
                df <- pipeline$dati_processati()
                grp <- gruppo()
                malattie_data <- malattie()
                
                prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
                nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
                
                # Se non ci sono animali a rischio
                if (length(prov_non_indenni) == 0 && length(nasc_non_indenni) == 0) {
                        return(data.frame())
                }
                
                # Ottieni metadati malattie per i nomi dei campi
                df_meta <- malattie_data[["metadati"]]
                malattie_gruppo <- df_meta[df_meta$specie == grp, ]
                
                # Prepara la lista di tutti gli animali a rischio
                animali_rischio <- data.frame()
                
                # Aggiungi animali da provenienze
                for (nome_malattia in names(prov_non_indenni)) {
                        df_mal <- prov_non_indenni[[nome_malattia]]
                        if (nrow(df_mal) > 0) {
                                # Trova il nome del campo dalla metadati
                                campo_raw <- malattie_gruppo$campo[malattie_gruppo$malattia == nome_malattia]
                                # Rimuovi prefisso Ind_ se presente
                                campo_clean <- sub("^Ind_", "", campo_raw)
                                col_name <- paste0("prov_", campo_clean)
                                
                                temp <- df_mal[, c("capo_identificativo", "dest_stabilimento_cod"), drop = FALSE]
                                temp$tipo <- "provenienza"
                                temp$malattia <- nome_malattia
                                temp$campo <- col_name
                                animali_rischio <- rbind(animali_rischio, temp)
                        }
                }
                
                # Aggiungi animali da nascite
                for (nome_malattia in names(nasc_non_indenni)) {
                        df_mal <- nasc_non_indenni[[nome_malattia]]
                        if (nrow(df_mal) > 0) {
                                # Trova il nome del campo dalla metadati
                                campo_raw <- malattie_gruppo$campo[malattie_gruppo$malattia == nome_malattia]
                                # Rimuovi prefisso Ind_ se presente
                                campo_clean <- sub("^Ind_", "", campo_raw)
                                col_name <- paste0("nascita_", campo_clean)
                                
                                temp <- df_mal[, c("capo_identificativo", "dest_stabilimento_cod"), drop = FALSE]
                                temp$tipo <- "nascita"
                                temp$malattia <- nome_malattia
                                temp$campo <- col_name
                                animali_rischio <- rbind(animali_rischio, temp)
                        }
                }
                
                if (nrow(animali_rischio) == 0) {
                        return(data.frame())
                }
                
                # Crea tabella riepilogativa per stabilimento
                # Colonne dinamiche per ogni combinazione tipo/malattia
                campi_unici <- unique(animali_rischio$campo)
                
                # Raggruppa per stabilimento e conta per ogni campo
                stab_list <- split(animali_rischio, animali_rischio$dest_stabilimento_cod)
                
                result <- lapply(names(stab_list), function(stab_cod) {
                        stab_data <- stab_list[[stab_cod]]
                        row <- data.frame(dest_stab_cod = stab_cod, stringsAsFactors = FALSE)
                        
                        for (campo in campi_unici) {
                                count <- sum(stab_data$campo == campo)
                                row[[campo]] <- count
                        }
                        
                        # Calcola totale
                        row$totale <- nrow(stab_data)
                        row
                })
                
                result_df <- do.call(rbind, result)
                
                # Ordina per totale decrescente
                result_df <- result_df[order(-result_df$totale), , drop = FALSE]
                
                result_df
        }, rownames = FALSE, digits = 0)
        
        # B) Pulsante BDN per tutti gli animali a rischio (provenienze + nascite)
        output$ui_bdn_non_indenni_all <- renderUI({
                prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
                nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
                
                # Se non ci sono animali da esportare
                if (length(prov_non_indenni) == 0 && length(nasc_non_indenni) == 0) {
                        return(NULL)
                }
                
                # Conta totale animali unici
                codici <- character(0)
                for (df_item in c(prov_non_indenni, nasc_non_indenni)) {
                        if (!is.null(df_item) && "capo_identificativo" %in% names(df_item)) {
                                codici <- c(codici, as.character(df_item$capo_identificativo))
                        }
                }
                codici <- unique(codici[!is.na(codici) & codici != ""])
                n_animali <- length(codici)
                
                if (n_animali == 0) return(NULL)
                
                div(
                        style = "margin: 20px 0; padding: 15px; background-color: #f0f8ff; border: 1px solid #4682b4; border-radius: 5px;",
                        h4("Esportazione per BDN - Interrogazione \"Capi da file\""),
                        p("Scarica i codici identificativi di ", strong("tutti"), " gli animali a rischio (provenienti o nati in zone non indenni per qualsiasi malattia), ",
                          "formattati per il caricamento nell'interrogazione \"Capi da file\" BDN."),
                        p(strong(paste0("Totale animali: ", n_animali))),
                        tags$ul(
                                tags$li("Codifica ANSI (Windows-1252) con interruzioni di linea Windows (CRLF)"),
                                tags$li("Un codice per riga, massimo 255 per file"),
                                tags$li("≤255 animali: download diretto file .txt"),
                                tags$li(">255 animali: download file .zip con multipli .txt")
                        ),
                        downloadButton("download_bdn_non_indenni_all", "Scarica per BDN (tutti)", 
                                       icon = icon("download"),
                                       class = "btn-primary")
                )
        })
        
        # Download handler per BDN tutti gli animali a rischio
        output$download_bdn_non_indenni_all <- downloadHandler(
                filename = function() {
                        prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
                        nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
                        
                        # Combina le liste
                        combined_list <- c(prov_non_indenni, nasc_non_indenni)
                        n_animali <- conta_animali_da_esportare(combined_list)
                        
                        if (n_animali <= 255) {
                                paste0("bdn_non_indenni_", format(Sys.Date(), "%Y%m%d"), ".txt")
                        } else {
                                paste0("bdn_export_non_indenni_", format(Sys.Date(), "%Y%m%d"), ".zip")
                        }
                },
                content = function(file) {
                        prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
                        nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
                        
                        tryCatch({
                                combined_list <- c(prov_non_indenni, nasc_non_indenni)
                                n_animali <- conta_animali_da_esportare(combined_list)
                                
                                if (n_animali <= 255) {
                                        txt_path <- crea_txt_bdn_export(combined_list, tipo = "non_indenni")
                                        file.copy(txt_path, file, overwrite = TRUE)
                                        unlink(txt_path)
                                } else {
                                        zip_path <- crea_zip_bdn_export(combined_list, tipo = "non_indenni")
                                        file.copy(zip_path, file, overwrite = TRUE)
                                        unlink(zip_path)
                                }
                        }, error = function(e) {
                                showNotification(
                                        paste("Errore nella creazione del file:", e$message),
                                        type = "error",
                                        duration = 10
                                )
                        })
                }
        )
        
        # C.1) Sezione Provenienze - Solo download buttons
        output$ui_provenienze_download <- renderUI({
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
                
                # Crea solo download buttons per ogni malattia (senza tabella)
                ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
                        download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        n_animali <- nrow(liste_malattie[[nome_malattia]])
                        
                        div(
                                style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                                div(
                                        style = "display: flex; justify-content: space-between; align-items: center;",
                                        span(strong(nome_malattia), " - ", n_animali, " animali"),
                                        downloadButton(download_id, "Scarica Excel", class = "btn-sm btn-outline-primary")
                                )
                        )
                })
                
                do.call(tagList, ui_elements)
        })
        
        # Crea dinamicamente i download per ogni malattia (provenienze)
        observe({
                req(pipeline$animali_provenienza_non_indenni())
                liste_malattie <- pipeline$animali_provenienza_non_indenni()
                
                lapply(names(liste_malattie), function(nome_malattia) {
                        download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
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
        
        # C.2) Sezione Nascite - Solo download buttons
        output$ui_nascite_download <- renderUI({
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
                
                # Crea solo download buttons per ogni malattia (senza tabella)
                ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
                        download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        n_animali <- nrow(liste_malattie[[nome_malattia]])
                        
                        div(
                                style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                                div(
                                        style = "display: flex; justify-content: space-between; align-items: center;",
                                        span(strong(nome_malattia), " - ", n_animali, " animali"),
                                        downloadButton(download_id, "Scarica Excel", class = "btn-sm btn-outline-primary")
                                )
                        )
                })
                
                do.call(tagList, ui_elements)
        })
        
        # Crea dinamicamente i download per ogni malattia (nascite)
        observe({
                req(pipeline$animali_nascita_non_indenni())
                liste_malattie <- pipeline$animali_nascita_non_indenni()
                
                lapply(names(liste_malattie), function(nome_malattia) {
                        download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
                        
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
