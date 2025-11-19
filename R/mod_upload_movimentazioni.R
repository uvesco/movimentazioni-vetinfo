# Modulo Upload Movimentazioni (accetta .xls e .gz)                 # descrizione del modulo
# Dipendenze: readxl, R.utils, bslib, shiny                         # pacchetti richiesti

mod_upload_movimentazioni_ui <- function(id) {                      # interfaccia del modulo di upload
        ns <- NS(id)                                                # namespace per elementi dell'interfaccia
        bslib::card(                                                # scheda Bootstrap per contenere l'input
                title = "Carica file movimentazioni",               # titolo della card
                class = "mb-3",                                     # classe CSS per margine
                shiny::fileInput(                                   # widget di caricamento file
                        ns("file"),                                 # identificatore con namespace
                        "Seleziona file (.xls oppure .gz)",         # testo mostrato all'utente
                        accept = c(".xls", ".gz"),                  # tipi di file accettati
                        buttonLabel = "Sfoglia…",                  # etichetta del bottone
                ),
                shiny::helpText("Il file deve essere il .xls originale oppure lo stesso compresso come .gz.") # nota informativa
        )
}

mod_upload_movimentazioni_server <- function(id) {                  # logica del modulo di upload
        moduleServer(id, function(input, output, session) {         # definizione del server del modulo

                read_mov_xls_or_gz <- function(path, orig_name = NULL) {   # funzione ausiliaria per leggere i file
                        if (is.null(orig_name)) orig_name <- path          # usa il nome originale se fornito
                        nm <- tolower(orig_name)                           # normalizza in minuscolo

                        if (grepl("\\.gz$", nm)) {                         # se il file è compresso .gz
                                # Decompressione SOLO base R
                                tmp_xls <- tempfile(fileext = ".xls")      # crea file temporaneo
                                in_con  <- gzfile(path, open = "rb")       # connessione in lettura gzip
                                out_con <- file(tmp_xls, open = "wb")      # connessione in scrittura

                                # copia a chunk
                                repeat {                                   # legge e scrive a blocchi
                                        chunk <- readBin(in_con, what = "raw", n = 262144L)  # 256 KB per giro
                                        if (length(chunk) == 0L) break                         # termina a fine file
                                        writeBin(chunk, out_con)                               # scrive il blocco
                                }

                                # CHIUDI SEMPRE PRIMA DI LEGGERE
                                close(in_con)                             # chiude connessione in lettura
                                close(out_con)                            # chiude connessione in scrittura

                                # leggi: prima read_excel (auto), fallback a read_xls
                                df <- tryCatch(                           # tenta la lettura del file Excel
                                        readxl::read_excel(tmp_xls),      # lettura automatica
                                        error = function(e) {
                                                # Tenta fallback con read_xls per vecchi formati
                                                tryCatch(
                                                        readxl::read_xls(tmp_xls),
                                                        error = function(e2) {
                                                                # Se entrambi falliscono, solleva errore descrittivo
                                                                stop("Impossibile leggere il file Excel dal file .gz. Il file potrebbe essere corrotto, non valido o in un formato non supportato.")
                                                        }
                                                )
                                        }
                                )
                                return(df)                                # restituisce il dataframe

                        } else if (grepl("\\.xls$", nm)) {                 # se il file è un .xls non compresso
                                df <- tryCatch(                           # tenta la lettura del file Excel
                                        readxl::read_excel(path),         # lettura automatica
                                        error = function(e) {
                                                # Gestisce errori di file corrotti o non validi
                                                stop("Impossibile leggere il file Excel. Il file potrebbe essere corrotto, non valido o in un formato non supportato.")
                                        }
                                )
                                return(df)                                # restituisce il dataframe

                        } else {                                          # formato non riconosciuto
                                stop("Formato non supportato: usa .xls o .gz")   # errore esplicativo
                        }
                }

                dati <- reactiveVal(NULL)                                 # variabile reattiva per i dati caricati
                gruppo_colonne <- reactiveVal(NULL)                       # variabile reattiva per il gruppo determinato
                partite <- reactiveVal(NULL)                              # variabile reattiva per le partite (sommario)
                upload_status <- reactiveVal(list(type = "idle", message = NULL)) # stato dell'upload per messaggi persistenti

                notify_upload_issue <- function(msg, type = "error", duration = 8, status_type = "error") {
                        upload_status(list(type = status_type, message = msg))
                        shiny::showNotification(
                                msg,
                                type = type,
                                duration = duration,
                                session = session
                        )
                        NULL
                }

                standardize_movimentazioni <- function(df, filename = NULL) { # standardizza colonne e determina il gruppo
                        if (is.null(df) || ncol(df) == 0) {
                                stop("Il file caricato è vuoto e non può essere elaborato.")
                        }

                        col_names <- colnames(df)
                        if (is.null(col_names) || length(col_names) == 0) {
                                stop("Il file caricato non contiene intestazioni di colonna.")
                        }

                        col_names_trim <- trimws(as.character(col_names))
                        if (all(is.na(col_names_trim)) || all(col_names_trim == "") ||
                                all(grepl("^\\.\\.\\.[0-9]+$", col_names_trim))) {
                                stop("Il file caricato non contiene intestazioni di colonna valide.")
                        }


                        gruppo_match <- NULL                              # inizializza il gruppo corrispondente

                        infer_gruppo_da_filename <- function(nome_file) {
                                if (is.null(nome_file)) return(NULL)
                                nome_file <- tolower(nome_file)
                                for (g in names(col_orig_gruppi)) {
                                        if (grepl(g, nome_file, fixed = TRUE)) {
                                                return(g)
                                        }
                                }
                                NULL
                        }

                        is_file_vuoto <- function(x) {
                                if (nrow(x) == 0 && ncol(x) == 0) {
                                        return(TRUE)
                                }

                                messaggio_vuoto <- "non ci sono movimentazioni"
                                elementi <- c(colnames(x), unlist(x, use.names = FALSE))
                                elementi <- elementi[!is.na(elementi)]

                                if (length(elementi) == 0) {
                                        return(FALSE)
                                }

                                ha_messaggio <- any(grepl(messaggio_vuoto, elementi, ignore.case = TRUE))
                                nrow(x) <= 1 && ncol(x) <= 1 && ha_messaggio
                        }

                        if (is_file_vuoto(df)) {
                                gruppo_match <- infer_gruppo_da_filename(filename)

                                if (is.null(gruppo_match)) {
                                        stop("File vuoto rilevato ma impossibile determinare il gruppo di specie dal nome del file.")
                                }

                                colonne_gruppo <- col_standard_gruppi[[gruppo_match]]
                                df <- as.data.frame(
                                        structure(
                                                rep(list(character()), length(colonne_gruppo)),
                                                names = colonne_gruppo
                                        ),
                                        stringsAsFactors = FALSE
                                )
                        }

                        for (g in names(col_orig_gruppi)) {               # verifica a quale gruppo appartengono le colonne
                                if (identical(colnames(df), col_orig_gruppi[[g]])) {
                                        gruppo_match <- g
                                        break
                                }
                        }

                        if (is.null(gruppo_match)) {                      # nessun gruppo riconosciuto
                                return(notify_upload_issue(
                                        "File vuoto per i parametri selezionati."
                                ))
                        }

                        colnames(df) <- col_standard_gruppi[[gruppo_match]]  # rinomina con i nomi standard del gruppo

                        colonne_mancanti <- setdiff(col_standard, colnames(df))  # verifica la presenza di tutte le colonne standard
                        if (length(colonne_mancanti) > 0) {
                                return(notify_upload_issue(
                                        paste(
                                                "Colonne standard mancanti nel file caricato:",
                                                paste(colonne_mancanti, collapse = ", ")
                                        )
                                ))
                        }

                        df_standard <- df[, col_standard, drop = FALSE]   # mantiene solo le colonne comuni
                        attr(df_standard, "gruppo_specie") <- gruppo_match # memorizza il gruppo determinato

                        upload_status(list(type = "success", message = NULL))

                        if (nrow(df_standard) == 0) {
                                notify_upload_issue(
                                        "File vuoto per i parametri selezionati",
                                        type = "warning",
                                        duration = 6,
                                        status_type = "empty"
                                )
                        } else if (all(vapply(df_standard, function(col) all(is.na(col)), logical(1)))) {
                                return(notify_upload_issue(
                                        "Il file caricato contiene solo valori mancanti."
                                ))
                        }

                        list(
                                animali = df_standard,                   # dataframe standardizzato
                                gruppo = gruppo_match                    # gruppo individuato dalle colonne
                        )
                }

                enrich_animali_data <- function(df_animali) {             # arricchisce i dati degli animali con chiavi geografiche
                        if (is.null(df_animali) || nrow(df_animali) == 0) {
                                return(list(
                                        animali = df_animali,
                                        partite = NULL
                                ))
                        }
                        
                        # 1. Merge con STATIC_MOTIVI_INGRESSO per derivare provenienza nazionale/estera
                        # Rimuovi il campo Codice che non serve
                        motivi_lookup <- STATIC_MOTIVI_INGRESSO[, c("Descrizione", "prov_italia")]
                        
                        # Funzione di normalizzazione robusta per gestire spazi multipli, encoding, etc.
                        normalize_text <- function(x) {
                                # Converti in carattere
                                x <- as.character(x)
                                # Rimuovi spazi iniziali e finali
                                x <- trimws(x)
                                # Converti in maiuscolo
                                x <- toupper(x)
                                # Sostituisci spazi multipli con uno singolo
                                x <- gsub("\\s+", " ", x)
                                # Rimuovi eventuali caratteri di controllo invisibili
                                x <- gsub("[\\x00-\\x1F\\x7F]", "", x)
                                return(x)
                        }
                        
                        # Applica normalizzazione robusta
                        motivi_lookup$Descrizione_norm <- normalize_text(motivi_lookup$Descrizione)
                        df_animali$ingresso_motivo_norm <- normalize_text(df_animali$ingresso_motivo)
                        
                        # Diagnostica: stampa valori unici prima del merge
                        valori_unici_file <- unique(df_animali$ingresso_motivo_norm[!is.na(df_animali$ingresso_motivo_norm)])
                        valori_unici_ref <- unique(motivi_lookup$Descrizione_norm)
                        message("[DEBUG] Valori unici in ingresso_motivo (file): ", length(valori_unici_file))
                        message("[DEBUG] Valori unici in motivi_lookup (riferimento): ", length(valori_unici_ref))
                        
                        # Trova valori non matchabili PRIMA del merge
                        non_matchati <- valori_unici_file[!valori_unici_file %in% valori_unici_ref]
                        if (length(non_matchati) > 0) {
                                message("[WARNING] Trovati ", length(non_matchati), " valori di ingresso_motivo che NON corrispondono al riferimento:")
                                for (val in non_matchati) {
                                        n_occorrenze <- sum(df_animali$ingresso_motivo_norm == val, na.rm = TRUE)
                                        message("[WARNING]   \"", val, "\" (", n_occorrenze, " occorrenze)")
                                }
                                message("[INFO] Usa il modulo 'Sistema di Verifica e Debug' per maggiori dettagli")
                        } else {
                                message("[OK] Tutti i valori di ingresso_motivo sono riconosciuti!")
                        }
                        
                        # Esegui il merge usando le versioni normalizzate
                        # Prima aggiungi un campo temporaneo con l'indice originale per preservare l'ordine
                        df_animali$temp_idx <- seq_len(nrow(df_animali))
                        
                        df_animali <- merge(df_animali, 
                                          motivi_lookup[, c("Descrizione_norm", "prov_italia")], 
                                          by.x = "ingresso_motivo_norm", 
                                          by.y = "Descrizione_norm", 
                                          all.x = TRUE, 
                                          sort = FALSE)
                        
                        # Ripristina l'ordine originale
                        df_animali <- df_animali[order(df_animali$temp_idx), ]
                        df_animali$temp_idx <- NULL
                        
                        # Diagnostica post-merge
                        n_con_prov_italia <- sum(!is.na(df_animali$prov_italia))
                        n_senza_prov_italia <- sum(is.na(df_animali$prov_italia))
                        message("[DEBUG] Animali con prov_italia assegnato: ", n_con_prov_italia, " / ", nrow(df_animali))
                        if (n_senza_prov_italia > 0) {
                                message("[WARNING] ", n_senza_prov_italia, " animali senza prov_italia (merge fallito)")
                        }

                        
                        # 2. Estrai primi 5 caratteri da orig_stabilimento_cod e merge con df_stab
                        # Solo per animali importati dall'Italia (prov_italia == TRUE)
                        df_animali$cod_stab <- substr(df_animali$orig_stabilimento_cod, 1, 5)
                        
                        # Seleziona solo le colonne necessarie da df_stab e rinomina con suffisso _orig
                        stab_cols <- df_stab[, c("cod_stab", "COD_PROV", "PRO_COM_T")]
                        colnames(stab_cols) <- c("cod_stab", "COD_PROV_orig", "PRO_COM_T_orig")
                        
                        df_animali <- merge(df_animali, stab_cols, 
                                          by = "cod_stab", 
                                          all.x = TRUE, sort = FALSE)
                        
                        # 4. Ricava lo stato di nascita dalle prime due lettere di capo_identificativo
                        df_animali$nascita_stato <- substr(df_animali$capo_identificativo, 1, 2)
                        
                        # 5. Merge con df_stati per aggiungere descrizione dello stato
                        stati_lookup <- df_stati[, c("Codice", "Descrizione")]
                        colnames(stati_lookup)[2] <- "nascita_stato_descr"
                        df_animali <- merge(df_animali, stati_lookup, 
                                          by.x = "nascita_stato", by.y = "Codice", 
                                          all.x = TRUE, sort = FALSE)
                        
                        # 6. Per animali nati in Italia, ricava nascita_COD_UTS dai caratteri 3-5
                        df_animali$nascita_COD_UTS <- NA_character_
                        is_born_in_italy <- !is.na(df_animali$nascita_stato) & df_animali$nascita_stato == "IT"
                        df_animali$nascita_COD_UTS[is_born_in_italy] <- substr(
                                df_animali$capo_identificativo[is_born_in_italy], 3, 5
                        )
                        
                        # 3. Crea dataframe partite (sommario senza campi capo_*)
                        # Identifica colonne che NON iniziano con "capo_"
                        non_capo_cols <- grep("^capo_", colnames(df_animali), value = TRUE, invert = TRUE)
                        
                        if (length(non_capo_cols) > 0) {
                                # Conta il numero di capi per ogni combinazione unica di valori non-capo
                                df_partite <- aggregate(
                                        list(n_capi = df_animali$capo_identificativo), 
                                        by = df_animali[, non_capo_cols, drop = FALSE],
                                        FUN = length
                                )
                        } else {
                                df_partite <- NULL
                        }
                        
                        list(
                                animali = df_animali,
                                partite = df_partite
                        )
                }

                observeEvent(input$file, {                                # osserva l'input file
                        if (is.null(input$file)) {                        # nessun file selezionato
                                dati(NULL)
                                gruppo_colonne(NULL)
                                partite(NULL)
                                upload_status(list(type = "idle", message = NULL))
                                return()
                        }

                        req(input$file$datapath)                          # verifica che il percorso esista
                        withProgress(message = "Lettura file…", value = 0.1, {   # mostra barra di avanzamento
                                df <- tryCatch(                           # gestisce errori durante la lettura del file
                                        read_mov_xls_or_gz(input$file$datapath, orig_name = input$file$name), # legge il file
                                        error = function(e) {
                                                notify_upload_issue(e$message)  # mostra messaggio di errore
                                                NULL                      # restituisce NULL in caso di errore
                                        }
                                )
                                
                                if (is.null(df)) {                        # se la lettura è fallita
                                        dati(NULL)
                                        gruppo_colonne(NULL)
                                        partite(NULL)
                                        return()
                                }
                                
                                incProgress(0.8)                          # aggiorna la progress bar
                                standardizzato <- standardize_movimentazioni(df, filename = input$file$name) # standardizza dati e gruppo
                                if (is.null(standardizzato)) {
                                        dati(NULL)
                                        gruppo_colonne(NULL)
                                        partite(NULL)
                                        return()
                                }

                                # Arricchisce i dati con informazioni aggiuntive
                                enriched <- enrich_animali_data(standardizzato$animali)
                                
                                dati(enriched$animali)                    # salva i dati arricchiti
                                gruppo_colonne(standardizzato$gruppo)     # salva il gruppo determinato
                                partite(enriched$partite)                 # salva il sommario delle partite
                        })
                }, ignoreInit = TRUE)                                     # esegue solo dopo il primo caricamento

                # verifica validità del file (colonne e righe)
                file_check <- reactive({
                        req(dati())                                      # richiede che i dati siano presenti
                        req(gruppo_colonne())                            # richiede che il gruppo sia definito
                        
                        # verifica che il dataframe contenga almeno le colonne standard
                        has_standard_cols <- all(col_standard %in% colnames(dati()))
                        has_standard_cols && nrow(dati()) > 0            # verifica presenza colonne base e almeno una riga
                })
                
                # restituisce il data.frame caricato (o NULL) e il gruppo collegato
                list(
                        animali = reactive(dati()),                      # espone i dati arricchiti
                        partite = reactive(partite()),                   # espone il sommario delle partite
                        gruppo = reactive(gruppo_colonne()),              # espone il gruppo determinato
                        status = reactive(upload_status()),               # espone lo stato dell'upload per messaggi
                        file_check = file_check                          # espone la verifica validità file
                )
        })
}
