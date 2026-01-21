# Modulo Upload Movimentazioni (accetta .xls e .gz)                 # descrizione del modulo
# Dipendenze: readxl, R.utils, bslib, shiny                         # pacchetti richiesti

mod_upload_movimentazioni_ui <- function(id) {                      # interfaccia del modulo di upload
        ns <- NS(id)                                                # namespace per elementi dell'interfaccia
        bslib::card(                                                # scheda Bootstrap per contenere l'input
                title = "Carica file movimentazioni",               # titolo della card
                class = "mb-3",                                     # classe CSS per margine
                shiny::fileInput(                                   # widget di caricamento file
                        ns("file"),                                 # identificatore con namespace
                        "Seleziona uno o più file (.xls oppure .gz)",  # testo mostrato all'utente
                        accept = c(".xls", ".gz"),                  # tipi di file accettati
                        buttonLabel = "Sfoglia…",                  # etichetta del bottone
                        multiple = TRUE                             # permette selezione multipla
                ),
                shiny::helpText("È possibile caricare più file contemporaneamente, purché siano tutti dello stesso gruppo di specie. I file devono essere i .xls originali oppure gli stessi compressi come .gz.") # nota informativa
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
                nome_file <- reactiveVal(NULL)                            # nome file caricato
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

                observeEvent(input$file, {                                # osserva l'input file
                        if (is.null(input$file)) {                        # nessun file selezionato
                                dati(NULL)
                                gruppo_colonne(NULL)
                                nome_file(NULL)
                                upload_status(list(type = "idle", message = NULL))
                                return()
                        }

                        req(input$file$datapath)                          # verifica che il percorso esista
                        
                        # Ottieni lista di file (singolo o multiplo)
                        file_paths <- input$file$datapath
                        file_names <- input$file$name
                        n_files <- length(file_paths)
                        
                        withProgress(message = "Lettura file…", value = 0.1, {   # mostra barra di avanzamento
                                
                                # Lista per memorizzare i dataframe standardizzati e i gruppi
                                df_list <- list()
                                gruppi_list <- character(0)
                                
                                # Leggi e processa ogni file
                                for (i in seq_along(file_paths)) {
                                        incProgress(0.1 / n_files, detail = paste("File", i, "di", n_files))
                                        
                                        df <- tryCatch(                           # gestisce errori durante la lettura del file
                                                read_mov_xls_or_gz(file_paths[i], orig_name = file_names[i]), # legge il file
                                                error = function(e) {
                                                        notify_upload_issue(paste("Errore nel file", file_names[i], ":", e$message))  # mostra messaggio di errore
                                                        NULL                      # restituisce NULL in caso di errore
                                                }
                                        )
                                        
                                        if (is.null(df)) {                        # se la lettura è fallita
                                                dati(NULL)
                                                gruppo_colonne(NULL)
                                                nome_file(NULL)
                                                return()
                                        }
                                        
                                        standardizzato <- standardize_movimentazioni(df, filename = file_names[i]) # standardizza dati e gruppo
                                        if (is.null(standardizzato)) {
                                                dati(NULL)
                                                gruppo_colonne(NULL)
                                                nome_file(NULL)
                                                return()
                                        }
                                        
                                        df_list[[i]] <- standardizzato$animali
                                        gruppi_list <- c(gruppi_list, standardizzato$gruppo)
                                }
                                
                                incProgress(0.4)
                                
                                # Verifica che tutti i file appartengano allo stesso gruppo di specie
                                gruppi_unici <- unique(gruppi_list)
                                if (length(gruppi_unici) > 1) {
                                        notify_upload_issue(
                                                paste("Errore: i file caricati appartengono a gruppi di specie diversi:",
                                                      paste(gruppi_unici, collapse = ", "),
                                                      ". Caricare solo file dello stesso gruppo di specie.")
                                        )
                                        dati(NULL)
                                        gruppo_colonne(NULL)
                                        nome_file(NULL)
                                        return()
                                }
                                
                                incProgress(0.2)
                                
                                # Combina tutti i dataframe in uno solo
                                if (length(df_list) == 1) {
                                        df_combinato <- df_list[[1]]
                                } else {
                                        df_combinato <- do.call(rbind, df_list)
                                }
                                
                                incProgress(0.2)
                                
                                dati(df_combinato)                             # salva i dati combinati
                                gruppo_colonne(gruppi_unici[1])                # salva il gruppo determinato
                                nome_file(paste(file_names, collapse = ", "))  # salva i nomi file originali
                        })
                }, ignoreInit = TRUE)                                     # esegue solo dopo il primo caricamento

                # verifica validità del file (colonne e righe)
                file_check <- reactive({
                        req(dati())                                      # richiede che i dati siano presenti
                        req(gruppo_colonne())                            # richiede che il gruppo sia definito
                        
                        # verifica che le colonne del dataframe corrispondano a quelle standard
                        identical(colnames(dati()), col_standard) &&     # verifica corrispondenza colonne
                                nrow(dati()) > 0                         # e presenza di almeno una riga
                })
                
                # restituisce il data.frame caricato (o NULL) e il gruppo collegato
                list(
                        animali = reactive(dati()),                      # espone i dati standardizzati
                        gruppo = reactive(gruppo_colonne()),              # espone il gruppo determinato
                        status = reactive(upload_status()),               # espone lo stato dell'upload per messaggi
                        file_check = file_check,                         # espone la verifica validità file
                        file_name = reactive(nome_file())                # espone il nome file caricato
                )
        })
}
