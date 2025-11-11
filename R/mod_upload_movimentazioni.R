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
                                        error = function(e) readxl::read_xls(tmp_xls)  # fallback per vecchi formati
                                )
                                return(df)                                # restituisce il dataframe

                        } else if (grepl("\\.xls$", nm)) {                 # se il file è un .xls non compresso
                                return(readxl::read_excel(path))          # lettura diretta del file Excel

                        } else {                                          # formato non riconosciuto
                                stop("Formato non supportato: usa .xls o .gz")   # errore esplicativo
                        }
                }

                dati <- reactiveVal(NULL)                                 # variabile reattiva per i dati caricati
                gruppo_colonne <- reactiveVal(NULL)                       # variabile reattiva per il gruppo determinato

                standardize_movimentazioni <- function(df) {              # standardizza colonne e determina il gruppo
                        gruppo_match <- NULL                              # inizializza il gruppo corrispondente

                        for (g in names(col_orig_gruppi)) {               # verifica a quale gruppo appartengono le colonne
                                if (identical(colnames(df), col_orig_gruppi[[g]])) {
                                        gruppo_match <- g
                                        break
                                }
                        }

                        if (is.null(gruppo_match)) {                      # nessun gruppo riconosciuto
                                stop("Struttura colonne non riconosciuta per il file caricato.")
                        }

                        colnames(df) <- col_standard_gruppi[[gruppo_match]]  # rinomina con i nomi standard del gruppo

                        colonne_mancanti <- setdiff(col_standard, colnames(df))  # verifica la presenza di tutte le colonne standard
                        if (length(colonne_mancanti) > 0) {
                                stop(
                                        "Colonne standard mancanti nel file caricato: ",
                                        paste(colonne_mancanti, collapse = ", ")
                                )
                        }

                        df_standard <- df[, col_standard, drop = FALSE]   # mantiene solo le colonne comuni
                        attr(df_standard, "gruppo_specie") <- gruppo_match # memorizza il gruppo determinato

                        list(
                                animali = df_standard,                   # dataframe standardizzato
                                gruppo = gruppo_match                    # gruppo individuato dalle colonne
                        )
                }

                observeEvent(input$file, {                                # osserva l'input file
                        if (is.null(input$file)) {                        # nessun file selezionato
                                dati(NULL)
                                gruppo_colonne(NULL)
                                return()
                        }

                        req(input$file$datapath)                          # verifica che il percorso esista
                        withProgress(message = "Lettura file…", value = 0.1, {   # mostra barra di avanzamento
                                df <- read_mov_xls_or_gz(input$file$datapath, orig_name = input$file$name) # legge il file
                                incProgress(0.8)                          # aggiorna la progress bar
                                standardizzato <- standardize_movimentazioni(df) # standardizza dati e gruppo
                                dati(standardizzato$animali)              # salva i dati standardizzati
                                gruppo_colonne(standardizzato$gruppo)     # salva il gruppo determinato
                        })
                }, ignoreInit = TRUE)                                     # esegue solo dopo il primo caricamento

                # restituisce il data.frame caricato (o NULL) e il gruppo collegato
                list(
                        animali = reactive(dati()),                      # espone i dati standardizzati
                        gruppo = reactive(gruppo_colonne())               # espone il gruppo determinato
                )
        })
}
