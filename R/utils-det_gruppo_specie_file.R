# script per determinare il tipo di file (differenti tipi per differenti specie)

determinare_gruppo <- function(animali, df_specie) {                   # funzione che determina il gruppo specie
        gruppo_standard <- attr(animali, "gruppo_specie")              # recupera eventuale gruppo già assegnato

        # caso in cui il file sia vuoto
        if (nrow(animali) == 0) {                                       # nessuna riga presente
                if (!is.null(gruppo_standard)) {                        # se il gruppo è noto dalle colonne
                        return(gruppo_standard)                         # restituisce il gruppo dedotto dalla struttura
                }
                return("vuoto")                                        # altrimenti segnala file vuoto
        }

        # identifica la colonna specie a seconda che i dati siano stati standardizzati o meno
        colonna_specie <- NULL
        if ("dest_specie" %in% colnames(animali)) {
                colonna_specie <- "dest_specie"
        } else if ("SPECIE" %in% colnames(animali)) {
                colonna_specie <- "SPECIE"
        } else {
                stop("Colonna delle specie non trovata nel file caricato")
        }

        # prendo tutte le specie presenti
        specie_presenti <- tolower(unique(animali[[colonna_specie]]))   # elenco delle specie in minuscolo

        # mappo al gruppo
        gruppi <- df_specie %>%                                         # unisce con tabella delle specie
                filter(SPECIE %in% specie_presenti) %>%                 # seleziona solo specie presenti
                pull(GRUPPO) %>%                                       # estrae il gruppo corrispondente
                unique()                                               # rimuove eventuali duplicati

        if (length(gruppi) == 1) {                                     # un solo gruppo trovato
                if (!is.null(gruppo_standard) && gruppo_standard != gruppi) {
                        stop("Il gruppo dedotto dai dati non corrisponde al gruppo atteso dalle colonne standard")
                }
                return(gruppi)                                          # restituisce il gruppo
        } else if (length(gruppi) == 0) {                              # nessun gruppo trovato
                if (!is.null(gruppo_standard)) {
                        return(gruppo_standard)
                }
                stop("Specie non riconosciute")                        # solleva errore
        } else {                                                        # più gruppi trovati
                stop("File contiene specie di gruppi diversi: ", paste(gruppi, collapse = ", ")) # errore con elenco gruppi
        }
}
