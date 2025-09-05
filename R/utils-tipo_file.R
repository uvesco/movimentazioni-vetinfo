# script per determinare il tipo di file                               # descrizione del file

determinare_gruppo <- function(animali, df_specie) {                   # funzione che determina il gruppo specie
        # caso in cui il file sia vuoto
        if (nrow(animali) == 0) {                                       # nessuna riga presente
                "vuoto"                                                 # ritorna la stringa "vuoto"
        } else {

        # prendo tutte le specie presenti
        specie_presenti <- tolower(unique(animali$SPECIE))              # elenco delle specie in minuscolo

        # mappo al gruppo
        gruppi <- df_specie %>%                                         # unisce con tabella delle specie
                filter(SPECIE %in% specie_presenti) %>%                 # seleziona solo specie presenti
                pull(GRUPPO) %>%                                       # estrae il gruppo corrispondente
                unique()                                               # rimuove eventuali duplicati

        if (length(gruppi) == 1) {                                     # un solo gruppo trovato
                return(gruppi)                                          # restituisce il gruppo
        } else if (length(gruppi) == 0) {                              # nessun gruppo trovato
                stop("Specie non riconosciute")                         # solleva errore
        } else {                                                        # più gruppi trovati
                stop("File contiene specie di gruppi diversi: ", paste(gruppi, collapse = ", ")) # errore con elenco gruppi
        }
}
}
