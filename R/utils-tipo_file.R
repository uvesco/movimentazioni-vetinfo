# script per determinare il tipo di file

determinare_gruppo <- function(animali, df_specie) {
	# caso in cui il file sia vuoto
	if (nrow(animali) == 0) {
		"vuoto"
	}else{
	
	# prendo tutte le specie presenti
	specie_presenti <- tolower(unique(animali$SPECIE))
	
	# mappo al gruppo
	gruppi <- df_specie %>%
		filter(SPECIE %in% specie_presenti) %>%
		pull(GRUPPO) %>%
		unique()
	
	if (length(gruppi) == 1) {
		return(gruppi)
	} else if (length(gruppi) == 0) {
		stop("Specie non riconosciute")
	} else {
		stop("File contiene specie di gruppi diversi: ", paste(gruppi, collapse = ", "))
	}
}
}

