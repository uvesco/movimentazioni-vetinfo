# =============================================================================
# UTILITY FUNCTIONS FOR BDN EXPORT
# =============================================================================
# Funzioni per l'esportazione di codici animali per l'interrogazione
# raffinata BDN. I file .txt devono rispettare i seguenti requisiti:
# - Un codice animale per riga
# - Codifica ANSI (Windows-1252)
# - Massimo 255 righe per file
# - File compressi in formato ZIP
# =============================================================================

#' Crea file ZIP con codici animali per BDN
#'
#' Questa funzione raccoglie tutti i codici identificativi degli animali
#' dalle liste di malattie (provenienze o nascite), li divide in chunks
#' di massimo 255 righe per file, e crea un archivio ZIP contenente
#' i file .txt con codifica ANSI.
#'
#' @param liste_malattie Lista di dataframe, uno per malattia. Ogni dataframe
#'   deve contenere la colonna 'capo_identificativo' con i codici degli animali.
#' @param tipo Tipo di esportazione: "provenienze" o "nascite"
#' @param max_righe_per_file Numero massimo di righe per file (default: 255)
#'
#' @return Path temporaneo del file ZIP creato
#'
#' @examples
#' \dontrun{
#' liste <- list("BRC" = data.frame(capo_identificativo = c("IT001", "IT002")))
#' zip_path <- crea_zip_bdn_export(liste, tipo = "provenienze")
#' }
crea_zip_bdn_export <- function(liste_malattie, tipo = "provenienze", max_righe_per_file = 255) {
	
	# Verifica input
	if (length(liste_malattie) == 0) {
		stop("Nessuna malattia con animali da esportare")
	}
	
	# Raccoglie tutti i codici identificativi da tutte le malattie
	# Rimuove duplicati per evitare di esportare lo stesso animale più volte
	codici_animali <- character(0)
	for (nome_malattia in names(liste_malattie)) {
		df <- liste_malattie[[nome_malattia]]
		if ("capo_identificativo" %in% names(df)) {
			codici <- as.character(df$capo_identificativo)
			# Rimuove NA e valori vuoti
			codici <- codici[!is.na(codici) & codici != ""]
			codici_animali <- c(codici_animali, codici)
		}
	}
	
	# Rimuove duplicati
	codici_animali <- unique(codici_animali)
	
	# Verifica che ci siano codici da esportare
	if (length(codici_animali) == 0) {
		stop("Nessun codice animale valido da esportare")
	}
	
	# Ordina i codici per una migliore organizzazione
	codici_animali <- sort(codici_animali)
	
	# Divide i codici in chunks di max_righe_per_file
	n_codici <- length(codici_animali)
	n_files <- ceiling(n_codici / max_righe_per_file)
	
	# Crea directory temporanea per i file
	temp_dir <- tempfile(pattern = "bdn_export_")
	dir.create(temp_dir, recursive = TRUE)
	
	# Crea i file .txt
	file_paths <- character(n_files)
	for (i in 1:n_files) {
		start_idx <- (i - 1) * max_righe_per_file + 1
		end_idx <- min(i * max_righe_per_file, n_codici)
		chunk <- codici_animali[start_idx:end_idx]
		
		# Nome file con indice
		file_name <- if (n_files == 1) {
			sprintf("bdn_%s.txt", tipo)
		} else {
			sprintf("bdn_%s_%02d.txt", tipo, i)
		}
		
		file_path <- file.path(temp_dir, file_name)
		file_paths[i] <- file_path
		
		# Scrive il file con codifica Windows-1252 (ANSI)
		# Usa fileEncoding per garantire la codifica corretta
		con <- file(file_path, open = "wb", encoding = "Windows-1252")
		tryCatch({
			# Scrive ogni codice su una riga separata
			writeLines(chunk, con = con, useBytes = TRUE)
		}, finally = {
			close(con)
		})
	}
	
	# Crea il file ZIP
	zip_name <- sprintf("bdn_export_%s_%s.zip", tipo, format(Sys.Date(), "%Y%m%d"))
	zip_path <- file.path(tempdir(), zip_name)
	
	# Rimuove il file ZIP se esiste già
	if (file.exists(zip_path)) {
		unlink(zip_path)
	}
	
	# Crea l'archivio ZIP
	# zip() richiede che i percorsi siano relativi alla directory corrente
	old_wd <- getwd()
	tryCatch({
		setwd(temp_dir)
		zip(zipfile = zip_path, files = basename(file_paths), flags = "-q")
	}, finally = {
		setwd(old_wd)
	})
	
	# Pulisce i file temporanei
	unlink(temp_dir, recursive = TRUE)
	
	return(zip_path)
}

#' Verifica se ci sono animali da esportare per BDN
#'
#' @param liste_malattie Lista di dataframe, uno per malattia
#'
#' @return TRUE se ci sono animali da esportare, FALSE altrimenti
ha_animali_da_esportare <- function(liste_malattie) {
	if (is.null(liste_malattie) || length(liste_malattie) == 0) {
		return(FALSE)
	}
	
	# Verifica se c'è almeno un dataframe con righe
	for (nome_malattia in names(liste_malattie)) {
		df <- liste_malattie[[nome_malattia]]
		if (!is.null(df) && nrow(df) > 0) {
			return(TRUE)
		}
	}
	
	return(FALSE)
}
