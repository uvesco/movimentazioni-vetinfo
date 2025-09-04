# R/app_server.R
app_server <- function(input, output, session) {
	
	# Importazione dati --------------------------------
	animali <- mod_upload_movimentazioni_server("upload_mov")
	
	df_specie <- read.csv("data_static/specie.csv", stringsAsFactors = FALSE)
	
	# ottieni il numero di righe dei dati importati
	# 
	output$tipo_file <- renderText({
		df <- animali()
		if (is.null(df)) {
			"File non ancora caricato"
		} else {
			tryCatch({
				gruppo <- determinare_gruppo(df, df_specie)
				if(gruppo == "vuoto") {
					return(paste0("File vuoto: "), colnames(df)[1])
				}else{
				paste("File importato correttamente. Gruppo specie:", gruppo)
				}
			}, error = function(e) {
				paste("Errore nel file:", e$message)
			
			})
		}
	})
	
	output$n_animali <- renderText({
		df <- animali()
		paste("Numero di animali importati:", nrow(df))
	})
}
