# R/app_server.R
app_server <- function(input, output, session) {
	
	# Importazione dati --------------------------------
	animali <- mod_upload_movimentazioni_server("upload_mov")
	
	df_specie <- read.csv("data_static/specie.csv", stringsAsFactors = FALSE)
	
	gruppo <- reactive({
		req(animali())
		df <- animali()
		determinare_gruppo(df, df_specie)
	})
	
	# crea due nuovi tab in caso animali() != "vuoto" e non sia NULL
	
	observe({
		req(animali())
		if (gruppo() != "vuoto") {

				insertTab(
					inputId = "tabs", target = "input", position = "after",
					tab = tabPanel(title = "Elaborazione", value = "elaborazione",
					               p(gruppo()))
					)
				
				insertTab(
					inputId = "tabs", target = "elaborazione", position = "after",
					tab = tabPanel(title = "Output", value = "output",
												 p("prova2"))
					)
				
			
		} else {
			# rimuovi tab se esistono
			if ("Elaborazione" %in% names(input$tabs)) {
				removeTab("tabs", "elaborazione")
			}
			if ("Output" %in% names(input$tabs)) {
				removeTab("tabs", "output")
			}
		}
	})
	
        # messaggio sul tipo di file importato
	
	output$tipo_file <- renderText({
		df <- animali()
		if (is.null(df)) {
			"File non ancora caricato"
		} else {
                        tryCatch({
                                if (gruppo() == "vuoto") {
                                        return(paste0("File vuoto: ", colnames(df)[1]))
                                } else {
                                        paste("File importato correttamente. Gruppo specie:", gruppo())
                                }
                        }, error = function(e) {
                                paste("Errore nel file:", e$message)
                        })
                }
        })


        # ottieni il numero di righe dei dati importati

        output$n_animali <- renderText({
                df <- animali()
                req(df)
                paste("Numero di animali importati:", nrow(df))
        })
}
