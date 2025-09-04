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

        tabs_inserite <- reactiveVal(FALSE)

        observe({
                req(animali())
                if (gruppo() != "vuoto" && !tabs_inserite()) {

                                insertTab(
                                        inputId = "tabs", target = "input", position = "after",
                                        tab = tabPanel(title = "Elaborazione", value = "elaborazione",
                                                       textOutput("gruppo_tab"))
                                        )

                                insertTab(
                                        inputId = "tabs", target = "elaborazione", position = "after",
                                        tab = tabPanel(title = "Output", value = "output",
                                                                                                 p("prova2"))
                                        )

                                tabs_inserite(TRUE)

                } else if (gruppo() == "vuoto" && tabs_inserite()) {
                        removeTab("tabs", "elaborazione")
                        removeTab("tabs", "output")
                        tabs_inserite(FALSE)
                }
        })

        output$gruppo_tab <- renderText(gruppo())

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
