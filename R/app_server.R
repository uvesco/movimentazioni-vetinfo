# R/app_server.R
app_server <- function(input, output, session) {
	
	# Importazione dati --------------------------------
	animali <- reactive({
		req(input$file1)
		inFile <- input$file1
		# Leggi il file xls (usa readxl o altro pacchetto a tua scelta)
		df <- readxl::read_excel(inFile$datapath)
		df
	})
	
	# ottieni il numero di righe dei dati importati
	
	output$n_animali <- renderText({
		df <- animali()
		paste("Numero di animali importati:", nrow(df))
	})
}
