# Imposta timezone coerente con te
Sys.setenv(TZ = "Europe/Rome")

library(shiny)
library(readxl)

ui <- fluidPage(
	titlePanel("Elaborazione movimentazioni da BDN (IN SVILUPPO)"),
	sidebarLayout(
		sidebarPanel(
			fileInput('file1', 'Seleziona un file xls da BDN',
								accept = c(".xls"), buttonLabel = "Sfoglia...", 
								placeholder = "Nessun file selezionato"
			)
		),
		mainPanel(
			verbatimTextOutput("n_animali")
		)
	)
)

server <- function(input, output, session) {

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

shinyApp(ui, server)
