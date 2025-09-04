app_ui <- function() {
	
	fluidPage(
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
}