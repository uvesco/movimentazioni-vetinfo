app_ui <- function() {
	
	fluidPage(
	titlePanel("Elaborazione movimentazioni da BDN (IN SVILUPPO)"),
	
	tabsetPanel(
		id = "tabs",
		tabPanel(
			title = "Input", value = "input",

			sidebarLayout(
				sidebarPanel(
					
					mod_upload_movimentazioni_ui("upload_mov")
					
					
				),
				mainPanel(
					verbatimTextOutput("tipo_file"),
					verbatimTextOutput("n_animali")
				)
			)
			
		)
	)
	
	

	)
}