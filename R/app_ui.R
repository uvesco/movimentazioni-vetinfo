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
					uiOutput("n_animali"),
					uiOutput("titolo_malattie"),
					tableOutput("malattie_importate")
				)
			)
			
		),
		
		# Tab for manual validation controls
		tabPanel(
			title = "Controllo Manuale", value = "controllo_manuale",
			h3("Animali con dati geografici non validi"),
			
			h4("Comune di provenienza non trovato"),
			p("Animali italiani per cui non è stato possibile identificare il comune di provenienza:"),
			DT::DTOutput("tabella_provenienza_non_trovata"),
			downloadButton("download_provenienza_non_trovata", "Scarica Excel"),
			
			hr(),
			
			h4("Provincia di nascita non trovata"),
			p("Animali italiani per cui non è stato possibile identificare la provincia di nascita dal marchio auricolare:"),
			DT::DTOutput("tabella_nascita_non_trovata"),
			downloadButton("download_nascita_non_trovata", "Scarica Excel")
		),
		
		# Tab for provenance disease checks
		tabPanel(
			title = "Provenienze", value = "provenienze",
			h3("Animali provenienti da zone non indenni"),
			uiOutput("ui_provenienze")
		),
		
		# Tab for birth disease checks
		tabPanel(
			title = "Nascite", value = "nascite",
			h3("Animali nati in zone non indenni"),
			uiOutput("ui_nascite")
		)
	)
	
	

	)
}