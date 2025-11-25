# =============================================================================
# UI PRINCIPALE DELL'APPLICAZIONE
# =============================================================================
# Questo file definisce l'interfaccia utente dell'applicazione Shiny.
# L'app è organizzata in tab per gestire diverse funzionalità:
# - Input: caricamento file movimentazioni
# - Help: documentazione e guida utente (sempre visibile)
# - Tab dinamici: Controllo Manuale, Provenienze, Nascite, Output
#   (mostrati solo quando necessario dopo il caricamento dati)
# =============================================================================

app_ui <- function() {
	
	fluidPage(
		# Titolo principale dell'applicazione
		titlePanel("Elaborazione movimentazioni da BDN (IN SVILUPPO)"),
		
		# Container principale con tab
		tabsetPanel(
			id = "tabs",
			
			# =====================================================================
			# TAB INPUT: Caricamento file movimentazioni
			# =====================================================================
			# Questo tab è sempre visibile e permette di caricare i file Excel
			# delle movimentazioni esportati dalla BDN (Banca Dati Nazionale)
			tabPanel(
				title = "Input", 
				value = "input",
				
				sidebarLayout(
					# Pannello laterale con widget di upload
					sidebarPanel(
						mod_upload_movimentazioni_ui("upload_mov")
					),
					# Pannello principale con informazioni sul file caricato
					mainPanel(
						verbatimTextOutput("tipo_file"),      # Messaggio stato caricamento
						uiOutput("n_animali"),                 # Conteggio animali
						uiOutput("titolo_malattie"),           # Titolo sezione malattie
						tableOutput("malattie_importate")      # Tabella malattie rilevanti
					)
				)
			),
			
			# =====================================================================
			# TAB HELP: Documentazione e guida utente
			# =====================================================================
			# Questo tab è sempre visibile e contiene la documentazione dell'app
			tabPanel(
				title = "Help",
				value = "help",
				
				# Contenuto Help in formato Markdown-like
				div(
					class = "container-fluid",
					style = "max-width: 900px; padding: 20px;",
					
					h2("Guida all'utilizzo"),
					hr(),
					
					h3("1. Introduzione"),
					p("Questa applicazione permette di elaborare le movimentazioni animali esportate dalla BDN 
					   e verificare lo stato sanitario delle zone di provenienza e nascita degli animali."),
					
					h3("2. Caricamento File"),
					h4("2.1 Formati supportati"),
					tags$ul(
						tags$li("File Excel .xls (formato originale BDN)"),
						tags$li("File compressi .gz (file .xls compressi)")
					),
					
					h4("2.2 Gruppi specie supportati"),
					tags$ul(
						tags$li("Bovini"),
						tags$li("Ovicaprini")
					),
					
					h3("3. Elaborazione Dati"),
					h4("3.1 Classificazione origine"),
					p("Gli animali vengono classificati come 'Italia' o 'Estero' basandosi su:"),
					tags$ul(
						tags$li("Prefisso 'IT' nel marchio auricolare"),
						tags$li("Motivo di ingresso nella tabella decodifiche")
					),
					
					h4("3.2 Estrazione dati geografici"),
					p("Per ogni animale vengono estratti:"),
					tags$ul(
						tags$li("Provincia di nascita: dalle prime 3 cifre del marchio auricolare"),
						tags$li("Comune di provenienza: dal codice stabilimento di origine")
					),
					
					h4("3.3 Incrocio con dati malattie"),
					p("I dati geografici vengono incrociati con le tabelle delle malattie per verificare:"),
					tags$ul(
						tags$li("Stato sanitario del comune di provenienza (prefisso prov_)"),
						tags$li("Stato sanitario della provincia di nascita (prefisso nascita_)")
					),
					
					h3("4. Tab Risultati"),
					h4("4.1 Controllo Manuale"),
					p("Mostra gli animali italiani per cui non è stato possibile identificare:"),
					tags$ul(
						tags$li("Il comune di provenienza (codice stabilimento non valido)"),
						tags$li("La provincia di nascita (marchio auricolare non mappabile)")
					),
					
					h4("4.2 Provenienze"),
					p("Mostra gli animali provenienti da comuni/zone non indenni per le malattie considerate."),
					
					h4("4.3 Nascite"),
					p("Mostra gli animali nati in province non indenni per le malattie considerate."),
					
					h4("4.4 Output"),
					p("Contiene il dataset completo con tutti i dati animali e lo stato sanitario delle malattie, scaricabile in Excel."),
					
					h3("5. Download"),
					p("Ogni tab con tabelle permette il download dei dati in formato Excel."),
					
					hr(),
					h3("6. Note tecniche"),
					tags$ul(
						tags$li("TRUE = zona indenne (disease-free)"),
						tags$li("FALSE = zona non indenne"),
						tags$li("Le malattie sono filtrate in base alla data di validità")
					)
				)
			)
			
			# =====================================================================
			# TAB DINAMICI: Controllo Manuale, Provenienze, Nascite
			# =====================================================================
			# Questi tab vengono inseriti dinamicamente dal server solo quando:
			# - Controllo Manuale: ci sono animali con dati geografici non validi
			# - Provenienze/Nascite: dopo il caricamento di un file valido
			# Vedi app_server.R per la logica di inserimento dinamico
		)
	)
}