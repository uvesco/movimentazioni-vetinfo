# =============================================================================
# UI PRINCIPALE DELL'APPLICAZIONE
# =============================================================================
# Questo file definisce l'interfaccia utente dell'applicazione Shiny.
# L'app è organizzata in tab per gestire diverse funzionalità:
# - Input: caricamento file movimentazioni
# - Help: documentazione e guida utente (sempre visibile)
# - Tab dinamici: Controllo Manuale, Provenienze, Nascite, Dataset
#   (mostrati solo quando necessario dopo il caricamento dati) 
# =============================================================================

app_ui <- function() {
	
	fluidPage(
		# Titolo principale dell'applicazione
		titlePanel("Elaborazione movimentazioni da BDN"),
		
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
						mod_upload_movimentazioni_ui("upload_mov"),
						div(
							style = "white-space: pre-wrap; word-wrap: break-word;",
							textOutput("tipo_file")
						)
					),
					# Pannello principale con informazioni sul file caricato
					mainPanel(
						uiOutput("n_animali"),                 # Conteggio animali
						uiOutput("titolo_malattie"),           # Titolo sezione malattie
						tableOutput("malattie_importate"),     # Tabella malattie rilevanti
						uiOutput("riepilogo_controlli")
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
				fluidRow(
					column(
						3,
						div(
							class = "small",
							h4("Indice"),
							tags$ul(
								tags$li(tags$a(href = "#help-introduzione", "Introduzione")),
								tags$li(tags$a(href = "#help-caricamento", "Caricamento file")),
								tags$li(tags$a(href = "#help-elaborazione", "Elaborazione dati")),
								tags$li(tags$a(href = "#help-risultati", "Tab risultati")),
								tags$li(tags$a(href = "#help-download", "Download")),
								tags$li(tags$a(href = "#help-note", "Note tecniche")),
								tags$li(tags$a(href = "#help-disclaimer", "Disclaimer")),
								tags$li(tags$a(href = "#help-crediti", "Crediti"))
							)
						)
					),
					column(
						9,
						div(
							class = "container-fluid",
							style = "max-width: 900px; padding: 20px;",
							
							h2(id = "help-guida", "Guida all'utilizzo"),
							hr(),
							
							h3(id = "help-introduzione", "1. Introduzione"),
							p("L'applicazione permette di elaborare le movimentazioni di animali direttamente dai files esportati dalla BDN (Banca Dati Nazionale) al fine di identificare animali provenienti o nati in zone non indenni per determinate malattie.",
							  "L'obiettivo è facilitare il controllo sanitario degli animali movimentati, consentendo di individuare rapidamente eventuali rischi associati alla loro origine geografica.",
							  "L'applicazione consente di caricare file Excel contenenti le movimentazioni, elaborare i dati per classificare gli animali in base alla loro origine (Italia o Estero),",
							  "estrarre informazioni geografiche rilevanti (comune di provenienza e provincia di nascita) 
							   e verificare lo stato sanitario delle zone di provenienza e nascita degli animali."),
							h3(id = "help-download-bdn", "2. Download da BDN"),
							
							p(
								"I dati possono essere scaricati indifferentemente dagli applicativi BDN di specie o dall'applicativo Interrogazione BDN, ",
								"seguono i passaggi per l'interrogazione dell'applicativo Interrogazione BDN:"
							),
							
							tags$ul(
								tags$li("Accedere all'applicativo Interrogazione BDN"),
								tags$li("Scegliere ", tags$strong("Dati")),
								tags$li("Scegliere ", tags$strong("Estrazione Dati")),
								tags$li(
									"Scegliere tra ",
									tags$strong("Bovini"),
									" e ",
									tags$strong("Ovini e Caprini")
								),
								tags$li("Scegliere ", tags$strong("Dati sugli animali")),
								tags$li(
									"Scegliere ",
									tags$strong("Movimentazioni di capi bovini e bufalini"),
									" (a oggi c'è un errore e la dicitura è la stessa anche per ovini e caprini)"
								),
								tags$li("Impostare i filtri desiderati per le movimentazioni"),
								tags$li(
									"Selezionare Stampa EXCEL e premere il pulsante ",
									tags$strong("Invio"),
									" per scaricare il file"
								)
							),
							
							   
							h3(id = "help-caricamento", "3. Caricamento File"),
							h4("2.1 Formati supportati"),
							tags$ul(
								tags$li("File Excel .xls (formato originale BDN)"),
								tags$li("File compressi .gz (file .xls compressi come da BDN)")
							),
							
							h4("2.2 Gruppi specie supportati"),
							tags$ul(
								tags$li("Bovini e bufalini"),
								tags$li("Ovicaprini")
							),
							
							h3(id = "help-elaborazione", "4. Elaborazione Dati"),
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
							
							h3(id = "help-risultati", "5. Tab Risultati"),
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
							
							h4("4.4 Dataset"),
							p("Contiene il dataset completo con tutti i dati animali e lo stato sanitario delle malattie, scaricabile in Excel."),
							
							h3(id = "help-download", "6. Download"),
							p("Ogni tab con tabelle permette il download dei dati in formato Excel."),
							
							hr(),
							h3(id = "help-note", "7. Note tecniche"),
							tags$ul(
								tags$li("TRUE = zona indenne (disease-free)"),
								tags$li("FALSE = zona non indenne"),
								tags$li("Le malattie sono filtrate in base alla data di validità")
							),
							
							hr(),
							h3(id = "help-disclaimer", "8. Disclaimer"),
							p("L'uso dell'applicazione è a rischio e pericolo dell'utente e non si forniscono garanzie. Si invita a verificare il dataset collegato e l'aggiornamento dei files di indennità delle province."),
							hr(),
							h3(id = "help-crediti", "9. Crediti"),
							p("Umberto Vesco (ASLTO3). Codice disponibile con licenza GNU GPL 3.0."),
							tags$a(href = "https://github.com/uvesco/movimentazioni-vetinfo", target = "_blank", "Repository GitHub")
						)
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
