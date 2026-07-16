# =============================================================================
# UI PRINCIPALE DELL'APPLICAZIONE
# =============================================================================
# Questo file definisce l'interfaccia utente dell'applicazione Shiny.
# L'app è organizzata in tab per gestire diverse funzionalità:
# - Input: caricamento file movimentazioni (uno o più, anche di gruppi diversi) +
#   riepiloghi per gruppo e bottone email / download capi problematici (comune)
# - Help: documentazione e guida utente (sempre visibile)
# - Tab dinamici per gruppo di specie (Bovini, Ovicaprini): inseriti dal server
#   appena il relativo file è caricato; ognuno contiene i sottotab Sommario,
#   Dataset, Non Indenni, Controllo Manuale (vedi R/mod_specie.R)
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
					# Pannello principale: azioni (email/download) + riepiloghi per gruppo
					mainPanel(
						# Riga azioni: invio email riepilogo + (se presenti) capi problematici
						div(
							class = "d-flex gap-2 my-3 flex-wrap align-items-center",
							uiOutput("email_link", inline = TRUE),
							uiOutput("ui_capi_problematici", inline = TRUE)
						),
						# Riepilogo per gruppo di specie (renderizzato solo se caricato)
						mod_specie_summary_ui("specie_bovini"),
						mod_specie_summary_ui("specie_ovicaprini")
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
								tags$li(tags$a(href = "#help-download-bdn", "Download BDN")),
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

							h4("2.1 Download manuale (applicativo Interrogazione BDN)"),
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

							h4("2.2 Download con bookmarklet (opzione alternativa)"),
							p(
								"In alternativa alla navigazione manuale è possibile utilizzare dei bookmarklet che aprono ",
								"direttamente il form di estrazione Vetinfo pre-compilato ",
								"(filtro movimentazioni, tipo report, formato EXCEL, data AL = oggi). ",
								"È sufficiente impostare solo la ", tags$strong("data DAL"), " e cliccare Invio."
							),
							p(
								"Per ciascuna specie sono disponibili due varianti: ",
								tags$strong("tutte le movimentazioni in ingresso"), " oppure ",
								tags$strong("solo quelle in provenienza da altre regioni"), "."
							),
							p(
								"Prerequisito: essere autenticati su Vetinfo (SPID/CIE) ed essere entrati ",
								"nell'", tags$strong("applicativo della specie"),
								" (Bovini e Bufalini / Ovini e Caprini) con il proprio ruolo, altrimenti ",
								"la pagina del form risponde \"RUOLO NON ASSOCIATO ALL'UTENTE\"."
							),
							p(tags$em(
								"Trascinare i pulsanti nella barra dei preferiti del browser, oppure usare ",
								tags$strong("Copia codice"), " e incollare il codice come URL di un nuovo segnalibro. ",
								"Cliccato dalla pagina del form, il segnalibro lo pre-compila direttamente; ",
								"cliccato da qualsiasi altra pagina apre il form (stessa scheda se già su ",
								"Vetinfo, nuova scheda altrimenti): una volta caricato, ricliccare il ",
								"segnalibro per pre-compilarlo."
							)),
							p(tags$small(
								"Nota: con la variante \"solo da altre regioni\" sul form Vetinfo risulta ",
								"selezionata l'opzione \"Solo movimentazioni verso altre regioni\": ",
								"l'etichetta del form è unica per ingressi e uscite e, per il report degli ",
								"ingressi, equivale alla provenienza da altre regioni."
							)),
							.vetinfo_bookmarklet_block("help_bm"),

							h3(id = "help-caricamento", "3. Caricamento File"),
							h4("3.1 Formati supportati"),
							tags$ul(
								tags$li("File Excel .xls (formato originale BDN)"),
								tags$li("File compressi .gz (file .xls compressi come da BDN)")
							),
							
							h4("3.2 Caricamento multiplo e doppio gruppo"),
							p("È possibile caricare più file contemporaneamente selezionandoli nella finestra di dialogo, ",
							  "anche di gruppi di specie diversi. I file vengono riconosciuti e smistati automaticamente: ",
							  "quelli dello stesso gruppo vengono uniti, mentre bovini e ovicaprini vengono elaborati ",
							  "separatamente."),
							p("Per ogni gruppo caricato compare un tab di primo livello dedicato (",
							  tags$strong("Bovini"), " e/o ", tags$strong("Ovicaprini"), "), ciascuno con i propri sottotab ",
							  "(Sommario, Dataset, Non Indenni, Controllo Manuale). Il tab di una specie compare appena il ",
							  "relativo file è caricato, indipendentemente dall'altra."),

							h4("3.3 Gruppi specie supportati"),
							tags$ul(
								tags$li("Bovini e bufalini"),
								tags$li("Ovicaprini")
							),
							
							h3(id = "help-elaborazione", "4. Elaborazione Dati"),
							h4("4.1 Classificazione origine"),
							p("Gli animali vengono classificati come 'Italia' o 'Estero' basandosi su:"),
							tags$ul(
								tags$li("Prefisso 'IT' nel marchio auricolare"),
								tags$li("Motivo di ingresso nella tabella decodifiche")
							),
							
							h4("4.2 Estrazione dati geografici"),
							p("Per ogni animale vengono estratti:"),
							tags$ul(
								tags$li("Provincia di nascita: dalle prime 3 cifre del marchio auricolare"),
								tags$li("Comune di provenienza: dal codice stabilimento di origine")
							),
							
							h4("4.3 Incrocio con dati malattie"),
							p("I dati geografici vengono incrociati con le tabelle delle malattie per verificare:"),
							tags$ul(
								tags$li("Stato sanitario del comune di provenienza (prefisso prov_)"),
								tags$li("Stato sanitario della provincia di nascita (prefisso nascita_)")
							),
							
							h3(id = "help-risultati", "5. Tab Risultati"),
							p("I risultati sono organizzati in un tab di primo livello per ogni gruppo di specie caricato (",
							  tags$strong("Bovini"), ", ", tags$strong("Ovicaprini"), "). Dentro a ciascun tab di specie si trovano ",
							  "i sottotab descritti di seguito."),
							h4("5.1 Controllo Manuale"),
							p("Mostra gli animali italiani per cui non è stato possibile identificare:"),
							tags$ul(
								tags$li("Il comune di provenienza (codice stabilimento non valido)"),
								tags$li("La provincia di nascita (marchio auricolare non mappabile)")
							),
							
							h4("5.2 Non Indenni"),
							p("Tab unificato che mostra gli animali provenienti o nati in zone non indenni per le malattie considerate:"),
							tags$ul(
								tags$li("Diagnostica riepilogativa con conteggio animali a rischio"),
								tags$li("Tabella riepilogativa per stabilimento di destinazione"),
								tags$li("Download file BDN per tutti gli animali a rischio"),
								tags$li("Sezione Provenienze con download per malattia"),
								tags$li("Sezione Nascite con download per malattia")
							),
							
							h4("5.3 Dataset"),
							p("Contiene il dataset completo con tutti i dati animali e lo stato sanitario delle malattie, scaricabile in Excel."),

							h4("5.4 Invio email riepilogo"),
							p("Nella pagina ", tags$strong("Input"), " il pulsante ", tags$strong("Invia email riepilogo"),
							  " apre il client di posta dell'utente con un'email precompilata (oggetto e corpo) contenente il ",
							  "riassunto dei controlli per i gruppi caricati. Serve tipicamente a comunicare l'esito negativo, ",
							  "cioè a documentare di aver effettuato il controllo, senza bisogno di allegati."),
							p("Quando sono presenti capi problematici (positività, evento raro), accanto al pulsante email compare ",
							  tags$strong("Scarica capi problematici"), ": scarica un file Excel (un foglio per gruppo) con gli ",
							  "animali provenienti/nati in zone non indenni o da controllare manualmente, da commentare e allegare a mano."),

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
							tags$a(href = "https://github.com/uvesco/movimentazioni-vetinfo", target = "_blank", "Repository GitHub"),
							hr(),
							h4("Citazione suggerita"),
							tags$blockquote(
								style = "background-color: #f8f9fa; border-left: 4px solid #6c757d; padding: 12px 16px; margin: 10px 0; font-family: monospace; font-size: 0.95em; user-select: all; cursor: text;",
								"Dati elaborati con l'applicazione movimentazioni-vetinfo ( https://github.com/uvesco/movimentazioni-vetinfo )."
							)
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
		),
		
		# =====================================================================
		# FOOTER: Informazioni autore, licenza e disclaimer
		# =====================================================================
		tags$footer(
			style = "background-color: #f5f5f5; border-top: 1px solid #ddd; padding: 15px; margin-top: 30px; text-align: center; font-size: 12px; color: #666;",
			tags$p(
				style = "margin: 5px 0;",
				tags$strong("Autore:"), " Umberto Vesco (ASLTO3)"
			),
			tags$p(
				style = "margin: 5px 0;",
				"Questa applicazione è rilasciata sotto licenza ", 
				tags$strong("GNU GPL 3.0"),
				" - ",
				tags$a(href = "https://www.gnu.org/licenses/gpl-3.0.html", target = "_blank", "Maggiori informazioni")
			),
			tags$p(
				style = "margin: 5px 0; font-style: italic;",
				tags$strong("Disclaimer:"), " Questa applicazione è fornita SENZA ALCUNA GARANZIA. ",
				"L'uso è a proprio rischio e pericolo. Si invita a verificare i dati e i risultati ottenuti."
			),
			tags$p(
				style = "margin: 5px 0;",
				tags$a(href = "https://github.com/uvesco/movimentazioni-vetinfo", target = "_blank", "Repository GitHub")
			)
		)
	)
}
