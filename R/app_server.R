# =============================================================================
# SERVER PRINCIPALE DELL'APPLICAZIONE (orchestratore)
# =============================================================================
# In modalità "doppia" il server è un orchestratore sottile:
# 1. mod_upload_movimentazioni_server: legge i file e li smista per gruppo
# 2. mod_import_malattie: carica i dati malattie (condivisi tra i gruppi)
# 3. mod_specie_server: istanziato due volte (bovini, ovicaprini); contiene
#    pipeline + tutti gli output (sommari, dataset, non indenni, controllo manuale)
# 4. Un observer riconciliante inserisce/rimuove i due tab di primo livello per
#    specie (compaiono indipendentemente, appena il relativo file è caricato)
# 5. Pagina Input: bottone email (mailto) + download capi problematici
# =============================================================================

# -----------------------------------------------------------------------------
# Helper: costruisce un URL mailto: con subject e body precompilati.
# Limita la lunghezza dell'URL encoded (~1900) per compatibilità con i client
# (es. Outlook tronca intorno ai 2048 caratteri di riga di comando).
# Subject e body sono codificati separatamente; i caratteri strutturali restano
# letterali. enc2utf8 garantisce escape UTF-8 corretti per gli accenti.
# -----------------------------------------------------------------------------
.build_mailto_url <- function(subject, body, max_url = 1900) {
	enc_subject <- utils::URLencode(enc2utf8(subject), reserved = TRUE)
	build <- function(b) {
		paste0(
			"mailto:?subject=", enc_subject,
			"&body=", utils::URLencode(enc2utf8(b), reserved = TRUE)
		)
	}
	url <- build(body)
	if (nchar(url) <= max_url) {
		return(url)
	}
	# Tronca il corpo (sul testo grezzo) finché l'URL encoded rientra nel limite
	nota <- "\r\n\r\n…(riassunto troncato)"
	lo <- 0L
	hi <- nchar(body)
	while (lo < hi) {
		mid <- as.integer((lo + hi + 1) %/% 2)
		cand <- paste0(substr(body, 1, mid), nota)
		if (nchar(build(cand)) <= max_url) lo <- mid else hi <- mid - 1L
	}
	build(paste0(substr(body, 1, lo), nota))
}

app_server <- function(input, output, session) {

	# =====================================================================
	# SEZIONE 1: IMPORTAZIONE DATI
	# =====================================================================

	# Modulo upload: legge i file e li smista per gruppo (bovini/ovicaprini)
	up <- mod_upload_movimentazioni_server("upload_mov")

	# Modulo malattie: status sanitario per provincia/comune (condiviso, 1 istanza)
	malattie <- mod_import_malattie("df_standard")

	# Dati e gruppo per ciascuna specie, derivati dalla lista per gruppo
	animali_bovini <- reactive(up$animali_per_gruppo()[["bovini"]])
	gruppo_bovini <- reactive({
		d <- up$animali_per_gruppo()[["bovini"]]
		if (!is.null(d) && nrow(d) > 0) "bovini" else NULL
	})
	animali_ovicaprini <- reactive(up$animali_per_gruppo()[["ovicaprini"]])
	gruppo_ovicaprini <- reactive({
		d <- up$animali_per_gruppo()[["ovicaprini"]]
		if (!is.null(d) && nrow(d) > 0) "ovicaprini" else NULL
	})

	# =====================================================================
	# SEZIONE 2: MODULI PER SPECIE (istanziati una volta, sempre attivi)
	# =====================================================================
	b <- mod_specie_server(
		"specie_bovini",
		animali = animali_bovini,
		gruppo = gruppo_bovini,
		malattie = malattie
	)
	o <- mod_specie_server(
		"specie_ovicaprini",
		animali = animali_ovicaprini,
		gruppo = gruppo_ovicaprini,
		malattie = malattie
	)

	# =====================================================================
	# SEZIONE 3: TAB DI PRIMO LIVELLO PER SPECIE
	# =====================================================================
	# Ogni tab compare appena il relativo file è caricato (indipendentemente
	# dall'altro). Ordine mantenuto: Input | Bovini | Ovicaprini | Help.
	tab_b_presente <- reactiveVal(FALSE)
	tab_o_presente <- reactiveVal(FALSE)

	observe({
		bl <- isTRUE(b$loaded())
		ol <- isTRUE(o$loaded())
		isolate({
			# Bovini: subito dopo "input"
			if (bl && !tab_b_presente()) {
				insertTab(
					inputId = "tabs",
					tab = tabPanel(
						title = "Bovini",
						value = "tab_specie_bovini",
						mod_specie_tabs_ui("specie_bovini")
					),
					target = "input",
					position = "after"
				)
				tab_b_presente(TRUE)
			} else if (!bl && tab_b_presente()) {
				removeTab("tabs", "tab_specie_bovini")
				tab_b_presente(FALSE)
			}

			# Ovicaprini: dopo Bovini se presente, altrimenti dopo "input"
			if (ol && !tab_o_presente()) {
				tgt <- if (tab_b_presente()) "tab_specie_bovini" else "input"
				insertTab(
					inputId = "tabs",
					tab = tabPanel(
						title = "Ovicaprini",
						value = "tab_specie_ovicaprini",
						mod_specie_tabs_ui("specie_ovicaprini")
					),
					target = tgt,
					position = "after"
				)
				tab_o_presente(TRUE)
			} else if (!ol && tab_o_presente()) {
				removeTab("tabs", "tab_specie_ovicaprini")
				tab_o_presente(FALSE)
			}
		})
	})

	# =====================================================================
	# SEZIONE 4: STATO UPLOAD (pagina Input)
	# =====================================================================
	output$tipo_file <- renderText({
		pg <- up$animali_per_gruppo()
		righe <- character(0)
		for (g in c("bovini", "ovicaprini")) {
			df <- pg[[g]]
			etichetta <- if (g == "bovini") "Bovini" else "Ovicaprini"
			if (is.null(df)) {
				next
			} else if (nrow(df) == 0) {
				righe <- c(righe, paste0(etichetta, ": file vuoto per i parametri selezionati"))
			} else {
				righe <- c(righe, paste0(etichetta, ": ", nrow(df), " capi importati"))
			}
		}
		stato <- up$status()
		if (!is.null(stato$message) && stato$type %in% c("error", "empty")) {
			righe <- c(righe, stato$message)
		}
		if (length(righe) == 0) {
			return("File non ancora caricato")
		}
		paste(righe, collapse = "\n")
	})

	# =====================================================================
	# SEZIONE 5: BOTTONE EMAIL (mailto) + DOWNLOAD CAPI PROBLEMATICI
	# =====================================================================

	# Bottone email: apre il client con oggetto e corpo (riassunto) precompilati.
	output$email_link <- renderUI({
		blocchi <- list()
		bt <- b$email_text()
		ot <- o$email_text()
		if (length(bt) > 0) blocchi[[length(blocchi) + 1]] <- bt
		if (length(ot) > 0) blocchi[[length(blocchi) + 1]] <- ot

		# Nessun gruppo caricato: bottone disabilitato
		if (length(blocchi) == 0) {
			return(tags$button(
				class = "btn btn-secondary",
				disabled = NA,
				icon("envelope"), " Invia email riepilogo"
			))
		}

		corpo_righe <- character(0)
		for (i in seq_along(blocchi)) {
			if (i > 1) corpo_righe <- c(corpo_righe, "")
			corpo_righe <- c(corpo_righe, blocchi[[i]])
		}
		corpo_righe <- c(
			corpo_righe, "", "—",
			"Riepilogo generato con l'app movimentazioni-vetinfo."
		)

		subject <- paste0("Movimentazioni BDN - controlli del ", format(Sys.Date(), "%d/%m/%Y"))
		body <- paste(corpo_righe, collapse = "\r\n")
		url <- .build_mailto_url(subject, body)

		tags$a(
			href = url,
			class = "btn btn-primary",
			icon("envelope"), " Invia email riepilogo"
		)
	})

	# Bottone "Scarica capi problematici": visibile solo se ce ne sono.
	output$ui_capi_problematici <- renderUI({
		nb <- tryCatch(b$n_problematici(), error = function(e) 0)
		no <- tryCatch(o$n_problematici(), error = function(e) 0)
		if (is.null(nb)) nb <- 0
		if (is.null(no)) no <- 0
		n_tot <- nb + no
		if (n_tot == 0) {
			return(NULL)
		}

		downloadButton(
			"download_capi_problematici",
			paste0("Scarica capi problematici (", n_tot, ")"),
			icon = icon("triangle-exclamation"),
			class = "btn-warning"
		)
	})

	# Excel multi-foglio (un foglio per gruppo con capi problematici)
	output$download_capi_problematici <- downloadHandler(
		filename = function() {
			paste0("capi_problematici_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
		},
		content = function(file) {
			fogli <- list()
			db <- tryCatch(b$capi_problematici_df(), error = function(e) NULL)
			do_ <- tryCatch(o$capi_problematici_df(), error = function(e) NULL)
			if (!is.null(db) && nrow(db) > 0) fogli[["bovini"]] <- db
			if (!is.null(do_) && nrow(do_) > 0) fogli[["ovicaprini"]] <- do_
			if (length(fogli) == 0) {
				fogli[["nessun_capo_problematico"]] <- data.frame(
					messaggio = "Nessun capo problematico individuato"
				)
			}
			openxlsx::write.xlsx(fogli, file)
		}
	)
}
