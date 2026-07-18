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

# -----------------------------------------------------------------------------
# Helper: corpo HTML dell'email riepilogo (per il file .eml).
# `riepiloghi` = lista di liste come da mod_specie riepilogo_email() (non NULL).
# Colori: verde per esiti a zero, rosso per valori > 0, arancio per i capi da
# controllare manualmente (testo da aggiornare a mano dopo la verifica).
# Stili inline: i client di posta ignorano i fogli di stile.
# -----------------------------------------------------------------------------
.build_email_html <- function(riepiloghi) {
	esc <- htmltools::htmlEscape
	verde <- "#1a7f37"
	rosso <- "#c1121f"
	arancio <- "#b35c00"

	riga_stato <- function(testo, valore) {
		colore <- if (isTRUE(valore > 0)) rosso else verde
		paste0('<div style="color:', colore, '; font-weight: bold; margin: 2px 0;">',
			esc(testo), ' ', valore, '</div>')
	}

	tabella_html <- function(df, max_righe = 100) {
		n_tot <- nrow(df)
		df <- utils::head(df, max_righe)
		celle <- function(valori, tag, extra = "") {
			paste0("<", tag, ' style="border: 1px solid #999; padding: 3px 6px; ',
				extra, '">', esc(ifelse(is.na(valori), "", as.character(valori))),
				"</", tag, ">", collapse = "")
		}
		testata <- paste0("<tr>", celle(names(df), "th",
			"background: #f8d7da; text-align: left;"), "</tr>")
		righe <- vapply(seq_len(nrow(df)), function(i) {
			paste0("<tr>", celle(unlist(df[i, ], use.names = FALSE), "td"), "</tr>")
		}, character(1))
		nota <- if (n_tot > max_righe) {
			paste0('<div style="font-size: 12px; color: #555;">Mostrate le prime ',
				max_righe, " righe di ", n_tot,
				": dettaglio completo nel file allegato.</div>")
		} else ""
		paste0('<table style="border-collapse: collapse; font-size: 12px; margin: 6px 0;">',
			testata, paste(righe, collapse = ""), "</table>", nota)
	}

	blocco_specie <- function(r) {
		righe_capi <- paste0("Totale: ", r$totale, " capi<br/>")
		if (!is.na(r$n_nazionale)) {
			righe_capi <- paste0(
				righe_capi,
				"Provenienza nazionale: ", r$n_nazionale, " capi (", r$lotti_nazionale, " lotti)<br/>",
				"Provenienza estera: ", r$n_estero, " capi (", r$lotti_estero, " lotti)<br/>"
			)
		}

		controlli <- paste0(
			riga_stato(paste("Animali nati in province non indenni", r$sigle, ":"),
				r$nati_non_indenni),
			riga_stato(paste("Animali provenienti da province non indenni", r$sigle, ":"),
				r$provenienti_non_indenni)
		)
		man_tot <- r$manuale_nascita + r$manuale_provenienza
		if (man_tot > 0) {
			controlli <- paste0(
				controlli,
				'<div style="color: ', arancio, '; font-weight: bold; margin: 2px 0;">',
				"Animali da controllare manualmente (nascita: ", r$manuale_nascita,
				", provenienza: ", r$manuale_provenienza, ") ",
				"&#8212; ESITO DA VERIFICARE: aggiornare questo testo dopo il controllo in BDN.",
				"</div>"
			)
		}

		probl_html <- ""
		if (!is.null(r$problematici) && nrow(r$problematici) > 0) {
			probl_html <- paste0(
				'<div style="margin-top: 6px; font-weight: bold;">Capi problematici (',
				r$etichetta, "):</div>",
				tabella_html(r$problematici)
			)
		}

		paste0(
			'<h2 style="margin: 14px 0 4px 0;">', esc(r$etichetta), "</h2>",
			'<h3 style="margin: 8px 0 2px 0;">Periodo</h3>',
			"Data prima movimentazione: ", esc(r$data_inizio), "<br/>",
			"Data ultima movimentazione: ", esc(r$data_fine), "<br/>",
			'<h3 style="margin: 8px 0 2px 0;">Capi movimentati</h3>',
			righe_capi,
			'<h3 style="margin: 8px 0 2px 0;">Riepilogo controlli</h3>',
			controlli,
			probl_html
		)
	}

	blocchi <- vapply(riepiloghi, blocco_specie, character(1))
	paste0(
		'<html><body style="font-family: Arial, Helvetica, sans-serif; font-size: 14px; color: #222;">',
		paste(blocchi, collapse = '<hr style="border: none; border-top: 1px solid #ccc; margin: 12px 0;"/>'),
		'<hr style="border: none; border-top: 1px solid #ccc; margin: 12px 0;"/>',
		'<p style="font-size: 12px; color: #555;">Dati elaborati con l&#39;applicazione ',
		'movimentazioni-vetinfo ( https://github.com/uvesco/movimentazioni-vetinfo ).</p>',
		"</body></html>"
	)
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

	# Fogli xlsx dei capi problematici (un foglio per gruppo), condivisi tra il
	# download diretto e l'allegato dell'email .eml
	fogli_problematici <- function() {
		fogli <- list()
		db <- tryCatch(b$capi_problematici_df(), error = function(e) NULL)
		do_ <- tryCatch(o$capi_problematici_df(), error = function(e) NULL)
		if (!is.null(db) && nrow(db) > 0) fogli[["bovini"]] <- db
		if (!is.null(do_) && nrow(do_) > 0) fogli[["ovicaprini"]] <- do_
		fogli
	}

	# Excel multi-foglio (un foglio per gruppo con capi problematici)
	output$download_capi_problematici <- downloadHandler(
		filename = function() {
			paste0("capi_problematici_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
		},
		content = function(file) {
			fogli <- fogli_problematici()
			if (length(fogli) == 0) {
				fogli[["nessun_capo_problematico"]] <- data.frame(
					messaggio = "Nessun capo problematico individuato"
				)
			}
			openxlsx::write.xlsx(fogli, file)
		}
	)

	# =====================================================================
	# SEZIONE 6: EMAIL .EML (corpo HTML + allegato capi problematici)
	# =====================================================================

	# Bottone: attivo solo se almeno un gruppo e' caricato
	output$ui_email_eml <- renderUI({
		attivo <- isTRUE(b$loaded()) || isTRUE(o$loaded())
		if (!attivo) {
			return(tags$button(
				class = "btn btn-secondary",
				disabled = NA,
				icon("envelope-open-text"), " Prepara email (.eml)"
			))
		}
		actionButton(
			"prepara_eml",
			label = tagList(icon("envelope-open-text"), " Prepara email (.eml)"),
			class = "btn btn-primary"
		)
	})

	# Al click: modal con l'esito dei controlli; se ci sono capi da controllare
	# manualmente, avvisa che il testo dell'email va aggiornato a mano dopo la
	# verifica in BDN. Il download vero e proprio avviene dal bottone nel modal.
	observeEvent(input$prepara_eml, {
		rrs <- Filter(Negate(is.null), list(b$riepilogo_email(), o$riepilogo_email()))
		if (length(rrs) == 0) return()

		ni_tot <- sum(vapply(rrs, function(r) r$nati_non_indenni + r$provenienti_non_indenni, numeric(1)))
		man_tot <- sum(vapply(rrs, function(r) r$manuale_nascita + r$manuale_provenienza, numeric(1)))
		n_probl <- sum(vapply(rrs, function(r) {
			if (is.null(r$problematici)) 0L else nrow(r$problematici)
		}, integer(1)))

		corpo <- tagList(
			if (ni_tot == 0 && man_tot == 0) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Nessun capo problematico: "),
					"l'email conterrà solo il testo riassuntivo (senza allegati)."
				)
			},
			if (ni_tot > 0) {
				div(
					class = "alert alert-danger",
					icon("exclamation-triangle"),
					strong(paste0(" ", ni_tot, " animali provenienti/nati in zone non indenni: ")),
					"l'email includerà la tabella dei capi problematici nel corpo e il file Excel in allegato."
				)
			},
			if (man_tot > 0) {
				div(
					class = "alert alert-warning",
					icon("triangle-exclamation"),
					strong(" Attenzione: "), man_tot,
					" animali da controllare manualmente (dati geografici non mappabili). ",
					"Il testo dell'email li segnala come ", strong("ESITO DA VERIFICARE"), ": ",
					"dopo la verifica in BDN ", strong("modificare a mano il testo"),
					" prima dell'invio."
				)
			},
			if (n_probl > 0 && ni_tot == 0) {
				p("I capi da controllare manualmente saranno comunque inclusi nella tabella e nell'allegato.")
			},
			p(tags$small(
				"Il file .eml si apre nel client di posta come bozza modificabile ",
				"(Outlook lo apre pronto all'invio; in Thunderbird usare ",
				tags$em("Messaggio → Modifica come nuovo messaggio"), ")."
			))
		)

		showModal(modalDialog(
			title = "Prepara email riepilogo (.eml)",
			corpo,
			footer = tagList(
				modalButton("Annulla"),
				downloadButton("download_eml", "Scarica .eml", class = "btn-primary")
			),
			easyClose = TRUE
		))
	})

	output$download_eml <- downloadHandler(
		filename = function() {
			paste0("riepilogo_movimentazioni_", format(Sys.Date(), "%Y%m%d"), ".eml")
		},
		content = function(file) {
			rrs <- Filter(Negate(is.null), list(b$riepilogo_email(), o$riepilogo_email()))
			if (length(rrs) == 0) {
				stop("Nessun dato disponibile per l'email.")
			}

			html <- .build_email_html(rrs)
			subject <- paste0("Movimentazioni BDN - controlli del ", format(Sys.Date(), "%d/%m/%Y"))

			allegati <- list()
			fogli <- fogli_problematici()
			tmp_xlsx <- NULL
			if (length(fogli) > 0) {
				tmp_xlsx <- tempfile(fileext = ".xlsx")
				openxlsx::write.xlsx(fogli, tmp_xlsx)
				allegati <- list(list(
					path = tmp_xlsx,
					name = paste0("capi_problematici_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
				))
			}

			.write_eml(.build_eml(subject, html, allegati), file)
			if (!is.null(tmp_xlsx)) unlink(tmp_xlsx)
			removeModal()
		}
	)
}
