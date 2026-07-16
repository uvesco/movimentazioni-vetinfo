# =============================================================================
# MODULO PER-SPECIE: ELABORAZIONE E OUTPUT DI UN GRUPPO DI SPECIE
# =============================================================================
# Questo modulo incapsula tutta la logica e gli output relativi a UN gruppo di
# specie (bovini OPPURE ovicaprini). Viene istanziato due volte dal server
# principale per lavorare "in doppio".
#
# Contiene:
# - mod_specie_server(): pipeline controlli + tutti gli output (sommari, dataset,
#   non indenni, controllo manuale). Espone reactive per la pagina Input e per
#   il bottone email (loaded, email_text, capi_problematici_df, n_problematici).
# - mod_specie_tabs_ui(): i 4 sottotab (Sommario/Dataset/Non Indenni/Controllo
#   Manuale) da inserire dentro il tab di primo livello della specie.
# - mod_specie_summary_ui(): i riepiloghi da mostrare nella pagina Input comune.
# =============================================================================

# ---- UI: sottotab del tab di specie ----------------------------------------
mod_specie_tabs_ui <- function(id) {
	ns <- NS(id)
	tabsetPanel(
		id = ns("subtabs"),

		# Sottotab Sommario
		tabPanel(
			title = "Sommario",
			value = "sommario",
			h3("Provenienza"),
			h4("Internazionali"),
			tableOutput(ns("sommario_internazionali")),
			h4("Regioni"),
			tableOutput(ns("sommario_provenienze_regioni")),
			h4("Province"),
			tableOutput(ns("sommario_provenienze_province")),
			hr(),
			h3("Nascita"),
			h4("Paesi"),
			tableOutput(ns("sommario_nascita_paesi")),
			h4("Province"),
			tableOutput(ns("sommario_nascita_province")),
			hr(),
			h3("Destinazioni"),
			tableOutput(ns("sommario_importatori"))
		),

		# Sottotab Dataset
		tabPanel(
			title = "Dataset",
			value = "dataset",
			h3("Dataset completo movimentazioni"),
			p("Download del dataset elaborato in formato Excel."),
			downloadButton(ns("download_dataset"), "Scarica dataset elaborato (.xlsx)")
		),

		# Sottotab Non Indenni
		tabPanel(
			title = "Non Indenni",
			value = "non_indenni",
			uiOutput(ns("ui_diagnostica_non_indenni")),
			hr(),
			h3("Riepilogo per stabilimento di destinazione"),
			p("Numero di animali a rischio per ogni stabilimento di destinazione, suddivisi per malattia e tipo di rischio (provenienza/nascita)."),
			tableOutput(ns("tabella_riepilogo_non_indenni")),
			uiOutput(ns("ui_bdn_non_indenni_all")),
			hr(),
			h3("Animali provenienti da zone non indenni"),
			p("Scarica il dataset filtrato per gli animali provenienti da comuni/zone non indenni per ciascuna malattia."),
			uiOutput(ns("ui_provenienze_download")),
			hr(),
			h3("Animali nati in zone non indenni"),
			p("Scarica il dataset filtrato per gli animali nati in province non indenni per ciascuna malattia."),
			uiOutput(ns("ui_nascite_download"))
		),

		# Sottotab Controllo Manuale
		tabPanel(
			title = "Controllo Manuale",
			value = "controllo_manuale",
			h3("Animali con dati geografici non validi"),
			p("Questa sezione mostra gli animali italiani per cui non è stato possibile identificare correttamente i dati geografici."),
			uiOutput(ns("ui_bdn_controllo_manuale")),
			hr(),
			h4("Animali di provenienza nazionale con codice stabilimento di origine non mappabile"),
			uiOutput(ns("ui_provenienza_non_trovata")),
			hr(),
			h4("Animali nati in Italia con provincia nel marchio auricolare non mappabile"),
			uiOutput(ns("ui_nascita_non_trovata"))
		)
	)
}

# ---- UI: riepilogo per la pagina Input -------------------------------------
mod_specie_summary_ui <- function(id) {
	ns <- NS(id)
	div(
		class = "specie-summary",
		uiOutput(ns("n_animali")),
		uiOutput(ns("riepilogo_controlli")),
		uiOutput(ns("titolo_malattie")),
		tableOutput(ns("malattie_importate"))
	)
}

# ---- Server ----------------------------------------------------------------
mod_specie_server <- function(id, animali, gruppo, malattie) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Pipeline controlli per questo gruppo
		pipeline <- mod_pipeline_controlli_server(
			"pipeline",
			animali = animali,
			gruppo = gruppo,
			malattie_data = malattie
		)

		# Helper: conta animali unici da una lista di dataframe
		conta_animali <- function(lista) {
			if (length(lista) == 0) {
				return(0)
			}
			ids <- unlist(lapply(lista, function(df_item) {
				if (!"capo_identificativo" %in% names(df_item)) {
					return(character(0))
				}
				df_item$capo_identificativo
			}))
			ids <- ids[!is.na(ids)]
			length(unique(ids))
		}

		# =====================================================================
		# OUTPUT SOMMARIO
		# =====================================================================

		output$sommario_internazionali <- renderTable({
			df <- pipeline$dati_processati()
			req(df)

			lot_id <- crea_lotto_id(df)
			is_italia <- df$orig_italia == TRUE
			is_estero <- df$orig_italia == FALSE

			animali_italia <- sum(is_italia, na.rm = TRUE)
			animali_estero <- sum(is_estero, na.rm = TRUE)
			animali_tot <- nrow(df)

			lotti_italia <- length(unique(lot_id[is_italia & !is.na(is_italia)]))
			lotti_estero <- length(unique(lot_id[is_estero & !is.na(is_estero)]))
			lotti_tot <- length(unique(lot_id))

			data.frame(
				Categoria = c("Italia", "Estero", "Totale"),
				Lotti = c(lotti_italia, lotti_estero, lotti_tot),
				Animali = c(animali_italia, animali_estero, animali_tot),
				check.names = FALSE
			)
		}, rownames = FALSE)

		output$sommario_provenienze_regioni <- renderTable({
			df <- pipeline$dati_processati()
			req(df)

			lot_id <- crea_lotto_id(df)
			regioni <- sort(unique(df_regioni$DEN_REG))
			animali_reg <- vapply(regioni, function(reg) {
				idx <- !is.na(df$orig_reg_nome) & df$orig_reg_nome == reg
				sum(idx)
			}, integer(1))
			lotti <- vapply(regioni, function(reg) {
				idx <- !is.na(df$orig_reg_nome) & df$orig_reg_nome == reg
				length(unique(lot_id[idx]))
			}, integer(1))

			data.frame(
				Regione = regioni,
				Lotti = lotti,
				Animali = animali_reg,
				check.names = FALSE
			)
		}, rownames = FALSE)

		output$sommario_provenienze_province <- renderTable({
			df <- pipeline$dati_processati()
			req(df)

			lot_id <- crea_lotto_id(df)
			province <- sort(unique(na.omit(df$orig_uts_nome)))
			if (length(province) == 0) {
				return(data.frame())
			}

			regioni <- tapply(df$orig_reg_nome, df$orig_uts_nome, function(x) {
				val <- unique(na.omit(x))
				if (length(val) == 0) NA else val[1]
			})

			animali_prov <- vapply(province, function(prov) {
				idx <- !is.na(df$orig_uts_nome) & df$orig_uts_nome == prov
				sum(idx)
			}, integer(1))
			lotti <- vapply(province, function(prov) {
				idx <- !is.na(df$orig_uts_nome) & df$orig_uts_nome == prov
				length(unique(lot_id[idx]))
			}, integer(1))

			risultato <- data.frame(
				Regione = unname(regioni[province]),
				Provincia = province,
				Lotti = lotti,
				Animali = animali_prov,
				check.names = FALSE
			)

			risultato[order(-risultato$Animali), , drop = FALSE]
		}, rownames = FALSE)

		output$sommario_nascita_paesi <- renderTable({
			df <- pipeline$dati_processati()
			req(df)

			totale_animali <- nrow(df)
			codici <- toupper(substr(as.character(df$capo_identificativo), 1, 2))
			codici[is.na(codici) | nchar(codici) < 2] <- "N/D"
			nomi <- df_stati$Descrizione[match(codici, df_stati$Codice)]
			paesi <- ifelse(!is.na(nomi), nomi, codici)

			conteggi <- sort(table(paesi), decreasing = TRUE)
			if (length(conteggi) == 0) {
				return(data.frame())
			}
			percentuali <- if (totale_animali > 0) round(100 * conteggi / totale_animali, 1) else 0

			data.frame(
				Paese = names(conteggi),
				Animali = as.integer(conteggi),
				Percentuale = paste0(percentuali, "%"),
				check.names = FALSE
			)
		}, rownames = FALSE)

		output$sommario_nascita_province <- renderTable({
			df <- pipeline$dati_processati()
			req(df)

			totale_animali <- nrow(df)
			province <- df$nascita_uts_nome
			province <- province[!is.na(province)]
			conteggi <- sort(table(province), decreasing = TRUE)
			if (length(conteggi) == 0) {
				return(data.frame())
			}
			percentuali <- if (totale_animali > 0) round(100 * conteggi / totale_animali, 1) else 0

			data.frame(
				Provincia = names(conteggi),
				Animali = as.integer(conteggi),
				Percentuale = paste0(percentuali, "%"),
				check.names = FALSE
			)
		}, rownames = FALSE)

		output$sommario_importatori <- renderTable({
			df <- pipeline$dati_processati()
			req(df)

			lot_id <- crea_lotto_id(df)
			dest_cod <- as.character(df$dest_stabilimento_cod)
			dest_com <- as.character(df$dest_com)
			dest_cod[is.na(dest_cod)] <- "N/D"
			dest_com[is.na(dest_com)] <- "N/D"
			dest_key <- paste(dest_cod, dest_com, sep = "|")

			gruppi <- split(seq_len(nrow(df)), dest_key)
			righe <- lapply(gruppi, function(idx) {
				totale <- length(idx)
				estero <- sum(df$orig_italia[idx] == FALSE, na.rm = TRUE)
				lotti <- length(unique(lot_id[idx]))
				percentuale <- if (totale > 0) round(100 * estero / totale, 1) else 0
				data.frame(
					`Codice destinazione` = dest_cod[idx][1],
					`Comune destinazione` = dest_com[idx][1],
					Lotti = lotti,
					Animali = totale,
					`Percentuale da estero` = paste0(percentuale, "%"),
					check.names = FALSE
				)
			})

			risultato <- do.call(rbind, righe)
			risultato[order(-risultato$Animali), , drop = FALSE]
		}, rownames = FALSE)

		# =====================================================================
		# OUTPUT - DATASET COMPLETO
		# =====================================================================

		output$download_dataset <- downloadHandler(
			filename = function() {
				grp <- gruppo()
				grp_lab <- if (is.null(grp)) "movimentazioni" else grp
				paste0("movimentazioni_", grp_lab, "_elab_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
			},
			content = function(file) {
				df <- pipeline$dati_processati()
				if (is.null(df) || nrow(df) == 0) {
					shiny::showNotification(
						"Nessun dato disponibile per il download.",
						type = "warning",
						duration = 6,
						session = session
					)
					stop("Nessun dato disponibile per il download.")
				}
				openxlsx::write.xlsx(df, file)
			}
		)

		# =====================================================================
		# RIEPILOGHI PAGINA INPUT
		# =====================================================================

		output$n_animali <- renderUI({
			df <- animali()
			grp <- gruppo()
			req(df, grp)

			data_inizio <- min(as.Date(df$ingresso_data, format = "%d/%m/%Y"), na.rm = TRUE)
			data_fine <- max(as.Date(df$ingresso_data, format = "%d/%m/%Y"), na.rm = TRUE)
			data_inizio_label <- ifelse(is.finite(data_inizio), format(data_inizio, "%d/%m/%Y"), "N/D")
			data_fine_label <- ifelse(is.finite(data_fine), format(data_fine, "%d/%m/%Y"), "N/D")

			# Breakdown nazionale/estero (disponibile dopo elaborazione pipeline)
			df_proc <- tryCatch(pipeline$dati_processati(), error = function(e) NULL)
			prov_ui <- if (!is.null(df_proc) && nrow(df_proc) > 0 &&
				       "orig_italia" %in% names(df_proc)) {
				lot_id  <- crea_lotto_id(df_proc)
				is_it   <- df_proc$orig_italia == TRUE
				is_est  <- df_proc$orig_italia == FALSE
				n_it    <- sum(is_it,  na.rm = TRUE)
				n_est   <- sum(is_est, na.rm = TRUE)
				lot_it  <- length(unique(lot_id[is_it  & !is.na(is_it)]))
				lot_est <- length(unique(lot_id[is_est & !is.na(is_est)]))
				tagList(
					"Provenienza nazionale: ", n_it,  " capi (", lot_it,  " lotti)", br(),
					"Provenienza estera: ",    n_est, " capi (", lot_est, " lotti)", br()
				)
			} else NULL

			div(
				h3(bs_icon("info-circle-fill"), " ", stringr::str_to_title(grp)),
				h4("Periodo"),
				"Data prima movimentazione: ", data_inizio_label, br(),
				"Data ultima movimentazione: ", data_fine_label, br(),
				h4("Capi movimentati"),
				"Totale: ", nrow(df), " capi", br(),
				prov_ui
			)
		})

		output$titolo_malattie <- renderUI({
			grp <- gruppo()
			req(grp)
			h4("Malattie considerate per il gruppo specie")
		})

		output$malattie_importate <- renderTable({
			grp <- gruppo()
			req(grp)

			tryCatch({
				malattie_data <- malattie()
				if (is.null(malattie_data)) return(NULL)

				df_malattie <- malattie_data[["metadati"]]
				if (is.null(df_malattie) || nrow(df_malattie) == 0) return(NULL)

				df_filtrato <- df_malattie[df_malattie$specie == grp,
					c("malattia", "riferimento", "data_inizio", "data_fine")]

				if (nrow(df_filtrato) > 0) {
					df_filtrato$data_inizio <- format(df_filtrato$data_inizio, "%d/%m/%Y")
					df_filtrato$data_fine <- format(df_filtrato$data_fine, "%d/%m/%Y")
				}

				df_filtrato
			}, error = function(e) {
				message("Errore in malattie_importate: ", e$message)
				NULL
			})
		})

		output$riepilogo_controlli <- renderUI({
			df <- animali()
			if (is.null(df)) {
				return(NULL)
			}

			df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
			df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
			prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
			nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

			manuale_nascita <- nrow(df_nasc)
			manuale_provenienza <- nrow(df_prov)
			nati_non_indenni <- conta_animali(nasc_non_indenni)
			provenienti_non_indenni <- conta_animali(prov_non_indenni)

			grp <- gruppo()
			df_meta <- malattie()[["metadati"]]
			sigle <- sub("^IND_", "", toupper(df_meta$campo[df_meta$specie == grp]))
			sigle_malattie <- paste0("(", paste(sigle, collapse = ", "), ")")

			riga_colore <- function(testo, valore) {
				colore <- ifelse(valore == 0, "green", "red")
				div(style = paste0("color: ", colore, ";"), paste(testo, valore))
			}
			riga_manuale <- function(testo, valore) {
				if (valore == 0) return(NULL)
				div(style = "color: orange;", paste(testo, valore))
			}

			div(
				h4("Riepilogo controlli"),
				riga_colore(paste("Animali nati in province non indenni", sigle_malattie, ":"), nati_non_indenni),
				riga_colore(paste("Animali provenienti da province non indenni", sigle_malattie, ":"), provenienti_non_indenni),
				riga_manuale("Animali da controllare manualmente per nascita:", manuale_nascita),
				riga_manuale("Animali da controllare manualmente per provenienza:", manuale_provenienza)
			)
		})

		# =====================================================================
		# OUTPUT CONTROLLO MANUALE
		# =====================================================================

		output$ui_provenienza_non_trovata <- renderUI({
			req(pipeline$df_provenienza_non_trovati())
			df <- pipeline$df_provenienza_non_trovati()

			if (nrow(df) == 0) {
				return(div(style = "color: green;",
					"Non ci sono animali di provenienza nazionale con codice stabilimento di origine non mappabile"))
			}

			tagList(
				DT::DTOutput(ns("tabella_provenienza_non_trovata")),
				downloadButton(ns("download_provenienza_non_trovata"), "Scarica Excel")
			)
		})

		output$tabella_provenienza_non_trovata <- DT::renderDT({
			req(pipeline$df_provenienza_non_trovati())
			df <- pipeline$df_provenienza_non_trovati()

			if (nrow(df) == 0) return(NULL)

			DT::datatable(
				df,
				options = list(pageLength = 10, scrollX = TRUE),
				rownames = FALSE
			)
		})

		output$download_provenienza_non_trovata <- downloadHandler(
			filename = function() {
				paste0("provenienza_non_trovata_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
			},
			content = function(file) {
				req(pipeline$df_provenienza_non_trovati())
				df <- pipeline$df_provenienza_non_trovati()
				openxlsx::write.xlsx(df, file)
			}
		)

		output$ui_nascita_non_trovata <- renderUI({
			req(pipeline$df_nascita_non_trovati())
			df <- pipeline$df_nascita_non_trovati()

			if (nrow(df) == 0) {
				return(div(style = "color: green;",
					"Non ci sono animali nati in Italia con provincia nel marchio auricolare non mappabile"))
			}

			tagList(
				DT::DTOutput(ns("tabella_nascita_non_trovata")),
				downloadButton(ns("download_nascita_non_trovata"), "Scarica Excel")
			)
		})

		output$tabella_nascita_non_trovata <- DT::renderDT({
			req(pipeline$df_nascita_non_trovati())
			df <- pipeline$df_nascita_non_trovati()

			if (nrow(df) == 0) return(NULL)

			DT::datatable(
				df,
				options = list(pageLength = 10, scrollX = TRUE),
				rownames = FALSE
			)
		})

		output$download_nascita_non_trovata <- downloadHandler(
			filename = function() {
				paste0("nascita_non_trovata_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
			},
			content = function(file) {
				req(pipeline$df_nascita_non_trovati())
				df <- pipeline$df_nascita_non_trovati()
				openxlsx::write.xlsx(df, file)
			}
		)

		# BDN export per controllo manuale
		output$ui_bdn_controllo_manuale <- renderUI({
			req(pipeline$df_provenienza_non_trovati)
			req(pipeline$df_nascita_non_trovati)

			df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
			df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())

			ha_animali <- (nrow(df_prov) > 0 || nrow(df_nasc) > 0)
			if (!ha_animali) {
				return(NULL)
			}

			codici_prov <- if (nrow(df_prov) > 0 && "capo_identificativo" %in% names(df_prov)) {
				as.character(df_prov$capo_identificativo)
			} else {
				character(0)
			}
			codici_nasc <- if (nrow(df_nasc) > 0 && "capo_identificativo" %in% names(df_nasc)) {
				as.character(df_nasc$capo_identificativo)
			} else {
				character(0)
			}

			codici_totali <- unique(c(codici_prov, codici_nasc))
			codici_totali <- codici_totali[!is.na(codici_totali) & codici_totali != ""]
			n_animali <- length(codici_totali)

			if (n_animali == 0) {
				return(NULL)
			}

			div(
				style = "margin: 20px 0; padding: 15px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;",
				h4("Esportazione per BDN - Interrogazione \"Capi da file\""),
				p("Scarica i codici identificativi di tutti gli animali con dati geografici non mappabili ",
				  "(sia codice stabilimento che provincia marchio auricolare)."),
				p(strong(paste0("Totale animali: ", n_animali))),
				tags$ul(
					tags$li("Codifica ANSI (Windows-1252) con interruzioni di linea Windows (CRLF)"),
					tags$li("Un codice per riga, massimo 255 per file"),
					tags$li("≤255 animali: download diretto file .txt"),
					tags$li(">255 animali: download file .zip con multipli .txt")
				),
				downloadButton(ns("download_bdn_controllo_manuale"), "Scarica per BDN",
					icon = icon("download"),
					class = "btn-warning")
			)
		})

		output$download_bdn_controllo_manuale <- downloadHandler(
			filename = function() {
				req(pipeline$df_provenienza_non_trovati)
				req(pipeline$df_nascita_non_trovati)

				df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
				df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())

				codici_prov <- if (nrow(df_prov) > 0 && "capo_identificativo" %in% names(df_prov)) {
					as.character(df_prov$capo_identificativo)
				} else {
					character(0)
				}
				codici_nasc <- if (nrow(df_nasc) > 0 && "capo_identificativo" %in% names(df_nasc)) {
					as.character(df_nasc$capo_identificativo)
				} else {
					character(0)
				}

				codici_totali <- unique(c(codici_prov, codici_nasc))
				codici_totali <- codici_totali[!is.na(codici_totali) & codici_totali != ""]
				n_animali <- length(codici_totali)

				if (n_animali <= 255) {
					paste0("bdn_controllo_manuale_", format(Sys.Date(), "%Y%m%d"), ".txt")
				} else {
					paste0("bdn_export_controllo_manuale_", format(Sys.Date(), "%Y%m%d"), ".zip")
				}
			},
			content = function(file) {
				req(pipeline$df_provenienza_non_trovati)
				req(pipeline$df_nascita_non_trovati)

				df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
				df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())

				tryCatch({
					lista_combined <- list()
					if (nrow(df_prov) > 0) {
						lista_combined[["Codice_stabilimento_non_mappabile"]] <- df_prov
					}
					if (nrow(df_nasc) > 0) {
						lista_combined[["Provincia_marchio_non_mappabile"]] <- df_nasc
					}

					if (length(lista_combined) == 0) {
						stop("Nessun animale da esportare")
					}

					n_animali <- conta_animali_da_esportare(lista_combined)

					if (n_animali <= 255) {
						txt_path <- crea_txt_bdn_export(lista_combined, tipo = "controllo_manuale")
						file.copy(txt_path, file, overwrite = TRUE)
						unlink(txt_path)
					} else {
						zip_path <- crea_zip_bdn_export(lista_combined, tipo = "controllo_manuale")
						file.copy(zip_path, file, overwrite = TRUE)
						unlink(zip_path)
					}
				}, error = function(e) {
					showNotification(
						paste("Errore nella creazione del file:", e$message),
						type = "error",
						duration = 10
					)
				})
			}
		)

		# =====================================================================
		# OUTPUT NON INDENNI
		# =====================================================================

		output$ui_diagnostica_non_indenni <- renderUI({
			prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
			nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

			n_prov <- conta_animali(prov_non_indenni)
			n_nasc <- conta_animali(nasc_non_indenni)

			if (n_prov == 0 && n_nasc == 0) {
				return(div(
					class = "alert alert-success",
					role = "alert",
					icon("check-circle"),
					strong(" Nessun animale a rischio: "),
					"Non sono presenti animali provenienti o nati in zone non indenni per le malattie considerate."
				))
			}

			tagList(
				if (n_prov > 0) {
					div(
						class = "alert alert-danger",
						role = "alert",
						icon("exclamation-triangle"),
						strong(paste(" Animali provenienti da zone non indenni:", n_prov)),
						" - Controllare la sezione 'Provenienze' per i dettagli."
					)
				},
				if (n_nasc > 0) {
					div(
						class = "alert alert-danger",
						role = "alert",
						icon("exclamation-triangle"),
						strong(paste(" Animali nati in zone non indenni:", n_nasc)),
						" - Controllare la sezione 'Nascite' per i dettagli."
					)
				}
			)
		})

		output$tabella_riepilogo_non_indenni <- renderTable({
			req(pipeline$dati_processati())
			req(malattie())
			req(gruppo())

			grp <- gruppo()
			malattie_data <- malattie()

			prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
			nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

			if (length(prov_non_indenni) == 0 && length(nasc_non_indenni) == 0) {
				return(data.frame())
			}

			df_meta <- malattie_data[["metadati"]]
			malattie_gruppo <- df_meta[df_meta$specie == grp, ]

			animali_rischio <- data.frame()

			for (nome_malattia in names(prov_non_indenni)) {
				df_mal <- prov_non_indenni[[nome_malattia]]
				if (nrow(df_mal) > 0) {
					campo_raw <- malattie_gruppo$campo[malattie_gruppo$malattia == nome_malattia]
					campo_clean <- sub("^Ind_", "", campo_raw)
					col_name <- paste0("prov_", campo_clean)

					temp <- df_mal[, c("capo_identificativo", "dest_stabilimento_cod"), drop = FALSE]
					temp$tipo <- "provenienza"
					temp$malattia <- nome_malattia
					temp$campo <- col_name
					animali_rischio <- rbind(animali_rischio, temp)
				}
			}

			for (nome_malattia in names(nasc_non_indenni)) {
				df_mal <- nasc_non_indenni[[nome_malattia]]
				if (nrow(df_mal) > 0) {
					campo_raw <- malattie_gruppo$campo[malattie_gruppo$malattia == nome_malattia]
					campo_clean <- sub("^Ind_", "", campo_raw)
					col_name <- paste0("nascita_", campo_clean)

					temp <- df_mal[, c("capo_identificativo", "dest_stabilimento_cod"), drop = FALSE]
					temp$tipo <- "nascita"
					temp$malattia <- nome_malattia
					temp$campo <- col_name
					animali_rischio <- rbind(animali_rischio, temp)
				}
			}

			if (nrow(animali_rischio) == 0) {
				return(data.frame())
			}

			campi_unici <- unique(animali_rischio$campo)
			stab_list <- split(animali_rischio, animali_rischio$dest_stabilimento_cod)

			result <- lapply(names(stab_list), function(stab_cod) {
				stab_data <- stab_list[[stab_cod]]
				row <- data.frame(dest_stab_cod = stab_cod, stringsAsFactors = FALSE)
				for (campo in campi_unici) {
					count <- sum(stab_data$campo == campo)
					row[[campo]] <- count
				}
				row$totale <- nrow(stab_data)
				row
			})

			result_df <- do.call(rbind, result)
			result_df <- result_df[order(-result_df$totale), , drop = FALSE]
			result_df
		}, rownames = FALSE, digits = 0)

		output$ui_bdn_non_indenni_all <- renderUI({
			prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
			nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

			if (length(prov_non_indenni) == 0 && length(nasc_non_indenni) == 0) {
				return(NULL)
			}

			codici <- character(0)
			for (df_item in c(prov_non_indenni, nasc_non_indenni)) {
				if (!is.null(df_item) && "capo_identificativo" %in% names(df_item)) {
					codici <- c(codici, as.character(df_item$capo_identificativo))
				}
			}
			codici <- unique(codici[!is.na(codici) & codici != ""])
			n_animali <- length(codici)

			if (n_animali == 0) return(NULL)

			div(
				style = "margin: 20px 0; padding: 15px; background-color: #f0f8ff; border: 1px solid #4682b4; border-radius: 5px;",
				h4("Esportazione per BDN - Interrogazione \"Capi da file\""),
				p("Scarica i codici identificativi di ", strong("tutti"), " gli animali a rischio (provenienti o nati in zone non indenni per qualsiasi malattia), ",
				  "formattati per il caricamento nell'interrogazione \"Capi da file\" BDN."),
				p(strong(paste0("Totale animali: ", n_animali))),
				tags$ul(
					tags$li("Codifica ANSI (Windows-1252) con interruzioni di linea Windows (CRLF)"),
					tags$li("Un codice per riga, massimo 255 per file"),
					tags$li("≤255 animali: download diretto file .txt"),
					tags$li(">255 animali: download file .zip con multipli .txt")
				),
				downloadButton(ns("download_bdn_non_indenni_all"), "Scarica per BDN (tutti)",
					icon = icon("download"),
					class = "btn-primary")
			)
		})

		output$download_bdn_non_indenni_all <- downloadHandler(
			filename = function() {
				prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
				nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

				if (length(prov_non_indenni) > 0) {
					names(prov_non_indenni) <- paste0("prov_", names(prov_non_indenni))
				}
				if (length(nasc_non_indenni) > 0) {
					names(nasc_non_indenni) <- paste0("nascita_", names(nasc_non_indenni))
				}
				combined_list <- c(prov_non_indenni, nasc_non_indenni)
				n_animali <- conta_animali_da_esportare(combined_list)

				if (n_animali <= 255) {
					paste0("bdn_non_indenni_", format(Sys.Date(), "%Y%m%d"), ".txt")
				} else {
					paste0("bdn_export_non_indenni_", format(Sys.Date(), "%Y%m%d"), ".zip")
				}
			},
			content = function(file) {
				prov_non_indenni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
				nasc_non_indenni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

				tryCatch({
					if (length(prov_non_indenni) > 0) {
						names(prov_non_indenni) <- paste0("prov_", names(prov_non_indenni))
					}
					if (length(nasc_non_indenni) > 0) {
						names(nasc_non_indenni) <- paste0("nascita_", names(nasc_non_indenni))
					}
					combined_list <- c(prov_non_indenni, nasc_non_indenni)
					n_animali <- conta_animali_da_esportare(combined_list)

					if (n_animali <= 255) {
						txt_path <- crea_txt_bdn_export(combined_list, tipo = "non_indenni")
						file.copy(txt_path, file, overwrite = TRUE)
						unlink(txt_path)
					} else {
						zip_path <- crea_zip_bdn_export(combined_list, tipo = "non_indenni")
						file.copy(zip_path, file, overwrite = TRUE)
						unlink(zip_path)
					}
				}, error = function(e) {
					showNotification(
						paste("Errore nella creazione del file:", e$message),
						type = "error",
						duration = 10
					)
				})
			}
		)

		# Sezione Provenienze - download buttons
		output$ui_provenienze_download <- renderUI({
			req(pipeline$animali_provenienza_non_indenni())
			liste_malattie <- pipeline$animali_provenienza_non_indenni()

			if (length(liste_malattie) == 0) {
				return(div(
					class = "alert alert-success",
					icon("check-circle"),
					" Nessuna movimentazione proveniente da zone non indenni per le malattie considerate."
				))
			}

			ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
				download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
				n_animali <- nrow(liste_malattie[[nome_malattia]])

				div(
					style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
					div(
						style = "display: flex; justify-content: space-between; align-items: center;",
						span(strong(nome_malattia), " - ", n_animali, " animali"),
						downloadButton(ns(download_id), "Scarica Excel", class = "btn-sm btn-outline-primary")
					)
				)
			})

			do.call(tagList, ui_elements)
		})

		observe({
			req(pipeline$animali_provenienza_non_indenni())
			liste_malattie <- pipeline$animali_provenienza_non_indenni()

			lapply(names(liste_malattie), function(nome_malattia) {
				download_id <- paste0("download_prov_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))

				output[[download_id]] <- downloadHandler(
					filename = function() {
						paste0("provenienza_", gsub("[^a-zA-Z0-9]", "_", nome_malattia),
							"_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
					},
					content = function(file) {
						df <- liste_malattie[[nome_malattia]]
						openxlsx::write.xlsx(df, file)
					}
				)
			})
		})

		# Sezione Nascite - download buttons
		output$ui_nascite_download <- renderUI({
			req(pipeline$animali_nascita_non_indenni())
			liste_malattie <- pipeline$animali_nascita_non_indenni()

			if (length(liste_malattie) == 0) {
				return(div(
					class = "alert alert-success",
					icon("check-circle"),
					" Nessuna movimentazione di animali nati in zone non indenni per le malattie considerate."
				))
			}

			ui_elements <- lapply(names(liste_malattie), function(nome_malattia) {
				download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))
				n_animali <- nrow(liste_malattie[[nome_malattia]])

				div(
					style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
					div(
						style = "display: flex; justify-content: space-between; align-items: center;",
						span(strong(nome_malattia), " - ", n_animali, " animali"),
						downloadButton(ns(download_id), "Scarica Excel", class = "btn-sm btn-outline-primary")
					)
				)
			})

			do.call(tagList, ui_elements)
		})

		observe({
			req(pipeline$animali_nascita_non_indenni())
			liste_malattie <- pipeline$animali_nascita_non_indenni()

			lapply(names(liste_malattie), function(nome_malattia) {
				download_id <- paste0("download_nasc_", gsub("[^a-zA-Z0-9]", "_", nome_malattia))

				output[[download_id]] <- downloadHandler(
					filename = function() {
						paste0("nascita_", gsub("[^a-zA-Z0-9]", "_", nome_malattia),
							"_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
					},
					content = function(file) {
						df <- liste_malattie[[nome_malattia]]
						openxlsx::write.xlsx(df, file)
					}
				)
			})
		})

		# =====================================================================
		# REACTIVE ESPOSTI (per pagina Input e bottone email)
		# =====================================================================

		# TRUE se questo gruppo ha dati caricati
		loaded <- reactive({
			df <- animali()
			!is.null(df) && nrow(df) > 0
		})

		# Dataframe dei capi problematici (non indenni + controllo manuale)
		capi_problematici_df <- reactive({
			grp <- gruppo()
			if (is.null(grp)) return(NULL)

			parts <- list()
			add_part <- function(d, categoria, malattia = NA_character_) {
				if (!is.null(d) && nrow(d) > 0) {
					d$categoria <- categoria
					d$malattia <- malattia
					parts[[length(parts) + 1]] <<- d
				}
			}

			df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
			add_part(df_prov, "controllo manuale provenienza")
			df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
			add_part(df_nasc, "controllo manuale nascita")
			prov_ni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
			for (nm in names(prov_ni)) add_part(prov_ni[[nm]], "provenienza non indenne", nm)
			nasc_ni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())
			for (nm in names(nasc_ni)) add_part(nasc_ni[[nm]], "nascita non indenne", nm)

			if (length(parts) == 0) return(NULL)
			res <- dplyr::bind_rows(parts)
			# Porta categoria e malattia in testa per leggibilità
			front <- intersect(c("capo_identificativo", "categoria", "malattia"), names(res))
			res[, c(front, setdiff(names(res), front)), drop = FALSE]
		})

		# Numero di capi unici problematici
		n_problematici <- reactive({
			d <- capi_problematici_df()
			if (is.null(d) || nrow(d) == 0) return(0)
			ids <- as.character(d$capo_identificativo)
			ids <- ids[!is.na(ids) & ids != ""]
			length(unique(ids))
		})

		# Testo riepilogo (per il corpo dell'email): vettore di righe, o character(0)
		email_text <- reactive({
			df <- animali()
			grp <- gruppo()
			if (is.null(df) || nrow(df) == 0 || is.null(grp)) return(character(0))

			data_inizio <- min(as.Date(df$ingresso_data, format = "%d/%m/%Y"), na.rm = TRUE)
			data_fine <- max(as.Date(df$ingresso_data, format = "%d/%m/%Y"), na.rm = TRUE)
			di <- if (is.finite(data_inizio)) format(data_inizio, "%d/%m/%Y") else "N/D"
			dfin <- if (is.finite(data_fine)) format(data_fine, "%d/%m/%Y") else "N/D"

			lines <- c(
				paste0("=== ", toupper(grp), " ==="),
				paste0("Periodo movimentazioni: ", di, " - ", dfin),
				paste0("Totale capi movimentati: ", nrow(df))
			)

			df_proc <- tryCatch(pipeline$dati_processati(), error = function(e) NULL)
			if (!is.null(df_proc) && nrow(df_proc) > 0 && "orig_italia" %in% names(df_proc)) {
				n_it <- sum(df_proc$orig_italia == TRUE, na.rm = TRUE)
				n_est <- sum(df_proc$orig_italia == FALSE, na.rm = TRUE)
				lines <- c(lines, paste0("  di cui provenienza nazionale: ", n_it, "; provenienza estera: ", n_est))
			}

			df_prov <- tryCatch(pipeline$df_provenienza_non_trovati(), error = function(e) data.frame())
			df_nasc <- tryCatch(pipeline$df_nascita_non_trovati(), error = function(e) data.frame())
			prov_ni <- tryCatch(pipeline$animali_provenienza_non_indenni(), error = function(e) list())
			nasc_ni <- tryCatch(pipeline$animali_nascita_non_indenni(), error = function(e) list())

			df_meta <- tryCatch(malattie()[["metadati"]], error = function(e) NULL)
			sigle_str <- ""
			if (!is.null(df_meta)) {
				sigle <- sub("^IND_", "", toupper(df_meta$campo[df_meta$specie == grp]))
				if (length(sigle) > 0) sigle_str <- paste0(" (", paste(sigle, collapse = ", "), ")")
			}

			nati_ni <- conta_animali(nasc_ni)
			prov_ni_n <- conta_animali(prov_ni)
			manuale_nascita <- nrow(df_nasc)
			manuale_provenienza <- nrow(df_prov)

			lines <- c(lines,
				"Riepilogo controlli:",
				paste0("  - Animali nati in province non indenni", sigle_str, ": ", nati_ni),
				paste0("  - Animali provenienti da province non indenni", sigle_str, ": ", prov_ni_n))
			if (manuale_nascita > 0) {
				lines <- c(lines, paste0("  - Da controllare manualmente per nascita: ", manuale_nascita))
			}
			if (manuale_provenienza > 0) {
				lines <- c(lines, paste0("  - Da controllare manualmente per provenienza: ", manuale_provenienza))
			}
			if (nati_ni == 0 && prov_ni_n == 0 && manuale_nascita == 0 && manuale_provenienza == 0) {
				lines <- c(lines, "  Esito: nessun capo problematico, tutti provenienti/nati in zone indenni.")
			}

			lines
		})

		# Valori esposti al server principale
		list(
			loaded = loaded,
			email_text = email_text,
			capi_problematici_df = capi_problematici_df,
			n_problematici = n_problematici
		)
	})
}
