# per testare le funzioni con file di esempio, fornisce ambiente con bovini (animali() e gruppo())
# source("tests/test.R")

# modulo per importare i dati geografici delle malattie
#2do: importare altre malattie dagli utenti

mod_import_malattie <- function(id, gruppo) {
	# definizione del server del modulo
	moduleServer(id, function(input, output, session) {
		# modulo server vero e proprio
		
		reactive({
			# valore reattivo restituito
			# req(animali())                                 # richiede che i dati siano presenti
			# req(gruppo())                                  # richiede che il gruppo sia definito
			

			
			# dati <- animali()
			
			# importazione dati statici -----------------------
			## tabelle geografiche ------
			# "df_comuni.csv" "df_prefissi_stab.csv" "df_province.csv" "df_regioni.csv" "df_stati_iso3166.csv" (UTF8)
			# prefissi codice di stabilimento
			df_stab          <- read.csv(
				"data_static/geo/df_prefissi_stab.csv",
				stringsAsFactors = FALSE,
				colClasses = "character"
			)
			# tabella stati esteri
			df_stati        <- read.csv(
				"data_static/geo/df_stati_iso3166.csv",
				stringsAsFactors = FALSE,
				colClasses = "character",
				fileEncoding = "UTF-8"
			)
			# tabella regioni
			df_regioni      <- read.csv(
				"data_static/geo/df_regioni.csv",
				stringsAsFactors = FALSE,
				colClasses = "character"
			)
			# tabella province
			df_province     <- read.csv(
				"data_static/geo/df_province.csv",
				stringsAsFactors = FALSE,
				colClasses = "character"
			)
			# tabella comuni
			df_comuni       <- read.csv(
				"data_static/geo/df_comuni.csv",
				stringsAsFactors = FALSE,
				colClasses = "character",
				fileEncoding = "UTF-8"
			)
			
			
			# df_codici_stabilimento <- read.csv("data_static/", stringsAsFactors = FALSE, colClasses = "character") # tabella comuni e province
			# df_province <- read.csv("data_static/chiave_province.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella province
			# df_regioni <- read.csv("data_static/chiave_regioni.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella regioni
			# df_decodifica <- read.csv("data_static/chiave_decodifica.csv", stringsAsFactors = FALSE) # tabella decodifica
			#
			## tabelle malattie --------
			# carico tutte le tabelle (files.xlsx) presenti in data_static/malattie e distinguo tra gli elenchi di comuni e le malattie
			files_malattie <- list.files("data_static/malattie",
																	 pattern = "\\.xlsx$",
																	 full.names = TRUE)
			

			# tipi di campi dei files metadati
			meta_col_types  <- c("text", "text", "text", "text", "date", "date")
			df_meta_malattie <- structure(
				list(
					campo = character(0),
					malattia = character(0),
					specie = character(0),
					riferimento = character(0),
					data_inizio = structure(numeric(0), class = "Date"),
					data_fine = structure(numeric(0), class = "Date"),
					file = character(0)
				),
				row.names = integer(0),
				class = "data.frame"
			)
			###### dataframe da popolare con i dati delle malattie nel ciclo for #####
			# df_province_malattie <- df_province[, c("COD_UTS", "COD_REG")]
			# df_comuni_malattie <- df_comuni[, c("PRO_COM_T", "COD_UTS", "COD_REG")]
			
			df_province_malattie_template <- df_province[, c("COD_UTS", "COD_REG")]
			df_comuni_malattie_template <- df_comuni[, c("PRO_COM_T", "COD_UTS", "COD_REG")]
			
			# importazione metadati da tutti i files ------------------------
			for (i in 1:length(files_malattie)) {
				file <- files_malattie[i]
				metadati <- readxl::read_excel(file, sheet = "metadati", col_types = meta_col_types)
				metadati$file <- file
				df_meta_malattie <- rbind(df_meta_malattie, metadati)
			}
			rm(metadati, file, i)
			# creo una lista che contiene: il dataframe delle malattie, una lista per gruppo di specie che contiene un dataframe per provincia e uno per comune
			malattie <- list()
			
			# seleziono solo le malattie rilevanti alla data odierna (così non processo files "scaduti")
			data_odierna <- Sys.Date()
			df_meta_malattie <- df_meta_malattie %>%
				filter((is.na(data_inizio) | data_inizio <= data_odierna) &
							 (is.na(data_fine) | data_fine >= data_odierna)
							 )
			# se ci sono duplicati nella malattia per lo stesso gruppo di specie -> errore
			if (any(duplicated(df_meta_malattie[, c("malattia", "specie")]))) {
				duplicati <- df_meta_malattie[duplicated(df_meta_malattie[, c("malattia", "specie")]) |
																				duplicated(df_meta_malattie[, c("malattia", "specie")], fromLast = TRUE), ]
				stop(
					"Attenzione: sono stati riscontrati duplicati nei metadati delle malattie per la malattia '",
					duplicati$malattia[1],
					"' e il gruppo '",
					duplicati$specie[1],
					"'. Controllare il file ",
					basename(duplicati$file[1])
				)
			}
			# salvo i metadati nella lista malattie
			
			malattie[["metadati"]] <- df_meta_malattie
			
			# inizializzo i dataframe delle malattie per ogni specie
			
			for(i in unique(df_meta_malattie$specie)){
				malattie[[i]] <- list()
				malattie[[i]][["province"]] <- df_province_malattie_template
				malattie[[i]][["comuni"]] <- df_comuni_malattie_template
			}

			
			#############################################################
			# caricamento dei dati delle malattie <- indenne si => TRUE #
			#############################################################
			
			for (i in 1:nrow(df_meta_malattie))
					 {
				file <- df_meta_malattie$file[i]
				fogli <- tolower(trimws(openxlsx::getSheetNames(file)))  # fogli effettivi del file
				# #### il foglio metadati c'è sempre e mi serve dopo ####
				# metadati <- readxl::read_excel(file, sheet = "metadati", col_types = meta_col_types)
				# 
				if (setequal(fogli, tipi_files_malattie_fogli[["province_indenni"]])) {

					########################################
					# file con province + metadati ------

					provinceind <- openxlsx::read.xlsx(file, sheet = "province")
					# (aggiunge con join al dataframe delle province della malattia il campo indenne)
					message("File ",
									basename(file),
									" riconosciuto come 'province_indenni'")
					provinceind <- provinceind[, c("COD_UTS", df_meta_malattie$campo[i])]
						
					# inserisco nel dataframe delle malattie nella lista corretta
					malattie[[df_meta_malattie$specie[i]]][["province"]] <-
						merge(
							malattie[[df_meta_malattie$specie[i]]][["province"]],
							provinceind,
							by = "COD_UTS",
							all.x = TRUE,
							all.y = FALSE
						)
					# inserisco anche nel dataframe dei comuni (per calcolo successivo)
					malattie[[df_meta_malattie$specie[i]]][["comuni"]] <-
						merge(
							malattie[[df_meta_malattie$specie[i]]][["comuni"]],
							provinceind,
							by = "COD_UTS",
							all.x = TRUE,
							all.y = FALSE
						)
					
				} else if (setequal(fogli, tipi_files_malattie_fogli[["blocchi"]])) {
					##################################################################
					
					# file con blocchi (regioni, province, comuni, metad) ----


						# porto a booleano il campo blocco
						
						###### ATTENZIONE AI VALORI BOOLEANI T/F (indenne vs bloccato) ######
						
						# per adesso lavoro a logica invertita
						regioni  <- openxlsx::read.xlsx(file, sheet = "regioni")
						regioni <- regioni %>%
							mutate(blocco = ifelse(
								is.na(blocco),
								FALSE,
								tolower(trimws(as.character(blocco))) %in% c("s", "si", "1", "t", "true")
							))
						
						province <- openxlsx::read.xlsx(file, sheet = "province")
						province <- province %>%
							mutate(blocco = ifelse(
								is.na(blocco),
								FALSE,
								tolower(trimws(as.character(blocco))) %in% c("s", "si", "1", "t", "true")
							))
						
						comuni   <- openxlsx::read.xlsx(file, sheet = "comuni")
						comuni <- comuni %>%
							mutate(blocco = ifelse(
								is.na(blocco),
								FALSE,
								tolower(trimws(as.character(blocco))) %in% c("s", "si", "1", "t", "true")
							))
						
						# riporto tutto a livello di df_comuni_malattie e df_province_malattie:
						# comuni:
						# se regione = FALSE -> tutti i comuni di quella regione sono bloccati
						# se provincia = FALSE -> tutti i comuni di quella provincia sono bloccati
						# se comune = FALSE -> quel comune è bloccato
						
						df_comuni_malattie_temp <- df_comuni_malattie_template
						df_comuni_malattie_temp <- merge(
							df_comuni_malattie_temp,
							comuni[, c("PRO_COM_T", "blocco")],
							by = "PRO_COM_T",
							all.x = TRUE,
							all.y = FALSE
						)
						df_comuni_malattie_temp <- merge(
							df_comuni_malattie_temp,
							province[, c("COD_UTS", "blocco")],
							by = "COD_UTS",
							all.x = TRUE,
							all.y = FALSE,
							suffixes = c("_comune", "_provincia")
						)
						df_comuni_malattie_temp <- merge(
							df_comuni_malattie_temp,
							regioni[, c("COD_REG", "blocco")],
							by = "COD_REG",
							all.x = TRUE,
							all.y = FALSE
						)
						# rinomina "blocco" in "blocco_regione"
						colnames(df_comuni_malattie_temp)[which(colnames(df_comuni_malattie_temp) == "blocco")] <- "blocco_regione"
						
						##################################################################################################
						# qui nego il valore booleano: se regione/provincia/comune è TRUE -> è indenne -> blocco = FALSE #
						###################################################################################################
						
						campo_malattia <- df_meta_malattie$campo[i]
						
						df_comuni_malattie_temp[, campo_malattia] <- 	with(
							df_comuni_malattie_temp,!(blocco_regione |
																					blocco_provincia |
																					blocco_comune)
						)
						
						# controllo valori NA
						
						if (any(is.na(df_comuni_malattie_temp[, campo_malattia]))) {
							stop(
								"Attenzione: valori NA riscontrati nel file ",
								basename(file),
								" per la malattia ",
								df_meta_malattie$malattia[i],
								" e il gruppo ",
								df_meta_malattie$specie[i]
							)
						}
						
						malattie[[df_meta_malattie$specie[i]]][["comuni"]] <-
							merge(
								malattie[[df_meta_malattie$specie[i]]][["comuni"]],
							df_comuni_malattie_temp[, c("PRO_COM_T", campo_malattia)],
							by = "PRO_COM_T",
							all.x = TRUE,
							all.y = FALSE
						)
						
						
						
						# province:
						# se regione = FALSE -> tutte le province di quella regione sono bloccate
						# se provincia = FALSE -> quella provincia è bloccata
						# se anche solo un comune della provincia = FALSE -> quella provincia è bloccata
						
						# ricavo il blocco delle province dai comuni (df_comuni_malattie_temp[, c("COD_UTS", metadati$campo)])
						
						
						df_province_malattie_temp <- df_comuni_malattie_temp %>%
							group_by(COD_UTS) %>%
							summarise(
								!!campo_malattia := all(.data[[campo_malattia]] == TRUE, na.rm = FALSE),
								# AND su tutti i comuni della provincia
								.groups = "drop"
							)
						# ora unisco con il blocco delle province
						malattie[[df_meta_malattie$specie[i]]][["province"]] <- 
							merge(
								malattie[[df_meta_malattie$specie[i]]][["province"]],
							df_province_malattie_temp[, c("COD_UTS", campo_malattia)],
							by = "COD_UTS",
							all.x = TRUE,
							all.y = FALSE
						)
						if (any(is.na(malattie[[df_meta_malattie$specie[i]]][["province"]][, campo_malattia]))) {
							stop(
								"Attenzione: valori NA riscontrati nel file ",
								basename(file),
								" per la malattia ",
								df_meta_malattie$malattia[i],
								" e il gruppo ",
								df_meta_malattie$specie[i]
							)
							#######################################################################################
							# la lista malattie è l'output finale con tutte le malattie a livello di provincia #
							#######################################################################################
							
						
						}
						
						
						# (accoda ai dataframe degli animali)
						
						
					
					}
					
				}
				
		}
			# # restituisco la lista malattie
			# malattie
		)
	})
}