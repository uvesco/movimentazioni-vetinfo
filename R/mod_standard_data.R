# modulo per standardizzare le colonne e collegare tutti i dati geografici e delle malattie

mod_standardize_server <- function(id, animali, gruppo) {               # definizione del server del modulo
	moduleServer(id, function(input, output, session) {            # modulo server vero e proprio
		
		reactive({                                             # valore reattivo restituito
			req(animali())                                 # richiede che i dati siano presenti
			req(gruppo())                                  # richiede che il gruppo sia definito
			
		# per testare le funzioni con file di esempio, fornisce ambiente con bovini (animali() e gruppo())
		# source("tests/test.R")	
			
		dati <- animali()

			# importazione dati statici -----------------------
			## tabelle geografiche ------
			# "df_comuni.csv" "df_prefissi_stab.csv" "df_province.csv" "df_regioni.csv" "df_stati_iso3166.csv" (UTF8)
			# prefissi codice di stabilimento
			df_stab          <- read.csv("data_static/geo/df_prefissi_stab.csv",
																	 stringsAsFactors = FALSE, 
																	 colClasses = "character")
			# tabella stati esteri
			df_stati        <- read.csv("data_static/geo/df_stati_iso3166.csv", 
																	 stringsAsFactors = FALSE, 
																	 colClasses = "character",
																	 fileEncoding = "UTF-8")
			# tabella regioni
			df_regioni      <- read.csv("data_static/geo/df_regioni.csv", 
																	 stringsAsFactors = FALSE, 
																	 colClasses = "character")
			# tabella province
			df_province     <- read.csv("data_static/geo/df_province.csv",
																	 stringsAsFactors = FALSE, 
																	 colClasses = "character")
			# tabella comuni
			df_comuni       <- read.csv("data_static/geo/df_comuni.csv",
																	 stringsAsFactors = FALSE, 
																	 colClasses = "character",
																	 fileEncoding = "UTF-8")
			
			
			# df_codici_stabilimento <- read.csv("data_static/", stringsAsFactors = FALSE, colClasses = "character") # tabella comuni e province
			# df_province <- read.csv("data_static/chiave_province.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella province
			# df_regioni <- read.csv("data_static/chiave_regioni.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella regioni
			# df_decodifica <- read.csv("data_static/chiave_decodifica.csv", stringsAsFactors = FALSE) # tabella decodifica
			# 
			## tabelle malattie --------
			# carico tutte le tabelle (files.xlsx) presenti in data_static/malattie e distinguo tra gli elenchi di comuni e le malattie
	    files_malattie <- list.files("data_static/malattie", pattern = "\\.xlsx$", full.names = TRUE)
			
	    tipi_files_malattie_fogli <- list(province_indenni = c("province", "metadati"),
	    																	blocchi = c("regioni", "province", "comuni", "metadati"))
	    # tipi di campi dei files metadati
	    meta_col_types  <- c("text", "text", "text", "text", "date", "date")
	    df_meta_malattie <- structure(list(campo = character(0), malattia = character(0), 
	    																	 specie = character(0), riferimento = character(0), data_inizio = structure(numeric(0), class = "Date"), 
	    																	 data_fine = structure(numeric(0), class = "Date")), row.names = integer(0), class = "data.frame")
	    ###### dataframe da popolare con i dati delle malattie nel ciclo for #####
	    df_prov_malattie <- df_province[, c("COD_UTS", "COD_REG")]
	    df_comuni_malattie <- df_comuni[, c("PRO_COM_T", "COD_UTS", "COD_REG")]
	    
	    df_prov_malattie_template <- df_province[, c("COD_UTS", "COD_REG")]
	    df_comuni_malattie_template <- df_comuni[, c("PRO_COM_T", "COD_UTS", "COD_REG")]
	    
	    
	    #############################################################
	    # caricamento dei dati delle malattie <- indenne si => TRUE #
	    #############################################################
	    
			for(i in 1:length(files_malattie)){
				file <- files_malattie[i]
				fogli <- tolower(trimws(openxlsx::getSheetNames(file)))  # fogli effettivi del file
				#### il foglio metadati c'è sempre e mi serve dopo ####
				metadati <- readxl::read_excel(file, sheet = "metadati", col_types = meta_col_types)
				
				if (setequal(fogli, tipi_files_malattie_fogli[["province_indenni"]])) {
					# === file con province + metadati ===
					provinceind <- openxlsx::read.xlsx(file, sheet = "province")
					# (aggiunge con join al dataframe df_province le colonne indicate in metadati$campo
					message("File ", basename(file), " riconosciuto come 'province_indenni'")
					provinceind <- provinceind %>%
						mutate(across(starts_with("ind_"),
													~ ifelse(is.na(.x), NA, tolower(trimws(.x)) %in% c("s", "si", "1", "t", "true"))))

					df_prov_malattie <-
						merge(df_prov_malattie, provinceind[,c( "COD_UTS", metadati$campo[metadati$specie == gruppo()])], 
								by = "COD_UTS",
								all.x = TRUE, 
								all.y = FALSE)
					
					df_comuni_malattie <-
						merge(df_comuni_malattie, provinceind[,c( "COD_UTS", metadati$campo[metadati$specie == gruppo()])], 
								by = "COD_UTS",
								all.x = TRUE, 
								all.y = FALSE)

				} else if (setequal(fogli, tipi_files_malattie_fogli[["blocchi"]])) {
					# === file con blocchi (regioni, province, comuni, metadati) ===
					if(metadati$campo[metadati$specie == gruppo()]){ # c'è un solo gruppo specie per file
					# porto a booleano il campo blocco
						
						###### ATTENZIONE AI CAMBI DI SEGNO (indenne vs bloccato) ######
						
					regioni  <- openxlsx::read.xlsx(file, sheet = "regioni")
					regioni <- regioni %>%
						mutate(across(starts_with("blocco"),
													~ ifelse(is.na(.x), TRUE, !(tolower(trimws(.x)) %in% c("s", "si", "1", "t", "true")))))
					
					province <- openxlsx::read.xlsx(file, sheet = "province")
					province <- province %>%
						mutate(across(starts_with("blocco"),
													~ ifelse(is.na(.x), TRUE, !(tolower(trimws(.x)) %in% c("s", "si", "1", "t", "true")))))
					
					comuni   <- openxlsx::read.xlsx(file, sheet = "comuni")
					comuni <- comuni %>%
						mutate(across(starts_with("blocco"),
													~ ifelse(is.na(.x), TRUE, !(tolower(trimws(.x)) %in% c("s", "si", "1", "t", "true")))))
				
					# riporto tutto a livello di df_comuni_malattie e df_prov_malattie:
					# comuni:
					# se regione = FALSE -> tutti i comuni di quella regione sono bloccati
					# se provincia = FALSE -> tutti i comuni di quella provincia sono bloccati
					# se comune = FALSE -> quel comune è bloccato
					
					df_comuni_malattie_temp <- df_comuni_malattie_template
					df_comuni_malattie_temp <- merge(df_comuni_malattie_temp, 
																			comuni[, c("PRO_COM_T", "blocco")],
																			by = "PRO_COM_T",
																			all.x = TRUE,
																			all.y = FALSE)
					df_comuni_malattie_temp <- merge(df_comuni_malattie_temp,
																			province[, c("COD_UTS", "blocco")],
																			by = "COD_UTS",
																			all.x = TRUE,
																			all.y = FALSE,
																			suffixes = c("_comune", "_provincia"))
					df_comuni_malattie_temp <- merge(df_comuni_malattie_temp,
																			regioni[, c("COD_REG", "blocco")],
																			by = "COD_REG",
																			by.y = "COD_REG",
																			all.x = TRUE,
																			all.y = FALSE,
																			suffixes = c("", "_regione")
																			)
					
					df_comuni_malattie_temp[, metadati$campo] <- 	with(df_comuni_malattie_temp,
																														 (blocco == FALSE) |
																														 	(blocco_provincia == FALSE) |
																														 	(blocco_comune == FALSE)
																														 )
					df_comuni_malattie <- merge(df_comuni_malattie,
																			df_comuni_malattie_temp[, c("PRO_COM_T", metadati$campo)],
																			by = "PRO_COM_T",
																			all.x = TRUE,
																			all.y = FALSE)

					
					
					# province:
					# se regione = FALSE -> tutte le province di quella regione sono bloccate
					# se provincia = FALSE -> quella provincia è bloccata
					# se anche solo un comune della provincia = FALSE -> quella provincia è bloccata
					
					# ricavo il blocco delle province dai comuni (df_comuni_malattie_temp[, c("COD_UTS", metadati$campo)])
					df_prov_malattie_temp  <- df_comuni_malattie_temp[, c("COD_UTS", metadati$campo)] %>%
						group_by(COD_UTS) %>%
						summarise(blocco_comuni = all(!!sym(metadati$campo))) # se tutti i comuni sono TRUE -> TRUE
					
					#BUG spariscono due province (roma e venezia) che ottengono un valore NA -> controllare nel file originario/template se ci sono quelle le province

					
					}
					

					# (accoda ai dataframe corrispondenti)

					
				} else {
					# === file non riconosciuto ===
					stop("struttura fogli non riconosciuta in ", basename(file))
					print(fogli)
				}
				
				
				
				
				
				
				
				
				
				
				fogli <- openxlsx::getSheetNames(files_malattie[i])
				assign(paste0("malattia_", tools::file_path_sans_ext(basename(files_malattie[i]))), 
								readxl::read_excel(files_malattie[i], col_types = "text"))
			}
			
			
			# attenzione ai comuni non validi (flag nella tabella) quando collego
			
			
	    
	    # per la nascita lavorare solo su province attuali
	    
	    
	    
	    
	    
# elaborazione dati -----------------------
## standardizzazione colonne -----------------------
		
		# trasformazione in colonne standardizzate indipendentemente dalle specie --------
		if(gruppo() == "ovicaprini"){
			colnames(dati) <- col_standard_ovicaprini
		}
		if(gruppo() == "bovini"){
			colnames(dati) <- col_standard_bovini
		}
		dati <- dati[, col_standard]
		
		# provenienza ------
		# la provenienza è ricavata dalla chiave del codice di stalla storico che rimanda alla provincia attuale, salvo errori che devono 2do essere evidenziati
		# seleziono la provenienza dall'Italia
		
		dati$IT_p <- !is.na(dati$orig_stabilimento_cod)
		# ricavo l'origine dal codice di stalla italiano
		dati$orig_com_stor <- substr(dati$orig_stabilimento_cod, 1, 5) # parte comunale del codice di stalla storico
		dati <- merge(dati, df_codici_stabilimento[,c("COD_STABILIMENTO", "COD_UTS_DT_FI")], #"COD_UTS_DT_FI" è il codice provincia attuale (Torino = 201)
									by.x = "orig_com_stor",
									by.y = "COD_STABILIMENTO",
									all.x = T,
									all.y = F)
		# dati <- merge(dati, df_province[c("COD_STABILIMENTO", "COD_UTS_DT_FI")])

		# nascita --------
		# nato in italia
		dati$IT_n <- grepl("^IT", dati$capo_identificativo)
		dati$prov_nascita <- NA
		# i nati in italia hanno le tre cifre del codice istat della provincia dopo it
		dati$prov_nascita[dati$IT_n] <- substr(dati$capo_identificativo[dati$IT_n], 3, 5)
		# se non sono tutte cifre è nullo
		dati$prov_nascita[!grepl("^[0-9]{3}$", dati$prov_nascita)] <- NA
		# porto a COD_UTS_DT_FI
		dati <- merge(dati, df_province[, c("COD_UTS_DT_FI", "PRO_STOR")],
									by.x = "prov_nascita",
									by.y = "PRO_STOR",
									all.x = T,
									all.y = F,
									suffixes = c("_p", "_n")
									)
		# dati$prov_nascita <- df_province$COD_UTS_DT_FI[match(dati$prov_nascita, df_province$PRO_STOR)]
		
		
	## capi vecchi ------
		# i capi vecchi hanno la sigla della provincia dopo it:
		# si lavora sulla selezione dei capi che iniziano per IT e che hanno prov_nascita nullo
		dati$prov_nascita_vec <- NA
		dati$IT_n_vec <- dati$IT_n & is.na(dati$prov_nascita) # nato in IT e ha codice vecchio
		# estraggo la sigla automobilistica
		dati$prov_nascita_vec[dati$IT_n_vec] <- substr(dati$capo_identificativo[dati$IT_n_vec], 3, 4) # terzo e 4° carattere (sigla automobilistica)
		# se non sono tutte lettere è nullo
		dati$prov_nascita_vec[!grepl("^[A-Z]{2}$", dati$prov_nascita_vec)] <- NA

		# collego la sigla con il codice istat della provincia di nascita (capi vecchi)
		dati$prov_nascita[!is.na(dati$prov_nascita_vec)] <- df_province$COD_UTS_DT_FI[match(dati$prov_nascita_vec[!is.na(dati$prov_nascita_vec)], df_province$PRO_STOR_SIGLA)]
		# se non trova corrispondenza rimane NA
		dati$COD_UTS_DT_FI_n[!is.na(dati$prov_nascita_vec)] <-  dati$prov_nascita[!is.na(dati$prov_nascita_vec)]
		
		dati <- dati[, setdiff(names(dati), c("IT_n_vec", "prov_nascita_vec", "prov_nascita"))] # per eliminare colonne inutili, inserire anche la denominazione della provincia di nascita e di provenienza
		
		
		# capi problematici da elencare se ce ne sono (nato in italia ma con codice di nascita NA o non nell'elenco attuale)
		# di cui non è stato possibile determinare con certezza la provincia di origine (idem di nascita)
		# oppure proveniente da Italia ma con codici di nascita non %in% chiave_province$COD_UTS_DT_FI
		dati$problema_p <- dati$IT_p & (!(dati$COD_UTS_DT_FI_p %in% df_province$COD_UTS_DT_FI))
		dati$problema_n <- dati$IT_n & (!(dati$COD_UTS_DT_FI_n %in% df_province$COD_UTS_DT_FI))
		
		# collego le malattie
		

		
	
		
		dati
		})
	})
}