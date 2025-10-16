# modulo per standardizzare le colonne

mod_standardize_server <- function(id, animali, gruppo) {               # definizione del server del modulo
	moduleServer(id, function(input, output, session) {            # modulo server vero e proprio
		
		reactive({                                             # valore reattivo restituito
			req(animali())                                 # richiede che i dati siano presenti
			req(gruppo())                                  # richiede che il gruppo sia definito
						
		dati <- animali()
		
		# source("tests/test.R")
		
			# carico tabelle di supporto
			df_codici_stabilimento <- read.csv("data_static/chiave_codici_stabilimento.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella comuni e province
			df_province <- read.csv("data_static/chiave_province.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella province
			df_regioni <- read.csv("data_static/chiave_regioni.csv", stringsAsFactors = FALSE, colClasses = "character") # tabella regioni
			df_decodifica <- read.csv("data_static/chiave_decodifica.csv", stringsAsFactors = FALSE) # tabella decodifica
			
			# carico tutte le tabelle (files.xlsx) presenti in data_static/malattie e distinguo tra gli elenchi di comuni 
	    files_malattie <- list.files("data_static/malattie", pattern = "\\.xlsx$", full.names = TRUE)
	    col_malattie <- 
			for(i in 1:length(files_malattie)){
				assign(paste0("malattia_", tools::file_path_sans_ext(basename(files_malattie[i]))), 
								readxl::read_excel(files_malattie[i], col_types = "text"))
			}
			
			
			# attenzione ai comuni non validi (flag nella tabella) quando collego
			
			

		
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