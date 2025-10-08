# modulo per standardizzare le colonne

mod_standardize_server <- function(id, animali, gruppo) {               # definizione del server del modulo
	moduleServer(id, function(input, output, session) {            # modulo server vero e proprio
		
		reactive({                                             # valore reattivo restituito
			req(animali())                                 # richiede che i dati siano presenti
			req(gruppo())                                  # richiede che il gruppo sia definito
			
			# carico tabelle di supporto
			df_codici_stabilimento <- read.csv("data_static/chiave_codici_stabilimento.csv", stringsAsFactors = FALSE) # tabella comuni e province
			df_province <- read.csv("data_static/chiave_province.csv", stringsAsFactors = FALSE) # tabella province
			df_regioni <- read.csv("data_static/chiave_regioni.csv", stringsAsFactors = FALSE) # tabella regioni
			
		dati <- animali()
		
		if(gruppo() == "ovicaprini"){
			colnames(dati) <- col_standard_ovicaprini
		}
		if(gruppo() == "bovini"){
			colnames(dati) <- col_standard_bovini
		}
		dati <- dati[, col_standard]
		# origine ------
		# ricavo l'origine dal codice di stalla italiano
		dati$orig_com_stor <- substr(dati$orig_stabilimento_cod, 1, 5) # parte comunale del codice di stalla storico
		dati <- merge(dati, df_codici_stabilimento[,-1], 
									by.x = "orig_com_stor",
									by.y = "COD_STABILIMENTO",
									all.x = T,
									all.y = F)
		# nascita --------
		# nato in italia
		dati$IT_n <- grepl("^IT", dati$capo_identificativo)
		dati$prov_nascita <- NA
		# i nati in italia hanno le tre cifre del codice istat della provincia dopo it
		dati$prov_nascita[dati$IT_n] <- substr(dati$capo_identificativo[dati$IT_n], 3, 5)
		# se non sono tutte cifre è nullo
		dati$prov_nascita[!grepl("^[0-9]{3}$", dati$prov_nascita)] <- NA
		
	# capi vecchi ------
		# i capi vecchi hanno la sigla della provincia dopo it:
		# si lavora sulla selezione dei capi che iniziano per IT e che hanno prov_nascita nullo
		dati$prov_nascita_vec <- NA
		dati$IT_n_vec <- dati$IT_n & is.na(dati$prov_nascita) # nato in IT e non ha codice giovane
		dati$prov_nascita_vec[dati$IT_n_vec] <- substr(dati$capo_identificativo[dati$IT_n_vec], 3, 4) # terzo e 4° carattere (sigla automobilistica)
		# se non sono tutte lettere è nullo
		dati$prov_nascita_vec[!grepl("^[A-Z]{2}$", dati$prov_nascita_vec)] <- NA

		# collego la sigla con il codice istat della provincia di nascita (capi vecchi)
		dati$prov_nascita[!is.na(dati$prov_nascita_vec)] <- df_province$COD_UTS[match(dati$prov_nascita_vec[!is.na(dati$prov_nascita_vec)], df_province$SIGLA_UTS)]
		# se non trova corrispondenza rimane NA
		# 2do: da richiamare dopo tra i capi problematici da elencare se ce ne sono (nato in italia ma con codice di nascita NA)
		
		## provenienza ------
		# la provenienza è ricavata dalla chiave del codice di stalla storico che rimanda alla provincia attuale, salvo errori che devono 2do essere evidenziati
		# seleziono la provenienza dall'Italia
		
		dati$IT_p <- !is.na(dati$orig_stabilimento_cod)
		
		
		dati$prov_provenienza <- NA
		
		# estraggo la chiave del comune da dati$orig_stabilimento_cod
		dati$orig_com_stor <- substr(dati$orig_stabilimento_cod, 1, 5) # parte comunale del codice di stalla storico
		# uso match per aggiungere il codice istat della provincia di provenienza al capo
		dati$prov_provenienza <- NA
		# utilizzo match al dataframe "df_codici_stabilimento" per aggiungere il codice istat al capo "prov_provenienza"
		dati$prov_provenienza <- 
		
		
		dati$prov_provenienza <- df_codici_stabilimento$COD_UTS_DT_FI[match(dati$orig_com_stor, df_codici_stabilimento$COD_STABILIMENTO)]
		
		
		
		# ## provenienza -------
		# # proveniente da IT_p vero o falso (se `AZI PROVENIENZA` non è nullo)
		# ovi$IT_p <- !is.na(ovi$`AZI PROVENIENZA`)
		# 
		# ovi$prov_provenienza <- NA
		# #isolo il codice provincia dai caratteri da 4 a 5 del codice azienda di provenienza
		# ovi$prov_provenienza[ovi$IT_p] <- substr(ovi$`AZI PROVENIENZA`[ovi$IT_p], 4, 5)
		# 
		# # trasformo il codice provincia in codice istat
		# ovi$prov_provenienza <- prov$`Codice Provincia (Storico)(1)`[match(ovi$prov_provenienza, prov$`Sigla automobilistica`)]
		
		
		# # utilizzo match al dataframe "prov" per aggiungere il codice istat al capo "prov_nascita"
		# ovi$prov_nascita <- prov$`Codice Provincia (Storico)(1)`[match(ovi$prov_nascita_giov, prov$`Codice Provincia (Storico)(1)`)]
		# ovi$prov_nascita[!is.na(ovi$prov_nascita_vec)] <- prov$`Codice Provincia (Storico)(1)`[match(ovi$prov_nascita_vec[!is.na(ovi$prov_nascita_vec)], prov$`Sigla automobilistica`)]
		

		
		
		dati
		})
	})
}