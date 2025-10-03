# genera una chiave a partire dai dati istat per tradurre qualunque codice di
# stabilimento storico in un comune e una provincia attuale

# genera anche una tabella delle province attuali e storiche

# pacchetti
library(httr2)
library(jsonlite)
library(dplyr)
# library(stringr)
library(tidyr)
# library(purrr)
# library(tidyselect)
# library(lubridate)


		data_da = "01/01/1991"
		data_a  = format(Sys.Date(), "%d/%m/%Y")   # metti la data odierna

	# endpoint
	url_304 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=304&pdatada=", data_da, "&pdataa=", data_a)
	# url_61  <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
	# 									"pfun=61&pdata=", data_a)
	# url_129 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
	# 									"pfun=129&pdata=", data_da)
	
	url_64 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?pfun=64&pdata=", data_da)
	
	url_113 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=113&pdatada=17/03/1861&pdataa=", data_a)
	
	message("Scarico SITUAS (304, 61, 129, 113)…")
	df_trans <- fromJSON(url_304, simplifyDataFrame = TRUE)$resultset  # storico -> attuale
	# df_now   <- fromJSON(url_61, simplifyDataFrame = TRUE)$resultset    # anagrafica comuni/province alla data_a
	# df_var   <- fromJSON(url_129, simplifyDataFrame = TRUE)$resultset   # variazioni (per passaggi di provincia)
	df_prov  <- fromJSON(url_113, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (storiche) + validità
	
	df_prov_val <- fromJSON(url_64, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (attuali)
	
	temp_prov_attu <- df_prov[, c("COD_UTS", "DEN_UTS", "SIGLA_UTS")]
	temp_prov_stor <- df_prov[, c("COD_UTS_REL", "DEN_UTS_REL", "SIGLA_UTS_REL")]
	temp_prov_val <- df_prov_val[, c("COD_UTS", "DEN_UTS", "SIGLA_AUTOMOBILISTICA", "COD_REG")]
	colnames(temp_prov_stor) <- colnames(temp_prov_attu)
	colnames(temp_prov_val) <- c(colnames(temp_prov_attu), "COD_REG")
	
	chiave_regioni <- df_prov_val[, c("COD_REG", "DEN_REG")]
	# elimino duplicati
	chiave_regioni <- chiave_regioni[!duplicated(chiave_regioni$COD_REG), ]
	
	chiave_province <- rbind(temp_prov_attu, temp_prov_stor)
	# elimino non completi e duplicati
	chiave_province <- chiave_province[complete.cases(chiave_province),]
	chiave_province <- chiave_province[!duplicated(chiave_province$SIGLA_UTS), ]
	# per le province storiche metto regione NA
	chiave_province$COD_REG <- NA

	# tolgo quelli che sono validi in modo da non avere duplicati e da avere la denominazione più nuova in base ai codici
	chiave_province <- chiave_province[!chiave_province$COD_UTS %in% temp_prov_val$COD_UTS, ]
	# aggiungo le sigle attuali
	chiave_province <- rbind(chiave_province, temp_prov_val)


	# separo i primi 3 caratteri della stringa df_trans$PRO_COM_T
	df_trans$PRO_STOR <- substr(df_trans$PRO_COM_T, 1, 3) # codice provincia storica
	
	# separo gli ultimi 3 caratteri della stringa df_trans$PRO_COM_T
	df_trans$COM_STOR <- substr(df_trans$PRO_COM_T, 4, 6)
	
	# genero la sigla della provincia storica
	df_trans$PRO_STOR_SIGLA <- chiave_province$SIGLA_UTS[match(df_trans$PRO_STOR, chiave_province$COD_UTS)]
	df_trans$COD_STABILIMENTO <- paste0(df_trans$COM_STOR, df_trans$PRO_STOR_SIGLA)
	
	chiave_province <- unique(df_trans[, c("COD_UTS_DT_FI", "DEN_UTS_DT_FI", "COD_REG_DT_FI", "DEN_REG_DT_FI", "PRO_STOR", "PRO_STOR_SIGLA")])
	
	chiave_codici_stabilimento <- df_trans[, c("FLAG_VALIDO", "PRO_COM_T_DT_FI", "COMUNE_DT_FI", 
																						 "COD_UTS_DT_FI", "DEN_UTS_DT_FI", "COD_REG_DT_FI", "DEN_REG_DT_FI", 
																						  "COD_STABILIMENTO")]
# elimino tutto tranne chiave_codici_stabilimento e chiave_province
rm(list = setdiff(ls(), c("chiave_codici_stabilimento", "chiave_province", "chiave_regioni")))

write.csv(chiave_codici_stabilimento, "data_static/chiave_codici_stabilimento.csv")
rm(chiave_codici_stabilimento)

write.csv(chiave_province, "data_static/chiave_province.csv")
rm(chiave_province)

write.csv(chiave_regioni, "data_static/chiave_regioni.csv")
rm(chiave_regioni)

