# genera una chiave a partire dai dati istat per tradurre qualunque codice di
# stabilimento storico in un comune e una provincia attuale

# pacchetti
library(httr2)
library(jsonlite)
# library(dplyr)
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
	url_113 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=113&pdatada=17/03/1861&pdataa=", data_a)
	
	message("Scarico SITUAS (304, 61, 129, 113)…")
	df_trans <- fromJSON(url_304, simplifyDataFrame = TRUE)$resultset  # storico -> attuale
	# df_now   <- fromJSON(url_61, simplifyDataFrame = TRUE)$resultset    # anagrafica comuni/province alla data_a
	# df_var   <- fromJSON(url_129, simplifyDataFrame = TRUE)$resultset   # variazioni (per passaggi di provincia)
	df_prov  <- fromJSON(url_113, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (storiche) + validità
	
	temp_prov_attu <- df_prov[, c("COD_UTS", "DEN_UTS", "SIGLA_UTS")]
	temp_prov_stor <- df_prov[, c("COD_UTS_REL", "DEN_UTS_REL", "SIGLA_UTS_REL")]
	colnames(temp_prov_stor) <- colnames(temp_prov_attu)
	
		chiave_province <- bind_rows(temp_prov_attu, temp_prov_stor) %>%
		distinct() %>%          # equivale a unique()
		drop_na()               # elimina le righe con NA (complete.cases)
	
	# separo i primi 3 caratteri della stringa df_trans$PRO_COM_T
	df_trans$PRO_STOR <- substr(df_trans$PRO_COM_T, 1, 3) # codice provincia storica
	
	# separo gli ultimi 3 caratteri della stringa df_trans$PRO_COM_T
	df_trans$COM_STOR <- substr(df_trans$PRO_COM_T, 4, 6)
	
	# genero la sigla della provincia storica
	df_trans$PRO_STOR_SIGLA <- chiave_province$SIGLA_UTS[match(df_trans$PRO_STOR, chiave_province$COD_UTS)]
	df_trans$COD_STABILIMENTO <- paste0(df_trans$COM_STOR, df_trans$PRO_STOR_SIGLA)
	
	chiave_codici_stabilimento <- df_trans[, c("FLAG_VALIDO", "PRO_COM_T_DT_FI", "COMUNE_DT_FI", 
																						 "COD_UTS_DT_FI", "DEN_UTS_DT_FI", "COD_REG_DT_FI", "DEN_REG_DT_FI", 
																						  "COD_STABILIMENTO")]

rm(temp_prov_stor, temp_prov_attu, df_var, df_trans, df_prov, df_now, data_da, data_a, url_304, url_113, chiave_province, df_prov, df_trans)

write.csv(chiave_codici_stabilimento, "data_static/chiave_codici_stabilimento.csv")
rm(chiave_codici_stabilimento)
