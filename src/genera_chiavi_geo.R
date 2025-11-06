# genera:
# - una chiave a partire dai dati istat per tradurre qualunque codice di
# stabilimento storico in una provincia attuale cui legare il file delle malattie (viene preferito però lo stabilimento ricavato da BDN)
# - una tabella delle province attuali e storiche (con rimando alle regioni)
# - una tabella delle regioni
# le tabelle vengono salvate in data_static
# il file viene eseguito all'interno di genera_chiavi_geo_da_bdn.R

# pacchetti
library(httr2)
library(jsonlite)
library(dplyr)
# library(stringr)
library(tidyr)
# library(purrr)
# library(tidyselect)
# library(lubridate)
library(sf)

oggetti_inizio_script <- ls() # serve a eliminare al fondo tutti gli oggetti temporanei eccetto quelli che servono

# caricamento dati iniziali --------
## comuni ISTAT --------
comuni <- st_read("src/stabilimenti_BDN/Limiti01012025/Com01012025/Com01012025_WGS84.shp") |> 
	st_make_valid() # shapefile fonte istat (non caricato in repository, ma aggiornato)

df_comuni <- st_drop_geometry(comuni)
# porto da numerico a due caratteri con zero iniziale se necessario il campo COD_REG di df_comuni
df_comuni$COD_RIP  <- sprintf("%01d", as.integer(df_comuni$COD_REG))
df_comuni$COD_REG  <- sprintf("%02d", as.integer(df_comuni$COD_REG))
df_comuni$COD_PROV <- sprintf("%03d", as.integer(df_comuni$COD_PROV))
df_comuni$COD_UTS  <- sprintf("%03d", as.integer(df_comuni$COD_UTS))

df_prov_val_reg <- unique(df_comuni[, c("COD_RIP", "COD_REG", "COD_PROV", "COD_UTS")]) # ricavo le province dai comuni

## dati SITUAS --------
		data_da = "01/01/1991"
		data_a  = format(Sys.Date(), "%d/%m/%Y")   # metti la data odierna

	# endpoint
	url_304 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=304&pdatada=", data_da, "&pdataa=", data_a)
	url_61  <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=61&pdata=", data_a)
	# url_129 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
	# 									"pfun=129&pdata=", data_da)
	
	url_64 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?pfun=64&pdata=", data_da)
	url_112 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=112&pdatada=17/03/1861&pdataa=", data_a)	
	url_113 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
										"pfun=113&pdatada=17/03/1861&pdataa=", data_a)
	url_102 <- "https://situas-servizi.istat.it/publish/reportspooljson?pfun=102&pdata=01/01/2006"
	url_114 <- "https://situas-servizi.istat.it/publish/reportspooljson?pfun=114&pdatada=17/03/1861&pdataa=12/10/2025"
	# url_105 <- "https://situas-servizi.istat.it/publish/reportspooljson?pfun=105&pdatada=17/03/1861&pdataa=12/10/2025"
	url_104 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?pfun=104&pdatada=17/03/1861&pdataa=", data_a)
	url_105 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?pfun=105&pdatada=01/01/2025&pdataa=", data_a)

										
	
	df_trans_comuni_2025 <- fromJSON(url_105, simplifyDataFrame = TRUE)$resultset  # storico -> attuale 2025
	df_trans <- fromJSON(url_304, simplifyDataFrame = TRUE)$resultset  # storico -> attuale
	# df_trans2 <- fromJSON(url_105, simplifyDataFrame = TRUE)$resultset  # storico -> attuale TEST
	df_comuni_now   <- fromJSON(url_61, simplifyDataFrame = TRUE)$resultset    # anagrafica comuni/province alla data_a
	# df_var   <- fromJSON(url_129, simplifyDataFrame = TRUE)$resultset   # variazioni (per passaggi di provincia)
	df_prov  <- fromJSON(url_113, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (storiche) + validità
	# df_prov_tutte <- fromJSON(url_104, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (storiche) + validità
	# df_test   <- fromJSON(url_102, simplifyDataFrame = TRUE)$resultset    # regioni con SIGLE
	df_test3  <- fromJSON(url_112, simplifyDataFrame = TRUE)$resultset    # comuni con SIGLE (attuali)st2  <- fromJSON(url_114, simplifyDataFrame = TRUE)$resultset    # comuni con SIGLE (storici) + validità
	# df_test3  <- fromJSON(url_112, simplifyDataFrame = TRUE)$resultset    # comuni con SIGLE (attuali)
	
	df_prov_val <- fromJSON(url_64, simplifyDataFrame = TRUE)$resultset    # province valide/UTS con SIGLE (attuali)
	
	# elaborazioni dati --------

	
	# unisco il codice regione alle variazioni di province storiche per avere il campo regione sia delle province attuali che delle province storiche
	df_prov <- merge(df_prov,
										df_prov_val_reg[, c("COD_UTS", "COD_REG")],
										by = "COD_UTS",
										all.x = T,
										all.y = F,
										suffixes = c("", "_VAL")
										)
	
	temp_prov_attu <- df_prov[, c("COD_UTS", "DEN_UTS", "SIGLA_UTS", "COD_REG")]
	temp_prov_stor <- df_prov[, c("COD_UTS_REL", "DEN_UTS_REL", "SIGLA_UTS_REL", "COD_REG")]
	temp_prov_val  <- df_prov_val[, c("COD_UTS", "DEN_UTS", "SIGLA_AUTOMOBILISTICA", "COD_REG")]
	colnames(temp_prov_stor) <- colnames(temp_prov_attu)
	colnames(temp_prov_val)  <- colnames(temp_prov_attu)
## regioni --------
	chiave_regioni <- df_prov_val[, c("COD_REG", "DEN_REG")]
	# elimino duplicati
	chiave_regioni <- chiave_regioni[!duplicated(chiave_regioni$COD_REG), ]
	
## province --------
	# unisco le province attuali e storiche
	# elimino le df_prov_stor che hanno COD_UTS già in df_prov_
	temp_prov_stor <- temp_prov_stor[!(temp_prov_stor$COD_UTS %in% temp_prov_attu$COD_UTS), ]
	# unisco
	chiave_province <- rbind(temp_prov_attu, temp_prov_stor)

	
	# elimino non completi e duplicati
	chiave_province <- chiave_province[complete.cases(chiave_province[, c("COD_UTS", "SIGLA_UTS")]), ]
	chiave_province <- chiave_province[!duplicated(chiave_province$SIGLA_UTS), ]


	# tolgo i record che sono validi in modo da non avere duplicati e riaggiungere dalle province valide in modo da avere la denominazione più nuova 
	# (es Latina e non Littoria) in base ai codici
	chiave_province <- chiave_province[!chiave_province$COD_UTS %in% temp_prov_val$COD_UTS, ]
	# aggiungo  di nuovo le sigle attuali con le denominazioni valide attualmente
	chiave_province <- rbind(chiave_province, temp_prov_val)
	# elimino le province giuliano-dalmate cedute ad altro stato
	chiave_province <- chiave_province[as.numeric(chiave_province$COD_UTS) <700,]
	
	# siccome attualmente tutti gli NA sono in regione sardegna inserisco a mano il codice
	# chiave_province$COD_REG[is.na(chiave_province$COD_REG)] <- "20"
	if(sum(is.na(chiave_province$COD_REG))>0){
		stop("Ci sono province senza codice regione, controlla!")
	}

	## codici stabilimento --------
	# creo il codice stabilimento storico a partire dal codice istat storico
	# il codice istat storico è composto da 6 caratteri, i primi 3 sono il codice provincia storica, gli ultimi 3 il codice del comune storico
	# il codice stabilimento è composto da 6 caratteri, i primi 3 sono il codice del comune storico , gli ultimi 3 la sigla della provincia storica (nel momento in cui viene registrato lo stabilimento) variando le province e i comuni e di conseguenza le loro sigle e codici istat i codici stabilimento rimangono invariati, ponendo attenzione al fatto che non ci siano ambiguità
	# 
	# 
	# separo i primi 3 caratteri della stringa df_trans$PRO_COM_T
	df_trans$PRO_STOR <- substr(df_trans$PRO_COM_T, 1, 3) # codice provincia storica
	
	# separo gli ultimi 3 caratteri della stringa df_trans$PRO_COM_T
	df_trans$COM_STOR <- substr(df_trans$PRO_COM_T, 4, 6)
	
	# genero la sigla della provincia storica
	df_trans$PRO_STOR_SIGLA <- chiave_province$SIGLA_UTS[match(df_trans$PRO_STOR, chiave_province$COD_UTS)]
	# non funzionano quelli di Sassari, li collego tramite denominazione
	df_trans <- merge(df_trans, unique(chiave_province[, c("SIGLA_UTS", "DEN_UTS")],
																		 ),
										by.x = "DEN_UTS_DT_FI",
										by.y = "DEN_UTS",
										all.x = T,
										suffixes = c("", "_y")
										)
	# se PRO_STOR_SIGLA è NA lo prendo
	df_trans[is.na(df_trans$PRO_STOR_SIGLA), "PRO_STOR_SIGLA"] <- df_trans[is.na(df_trans$PRO_STOR_SIGLA), "SIGLA_UTS"]

	df_trans$COD_STABILIMENTO <- paste0(df_trans$COM_STOR, df_trans$PRO_STOR_SIGLA)
	
	# chiave_province <- unique(df_trans[, c("COD_UTS_DT_FI", "DEN_UTS_DT_FI", "COD_REG_DT_FI", "DEN_REG_DT_FI", "PRO_STOR", "PRO_STOR_SIGLA")])
	
	# per collegarsi ai files di indennità dalle malattie, la chiave che conta è quella della provincia attuale, COD_UTS_FI
	# se non avviene il merge perché la chiave non contempla il caso occorre evidenziare i record per verifica manuale
	chiave_codici_stabilimento <- df_trans[, c("FLAG_VALIDO", "PRO_COM_T_DT_FI", "PRO_STOR", "COMUNE_DT_FI", 
																						 "COD_UTS_DT_FI", "DEN_UTS_DT_FI", "COD_REG_DT_FI", "DEN_REG_DT_FI", 
																						  "COD_STABILIMENTO")]
	#dato che le province talvolta vengono modificate e rimodificate ci sono dei doppioni nel codice stabilimento, 
	# vanno eliminati tenendo solo quelli con il flag non valido senza pericolo se puntano alla stessa provincia
	# se puntano a due province diverse conviene mettere un flag di attenzione per sicurezza di controllo manuale
	# oppure eliminare il record
	
	# riordino
	chiave_codici_stabilimento <- chiave_codici_stabilimento[order(chiave_codici_stabilimento$COD_UTS_DT_FI,
																																 chiave_codici_stabilimento$COD_STABILIMENTO, 
																																 -chiave_codici_stabilimento$FLAG_VALIDO), ]
	
	# chiavi non duplicate (univoche da usare per eliminare quelle che sono evidentemente sbagliate per georefenziazione
	# nell'approccio bdn)
duplicati <- duplicated(chiave_codici_stabilimento$COD_STABILIMENTO)
chiave_stab_nondup <- chiave_codici_stabilimento[!(chiave_codici_stabilimento$COD_STABILIMENTO %in% 
																									 	chiave_codici_stabilimento$COD_STABILIMENTO[duplicati]), 
																								 c( "PRO_COM_T_DT_FI", 
																								 	"COD_STABILIMENTO")]
sum(duplicated(chiave_stab_nondup$COD_STABILIMENTO))	

	# elimino quelli che sono duplicati per codice stabilimento, per provincia, per comune attuale e anche validi
	# individua tutti gli uguali, anche la prima copia (giocando con fromLast = T)
	dup_codStab_codUtsDtFi <- duplicated(chiave_codici_stabilimento[, c("COD_STABILIMENTO", "COD_UTS_DT_FI", "PRO_COM_T_DT_FI")]) |
		duplicated(chiave_codici_stabilimento[, c("COD_STABILIMENTO", "COD_UTS_DT_FI", "COMUNE_DT_FI")],  fromLast = T)
	# elimino quelli se sono anche valid=T dato che in Sardegna si vede che prevale il codice precedente consolidato
		chiave_codici_stabilimento_temp <- chiave_codici_stabilimento[!(dup_codStab_codUtsDtFi & chiave_codici_stabilimento$FLAG_VALIDO == 1), ] # codici stabilimento duplicati che ricadono nella stessa provincia attuale
	# elimino i duplicati con lo stesso comune (PRO_COM_T_DT_FI), che dovrebbero essere 3
		chiave_codici_stabilimento_temp <- chiave_codici_stabilimento_temp[!duplicated(chiave_codici_stabilimento_temp[, c("COD_STABILIMENTO", "COD_UTS_DT_FI", "PRO_COM_T_DT_FI")]), ]
		
		# quanti ne restano? -> 269
		sum(duplicated(chiave_codici_stabilimento_temp$COD_STABILIMENTO))
	# quanti ne restano con il codice uguale e anche la provincia definitiva uguale? --> 192 che a questo punto hanno comune definitivo diverso
		nrow(chiave_codici_stabilimento_temp[duplicated(chiave_codici_stabilimento_temp[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")]), ])
		chiave_codici_stabilimento_dup <- chiave_codici_stabilimento_temp[duplicated(chiave_codici_stabilimento_temp[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")]) |
																	 	duplicated(chiave_codici_stabilimento_temp[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")], fromLast = T), ]
		# per questi impongo comune = 999 ma lascio la connessione con la provincia se la provincia è univoca
		
		
		# nella fase successiva elimino il duplicato
		
		# individuo i duplicati con provincia diversa
		
		# impongo codice provincia 999 così il merge non avviene e possono essere facilmente diagnosticati (provincia ambigua)
		
		chiave_codici_stabilimento_temp$COMUNE_DT_FI[duplicated(chiave_codici_stabilimento_temp$COD_STABILIMENTO)] <- "<AMBIGUO>"
		
	# impongo COD_UTS_DT_FI = 999 agli altri duplicati doppi
	
	sum(duplicated(chiave_codici_stabilimento[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")]))
	# tra quelli che hanno il duplicato che ricade nella provincia attuale elimino quelli validi
	index_duplicati <- which(chiave_codici_stabilimento$COD_STABILIMENTO %in% duplicati)
	# di questi devo togliere solo quelli che hanno codice stabilimento uguale, valido =1 e provincia finale uguale
	
	chiave_codici_stabilimento_temp  <- chiave_codici_stabilimento[!((chiave_codici_stabilimento$COD_STABILIMENTO %in% duplicati) & 
																																 	(chiave_codici_stabilimento$FLAG_VALIDO == 1)), ]
	
	chiave_codici_stabilimento <- chiave_codici_stabilimento[!(duplicated(chiave_codici_stabilimento[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")]) &
																													 	chiave_codici_stabilimento$FLAG_VALIDO == 1), ]
	
	duplicati <- chiave_codici_stabilimento$COD_STABILIMENTO[duplicated(chiave_codici_stabilimento[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")])] # codici stabilimento duplicati che ricadono nella stessa provincia attuale
	
	
	
	duplicatiNonStessaProvincia <- chiave_codici_stabilimento$COD_STABILIMENTO[duplicated(chiave_codici_stabilimento$COD_STABILIMENTO)] # codici stabilimento duplicati che ricadono in province diverse

	
	
	# chiave_codici_stabilimento <- chiave_codici_stabilimento[!duplicated(chiave_codici_stabilimento[, c("COD_STABILIMENTO", "COD_UTS_DT_FI")]), ]
	sum(duplicated(chiave_codici_stabilimento_temp[, c("COD_STABILIMENTO", "FLAG_VALIDO")]))
	chiave_codici_stabilimento_temp
	chiave_codici_stabilimento_temp$COD_STABILIMENTO[duplicated(chiave_codici_stabilimento_temp[, c("COD_STABILIMENTO", "FLAG_VALIDO")])]
	
	# elimino i codici stabilimento doppi che sono doppi anche per COD_UTS_DT_FI
	# chiave_codici_stabilimento_temp <- chiave_codici_stabilimento$COD_STABILIMENTO[!duplicated(chiave_codici_stabilimento]
	
	chiave_codici_stabilimento[chiave_codici_stabilimento$COD_STABILIMENTO %in% duplicati, ]
	
# elimino tutto tranne chiave_codici_stabilimento chiave_regioni e df_prov_val # ho tolto la chiave province generata al fondo
rm(list = setdiff(ls(), c(oggetti_inizio_script, "chiave_codici_stabilimento", "chiave_province", "chiave_regioni", "chiave_stab_nondup", "df_prov_val")))





# write.csv(chiave_codici_stabilimento, "data_static/chiave_codici_stabilimento.csv")
# rm(chiave_codici_stabilimento)

# write.csv(df_prov_val, "data_static/chiave_province.csv")
# rm(df_prov_val)
# rm(chiave_province)
# 
# write.csv(chiave_regioni, "data_static/chiave_regioni.csv")
# rm(chiave_regioni)

