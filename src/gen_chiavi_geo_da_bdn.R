# # script per generare le chiavi geografiche dalla estrazione dei codici di stabilimento da interrogazioni BDN
# # chiave <- iniziale codice aziendale, codice istat comune, codice istat provincia
# # dati estratti da applicativo Interrogazione BDN (anagint)
# # -> Stampa lista stabilimenti con coordinate geografiche
# # senza selezione regioni
# # senza selezione ASL
# # selezIone gruppo specie BOVINI E BUFALINI e poi OVINI E CAPRINI
# # flag "Escludi stabilimenti chiuse" ON (valutare di passare a OFF)
# # flag "Escludi stabilimenti in cui non esistono allevamenti aperti  della specie selezionata" OFF
# # radiobutton "Stampa l'elenco di tutti gli stabilimenti"
# 
# preambolo ----------

library(rvest)
library(dplyr)
library(janitor)
library(stringr)
library(stringi)
library(readr)
library(lubridate)
library(sf)

# genero chiave_province, regioni e chiave stabilimento per integrare i comuni che non hanno nessun allevamento all'interno (2do: mettere un flag ai codici derivanti da situazioni diverse) con genera_chiavi_geo.R
source("src/genera_chiavi_geo.R")

# # importazione files stabilimenti da BDN ---------
# # il file non è un vero xls binario ma un file html contentente una tabella excel


path <- vector()
path[1] <- "src/stabilimenti_BDN/BDN/bovini_tot_u.vesco_VET_coordinate_geografiche_14_10_2025_08_35_50/u.vesco_VET_coordinate_geografiche_14_10_2025_08_35_50.xls"
path[2] <- "src/stabilimenti_BDN/BDN/ovicaprini_tot_u.vesco_VET_coordinate_geografiche_14_10_2025_08_35_50/u.vesco_VET_coordinate_geografiche_14_10_2025_08_37_24.xls"
specie <- c("bovini", "ovicaprini")
df_stab <- NULL

for (i in 1:length(path)) {
	# legge come HTML (encoding dichiarato nel file)
	html <- read_html(path[i], encoding = "windows-1252")
	
	# Estrae la prima tabella
	df <- html_table(html, fill = TRUE)[[1]]
	
	# Promuove la prima riga a intestazioni
	headers <- as.character(unlist(df[1, ], use.names = FALSE))
	names(df) <- headers
	df <- df[-1, , drop = FALSE]
	
	# Pulisce nomi e spazi
	df <- df %>%
		mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
		clean_names()
	
	# Elimina eventuali righe di intestazione ripetute
	first_col <- names(df)[1]
	df <- df %>%
		filter(
			.data[[first_col]] != first_col,
			.data[[first_col]] != ""
		)
	
	df$specie <- specie[i]
	
	df_stab <- rbind(df, df_stab)
}

# pulizia memoria
rm(list = setdiff(ls(), "df_stab"))
gc()

# elaborazione stabilimenti -----
# converto a numerico le coordinate utm e gb
df_stab <- df_stab %>%
	mutate(
		latitudine = as.numeric(latitudine),
		longitudine = as.numeric(longitudine),
		nord_gb = as.numeric(nord_gb),
		est_gb = as.numeric(est_gb),
		x_utm = as.numeric(x_utm),
		y_utm = as.numeric(y_utm)
	)

df_stab$prefissi <- substr(df_stab$codice_aziendale, 1, 5)
tutti_prefissi <- unique(df_stab$prefissi)

# date: ogni colonna che inizia con "data" la interpreto come gg/mm/aaaa
date_cols <- grep("^data", names(df_stab), value = TRUE)
if (length(date_cols)) {
	df_stab <- df_stab %>%
		mutate(across(all_of(date_cols), ~ suppressWarnings(dmy(.x))))
}

# # attribuisco le coordinate lat/lon , utilizando solamente 
# source("src/stabilimenti_BDN/converti_coord_BDN.R")
# df_ok <- converti_coord(df_stab)
# semplifico solo a 

# Dati dei punti: df_stab ha colonne lon, lat (in WGS84/EPSG:4326)
#    Se le colonne si chiamano diversamente, cambia i nomi sotto
# seleziono solo i record con coordinate valorizzate
pts_ll <- st_as_sf(df_stab[complete.cases(df_stab[, c("longitudine", "latitudine")]), ], 
									 coords = c("longitudine","latitudine"), crs = 4326, remove = FALSE)

# Porta i punti nello stesso CRS dei poligoni dei comuni
pts_utm <- st_transform(pts_ll, st_crs(comuni))

# Join spaziale: attribuisce ai punti gli attributi del poligono che li contiene
#    Scegli le colonne dei Comuni che ti servono
# Join spaziale: aggiunge *tutti* i campi di `comuni` ai punti
out <- st_join(
	pts_utm,
	comuni,             # niente st_drop_geometry qui
	join = st_within,   # per includere i bordi: usa st_intersects
	left = TRUE
)

# data.frame "piatto" senza geometria finale:
risultato <- st_drop_geometry(out)

# estraggo i primi 5 caratteri del codice azienda
risultato$cod_stab <- substr(risultato$codice_aziendale, 1, 5)

risultato <- risultato[, c("regione", "azienda_asl", 
													 "comune", "cod_stab", "COD_RIP", "COD_REG", 
													 "COD_PROV", "COD_UTS", "PRO_COM_T", "COMUNE", 
													 "CC_UTS")]

# 1) trim su tutte le character
ris <- risultato %>% mutate(across(where(is.character), ~str_squish(.x)))

# CONTEGGIO ----------
# Conteggio per combinazione di TUTTI i campi rimasti (dopo la tua riduzione)
#    -> utile per vedere duplicati/forza delle combinazioni (ci possono essere delle attribuzioni sbagliate per errori di georeferenziazione
#    )
conteggi_combinazioni <- ris %>%
	group_by(across(everything())) %>%
	summarise(n = n(), .groups = "drop") %>%
	arrange(desc(n))

conteggi_combinazioni <- conteggi_combinazioni %>%
	mutate(
		# obiettivo rendere confrontabili i nomi dei comuni che provengono dalle due fonti diverse
		# --- normalizzo il campo 'comune' (minuscole, spazi, apostrofo tipografico -> ASCII)
		comune_norm = comune |>
			str_replace_all("’", "'") |>
			str_to_lower() |>
			str_squish() |>
			# normalizza tutti i trattini/dash a " - "
			str_replace_all("\\s*/\\s*", " / ") |>                       # se per caso c'è qualche slash, lo lascio neutro
			str_replace_all("\\s*[-–—−]+\\s*", " - ") |>
			str_squish(),
		
		# --- normalizzo COMUNE: slash -> " - ", tutti i trattini -> " - "
		COMUNE_norm = COMUNE |>
			str_replace_all("’", "'") |>
			str_to_lower() |>
			str_squish() |>
			str_replace_all("\\s*/\\s*", " - ") |>                       # <-- richiesta: "/" diventa " - "
			str_replace_all("\\s*[-–—−]+\\s*", " - ") |>                 # tutti i tipi di trattino/dash
			str_squish(),
		
		# --- rimozione accenti (Latin-ASCII) e apostrofi
		comune_trans = comune_norm |>
			stringi::stri_trans_general("Latin-ASCII") |>
			str_replace_all("['`]", "") |>
			str_squish(),
		
		COMUNE_TRANS = COMUNE_norm |>
			stringi::stri_trans_general("Latin-ASCII") |>
			str_replace_all("['`]", "") |>
			str_squish()
	) %>%
	select(-comune_norm, -COMUNE_norm)



# controllo lunghezza
max(nchar(conteggi_combinazioni$COMUNE_TRANS), na.rm = T) #62 caratteri
max(nchar(conteggi_combinazioni$comune_trans), na.rm = T) #54 caratteri

# confronto accorciando COMUNE_TRANS a 50
conteggi_combinazioni$ctr_nome <- tolower(substr(conteggi_combinazioni$comune_trans, 1, 50)) == 
	tolower(substr(conteggi_combinazioni$COMUNE_TRANS, 1, 50))
conteggi_combinazioni$ctr_nome2 <- tolower(substr(conteggi_combinazioni$comune_trans, 1, 6)) == 
	tolower(substr(conteggi_combinazioni$COMUNE_TRANS, 1, 6))

table(conteggi_combinazioni$ctr_nome)

# correggo UTS da numeric a character
conteggi_combinazioni$COD_UTS <- ifelse(
	is.na(conteggi_combinazioni$COD_UTS),
	NA_character_,
	sprintf("%03d", as.integer(conteggi_combinazioni$COD_UTS))
)


conteggi_combinazioni <- merge(conteggi_combinazioni, chiave_province, all.x = T, all.y = F)

# seleziono quelli con nome non corrispondente e provincia non corrispondente
conteggi_combinazioni$cod_prov <- substr(conteggi_combinazioni$cod_stab, 4, 5)
conteggi_combinazioni$ctr_prov <- conteggi_combinazioni$cod_prov == conteggi_combinazioni$SIGLA_UTS

table(conteggi_combinazioni$n)
conteggi_combinazioni$ctr_num_magg_2 <- conteggi_combinazioni$n < 2

conteggi_combinazioni <- merge(conteggi_combinazioni, chiave_stab_nondup,
															 by.x = "cod_stab",
															 by.y = "COD_STABILIMENTO",
															 all.x = T,
															 all.y = F)
# prevedibile il cambio di nome se corrisponde invece il codice istat del comune, quindi non si considera più sbagliato
conteggi_combinazioni$ctr_prev <- (conteggi_combinazioni$PRO_COM_T_DT_FI == conteggi_combinazioni$PRO_COM_T)
table(conteggi_combinazioni$ctr_prev, conteggi_combinazioni$ctr_nome)

# se conteggi_combinazioni$ctr_nome è F ma conteggi_combinazioni$ctr_prev è T allora conteggi_combinazioni$ctr_nome deve diventare T
ix <- conteggi_combinazioni$ctr_prev & (!conteggi_combinazioni$ctr_nome)
conteggi_combinazioni$ctr_nome[ix] <- TRUE

# non funziona perché ci sono degli NA (DA TROVARE)
table(conteggi_combinazioni$ctr_nome)
sum(ix)
sum(is.na(conteggi_combinazioni$ctr_prev))
sum(is.na(conteggi_combinazioni$ctr_nome))

#tutto quello che ha un solo comune in BDN deve avere solo il 'nome comune' True ed eliminati i record che sono F
#1) controllo che non esista lo stesso codice per più comuni
ctr_bdn <- unique(conteggi_combinazioni[, c("cod_stab", "comune")])
ctr_bdn$cod_stab[(duplicated(ctr_bdn$cod_stab))]
# problematica: 024NU098 039NU090 (eccezioni unici due codici che 
# iniziano per un comune e sono effettivamente nel comune accanto): tolti quei due tutti gli altri 
# devono avere il valore True o in assenza verificare
# eccetto che "024NU" "039NU" eliminare i record con 
# conteggi_combinazioni$ctr_nome == F dove ci sono altri
# con uguale valore di conteggi_combinazioni$comune con invece 
# un record con conteggi_combinazioni$ctr_nome == T

# quali sono NA?
sum(is.na(conteggi_combinazioni$ctr_nome))
conteggi_combinazioni$cod_stab[is.na(conteggi_combinazioni$ctr_nome)]

### FILTRO ------------

df <- conteggi_combinazioni %>%
	mutate(
		ctr_nome  = coalesce(as.logical(ctr_nome),  FALSE),
		ctr_nome2 = coalesce(as.logical(ctr_nome2), FALSE)
	)

# --- eccezioni vere (Nuoro) ---
eccezioni_codici   <- c("024NU098", "039NU090")
eccezioni_prefissi <- c("024NU", "039NU")

df <- df %>%
	mutate(
		is_exc = cod_stab %in% eccezioni_codici |
			str_detect(cod_stab, paste0("^(", paste(eccezioni_prefissi, collapse="|"), ")"))
	)

# --- comune BDN autorevole per ciascun codice ---
by_cod_bdn <- df %>%
	group_by(cod_stab) %>%
	summarise(
		n_bdn = n_distinct(comune_trans, na.rm = TRUE),
		# prendi la moda (in caso di sporadici errori/NA nel campo testuale)
		comune_bdn = names(sort(table(comune_trans), decreasing = TRUE))[1],
		.groups = "drop"
	)

df1 <- df %>%
	left_join(by_cod_bdn, by = "cod_stab") %>%
	mutate(
		match_bdn = (COMUNE_TRANS == comune_bdn)
	)

# --- fallback a livello di CODICE: se non c'è nessun ctr_nome TRUE, usa ctr_nome2 ---
has_true_cod_strong <- df1 %>%
	group_by(cod_stab) %>%
	summarise(has_true_cod_strong = any(ctr_nome), .groups = "drop")

df1 <- df1 %>%
	left_join(has_true_cod_strong, by = "cod_stab") %>%
	mutate(flag_eff = if_else(has_true_cod_strong, ctr_nome, ctr_nome2))

# --- vincolo: salvo eccezioni, tieni SOLO le righe che rispettano il comune BDN ---
df2 <- df1 %>%
	group_by(cod_stab) %>%
	group_modify(~{
		d <- .x
		if (any(d$is_exc)) return(d)                       # per le eccezioni non filtriamo su match_bdn
		if (any(d$match_bdn, na.rm = TRUE)) d <- d %>% filter(match_bdn)
		d
	}) %>%
	ungroup()

# --- filtro morbido su flag_eff: se esistono TRUE, butta i FALSE; se sono tutti FALSE, tieni tutto ---
by_pair_eff <- df2 %>%
	group_by(cod_stab, COMUNE) %>%
	summarise(has_true_pair_eff = any(flag_eff), .groups = "drop")

by_cod_eff <- df2 %>%
	group_by(cod_stab) %>%
	summarise(has_true_cod_eff = any(flag_eff), .groups = "drop")

df2 <- df2 %>%
	left_join(by_pair_eff, by = c("cod_stab","COMUNE")) %>%
	left_join(by_cod_eff,  by = "cod_stab") %>%
	mutate(
		across(c(has_true_pair_eff, has_true_cod_eff), ~coalesce(.x, FALSE))
	) %>%
	filter(
		is_exc |                           # eccezioni sempre tenute
			flag_eff |                         # tieni sempre i TRUE
			!(has_true_pair_eff | has_true_cod_eff)  # se tutti FALSE per codice/coppia, tieni
	)

# --- dedup: UNA riga per codice (ctr_nome > ctr_nome2 > n) ---
df_filtrato <- df2 %>%
	mutate(rank_win = case_when(ctr_nome ~ 1L,
															ctr_nome2 ~ 2L,
															TRUE ~ 3L)) %>%
	arrange(cod_stab, rank_win, desc(n)) %>%
	group_by(cod_stab) %>%
	slice_head(n = 1) %>%
	ungroup()


# CONTROLLO ---------

 # prefissi unici in BDN
length(unique(ris$cod_stab))
prefissi <- unique(ris$cod_stab)

# prefissi nel df chiave
sum(df_filtrato$cod_stab %in% prefissi)
length(unique(df_filtrato$cod_stab))
length(df_filtrato$cod_stab)
df_filtrato$cod_stab[!(df_filtrato$cod_stab %in% prefissi)]

# normalizza prima (spazi/case) per sicurezza
ris_codes <- ris$cod_stab |> str_squish() |> str_to_upper()
cc_codes  <- df_filtrato$cod_stab |> str_squish() |> str_to_upper()

u_ris <- unique(ris_codes)
u_cc  <- unique(cc_codes)

# 1) Chi c'è in ris ma NON in conteggi_combinazioni? (i "6" che cerchi)
mancano_in_cc <- setdiff(u_ris, u_cc)

# 2) Chi c'è in conteggi_combinazioni ma NON in ris? (eventuali extra)
extra_in_cc <- setdiff(u_cc, u_ris)

length(u_ris); length(u_cc)
length(mancano_in_cc); length(extra_in_cc)

mancano_in_cc
extra_in_cc

# ci sono dei duplicati?
duplicati <- df_filtrato$cod_stab[duplicated(df_filtrato$cod_stab)]
duplicati
# ne mancano 3 e ce ne sono 97 di troppo

# ce ne sono dei record senza codice istat del comune attuale?
sum(is.na(df_filtrato$PRO_COM_T)) # quanti record?
non_collegati <- df_filtrato[is.na(df_filtrato$PRO_COM_T),]
non_collegati # quali record?

# esportazione problematici
problematici <- bind_rows(
	df_filtrato %>%
		filter(cod_stab %in% duplicati) %>%
		mutate(reason = "duplicato"),
	non_collegati %>%
		mutate(reason = "istat_missing"),
	df_filtrato %>%
		filter(cod_stab %in% mancano_in_cc | cod_stab %in% extra_in_cc) %>%
		mutate(reason = "setdiff")
) %>%
	distinct()

if(nrow(problematici) == 0) {
	message("Nessun problema rilevato.")
} else {
	message("Attenzione: sono stati rilevati problemi. Esportati in test/problemi_prefissi.xlsx")
	print(problematici)
	openxlsx::write.xlsx(problematici,
											 file = "test/problemi_prefissi.xlsx",
											 overwrite = TRUE)
}


# CORREZIONI MANUALI ----
# Isola del giglio cade fuori (un solo allevamento georeferenziato in mare?)
# impongo che PRO_COM_T 053012
df_filtrato$PRO_COM_T[df_filtrato$cod_stab == "012GR"] <- "053012"

# prefissi che sono nel dataframe originario di BDN ma che non risultano nel DF filtrato (senza coordinate LL)
df_stab$cod_stab <- df_stab$prefissi
prefissi_mancanti <- unique(df_stab$cod_stab[!(df_stab$cod_stab %in% df_filtrato$cod_stab)])
df_stab_mancanti <- df_stab[df_stab$cod_stab %in% prefissi_mancanti, ]
# aggiungo il codice UTS
df_stab_mancanti <- merge(df_stab_mancanti,
													chiave_codici_stabilimento,
													by.x = "cod_stab",
													by.y = "COD_STABILIMENTO",
													all.x = T,
													all.y = F)

# rinomina in df_stab_mancanti il campo "PRO_COM_T_DT_FI" in "PRO_COM_T"
colnames(df_stab_mancanti)[colnames(df_stab_mancanti) == "PRO_COM_T_DT_FI"] <- "PRO_COM_T"

if(!nrow(mancanti) == length(prefissi_mancanti)){
	stop("Mancano codici stabilimento in chiave_codici_stabilimento oppure ci sono codici non univoci")
}

# credo un ti
# RIORDINO DF ------
# rinomino df_stab$prefissi in df_stab$cod_stab
df_stab$cod_stab <- df_stab$prefissi

df_finale <- df_filtrato[, intersect(colnames(df_stab_mancanti),colnames(df_filtrato))]
df_finale <- rbind(df_finale, df_stab_mancanti[, intersect(colnames(df_stab_mancanti),colnames(df_filtrato))])

# controllo se ci sono buchi nei dati
if(sum(!complete.cases(df_finale)) > 0){
	print(df_finale[!complete.cases(df_finale), ])
	stop("Attenzione: ci sono righe con valori mancanti in df_finale")
	print(df_finale[!complete.cases(df_finale), ])
}

#controllo se ci sono comuni senza codici (chiave comune PRO_COM_T)
comuni_df_finale <- unique(df_finale$PRO_COM_T)
comuni_senza_allevamenti <- comuni[!(comuni$PRO_COM_T %in% comuni_df_finale), ]
# genero i dati (senza asl)
comuni_senza_allevamenti <- st_drop_geometry(comuni_senza_allevamenti)
# tutti i comuni_senza_allevamenti
comuni_senza_allevamenti <- merge(comuni_senza_allevamenti, 
																	chiave_codici_stabilimento,
																	by.x = "PRO_COM_T",
																	by.y = "PRO_COM_T_DT_FI",
																	all.x = T,
																	all.y = F
																	)
# aggiungo campo asl vuoto non avendolo
comuni_senza_allevamenti$azienda_asl <- as.character(NA)
# rinomino il nome campo di comuni_senza_allevamenti da "COD_STABILIMENTO" a "cod_stab"
colnames(comuni_senza_allevamenti)[colnames(comuni_senza_allevamenti) == "COD_STABILIMENTO"] <- "cod_stab"
campi_comuni <- intersect(colnames(df_finale),colnames(comuni_senza_allevamenti))
# unisco solo con i 3 campi in comune
df_completo <- rbind(df_finale[, campi_comuni],
										 comuni_senza_allevamenti[, campi_comuni]
										 )

# controllo che non ci siano codici senza comune (esclusa la colona 2, asl)
sum(!complete.cases(df_completo[, c(1, 3)]))
# controllo che non ci siano comuni senza codice
sum(!(comuni$PRO_COM_T %in% df_completo$PRO_COM_T))

# 
# # collego dati ASL e dati comuni (province, regioni ecc)
# dput(colnames(comuni))

# ESPORTAZIONE -----

df_completo <- merge(df_completo[, c("cod_stab", "PRO_COM_T")],
										 df_comuni,
										 by = "PRO_COM_T",
										 all.x = T,
										 all.y = F
)
# merge con chiave_regioni
df_completo <- merge(df_completo,
												 chiave_regioni,
												 by = "COD_REG",
												 all.x = T,
												 all.y = F
)
# riordino campi
df_completo <- df_completo[, c("cod_stab", "COD_RIP", "COD_REG", "DEN_REG", "COD_PROV","COD_UTS", "PRO_COM_T", "COMUNE")]

# solo per aggiornamento file intra anno 2025 nella regione sardegna (da aggiornare se ci saranno nuovi cambiamenti nello shapefile dei comuni da Istat)

# df_completo <- df_prefissi_stab
df_completo <- merge(df_completo, 
										 df_trans_comuni_2025[, c("PRO_COM_T", "PRO_COM_T_REL", "COD_UTS_REL")],
										 by = "PRO_COM_T",
										 all.x = T,
										 all.y = F
										 )
df_completo$PRO_COM_T <- ifelse(!is.na(df_completo$PRO_COM_T_REL),
																df_completo$PRO_COM_T_REL,
																df_completo$PRO_COM_T
																)
df_completo$COD_UTS <- ifelse(!is.na(df_completo$COD_UTS_REL),
															df_completo$COD_UTS_REL,
															df_completo$COD_UTS
															)
df_completo <- df_completo[, c("cod_stab", "COD_RIP", "COD_REG", "DEN_REG", "COD_PROV","COD_UTS", "PRO_COM_T", "COMUNE")]


write.csv(df_completo,
					file = "data_static/geo/df_prefissi_stab.csv",
					row.names = FALSE,
					fileEncoding = "UTF-8"
					)
write.csv(chiave_regioni,
					file = "data_static/geo/df_regioni.csv",
					row.names = FALSE,
					fileEncoding = "UTF-8"
)
write.csv(df_prov_val,
					file = "data_static/geo/df_province.csv",
					row.names = FALSE,
					fileEncoding = "UTF-8"
)
write.csv(df_comuni_now,
					file = "data_static/geo/df_comuni.csv",
					row.names = FALSE,
					fileEncoding = "UTF-8"
)
