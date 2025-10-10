# genera template province attuali per aggiornamento indennità riprendendo le indennità vecchie quando attivabili

# pacchetti
library(utils)
library(httr2)
library(jsonlite)
# library(dplyr)
# library(stringr)
library(tidyr)
# library(purrr)
# library(tidyselect)
# library(lubridate)
library(readxl)
library(openxlsx)

data_da = "01/01/1991"
data_a  = format(Sys.Date(), "%d/%m/%Y")   # metti la data odierna

# endpoint
url_64 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
									"pfun=64&pdata=", data_a)

df_prov_att  <- fromJSON(url_64, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (attuali)

files <- dir(file.path("data_static", "malattie"), pattern = "\\.xlsx$", full.names = TRUE)

choice <- menu(files, title = "Seleziona un file")

# Controlla che la scelta sia valida
if (choice == 0) stop("Nessuna selezione effettuata.")

indenni <- read_excel(files[choice])


# i campi indenne devono sempre iniziare per 'Ind_'
colonne_indenni <- grep("^Ind_", colnames(indenni), value = TRUE)
# campo chiave da ritenere: "COD_PROV_STORICO"

indenni <- merge(df_prov_att, 
								 indenni[, c("COD_PROV_STORICO", 
								 						colonne_indenni
								 						)], 
								 by= "COD_PROV_STORICO", 
								 all.x = T)

# editare il file aggiornato alle province nuove e esportarlo nuovamente  
# aggiungere un foglio con nella cella A1 la versione del REGOLAMENTO DI ESECUZIONE (UE) 2021/620 DELLA COMMISSIONE
# https://eur-lex.europa.eu/eli/reg_impl/2021/620

# genero dataframe metadati con campi: colonna, malattia, specie, riferimento, data_inizio, data_fine
metadata <- data.frame(campo = colonne_indenni, 
											 malattia = as.character(rep(NA, length(colonne_indenni))),
											 specie = as.character(rep(NA, length(colonne_indenni))),
											 riferimento = as.character(rep(NA, length(colonne_indenni))),
											 data_inizio = as.Date(rep(NA, length(colonne_indenni))),
											 data_fine = as.Date(rep(NA, length(colonne_indenni))),
											 stringsAsFactors = FALSE)

# se non esiste la cartella templates la crea, la cartella templates è ignorata da git

if(!exists("data_static/malattie_template")){
	dir.create("data_static/malattie_template")
}

write.xlsx(list(province = indenni,
								metadati = metadata), 
					 file = file.path("data_static", "malattie_template",
					 								 basename(files[choice])),
						overwrite = TRUE)
