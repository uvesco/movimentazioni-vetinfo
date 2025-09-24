# genera template province attuali per aggiornamento indennità riprendendo le indennità vecchie quando attivabili

# pacchetti
library(httr2)
library(jsonlite)
# library(dplyr)
# library(stringr)
library(tidyr)
# library(purrr)
# library(tidyselect)
# library(lubridate)
library(readxl)
library(openxlsx2)

data_da = "01/01/1991"
data_a  = format(Sys.Date(), "%d/%m/%Y")   # metti la data odierna

# endpoint
url_64 <- paste0("https://situas-servizi.istat.it/publish/reportspooljson?",
									"pfun=64&pdata=", data_a)

df_prov_att  <- fromJSON(url_64, simplifyDataFrame = TRUE)$resultset    # province/UTS con SIGLE (attuali)

indenni <- read_excel("data_static/prov_indenni.xlsx")

indenni <- merge(df_prov_att, indenni[, c(1, 13:15)], by= "COD_PROV_STORICO", all.x = T)

# editare il file aggiornato alle province nuove e esportarlo nuovamente  
# aggiungere un foglio con nella cella A1 la versione del REGOLAMENTO DI ESECUZIONE (UE) 2021/620 DELLA COMMISSIONE
# https://eur-lex.europa.eu/eli/reg_impl/2021/620

write_xlsx(indenni, "data_static/prov_indenni.xlsx")

