# script per importare le ASL dalle tabelle della BDN (salvati con nome i files .htm delle finestre pop-up
# di selezione asl da interrogazione -> dati -> decodifiche

###### ATTENZIONE NON FUNZIONA 

library(rvest)
library(dplyr)

directory_files_asl <- "src/stabilimenti_BDN/asl"
lista_files <- dir(directory_files_asl)
# seleziono solo quelli con estensione .htm
lista_files <- lista_files[grepl("\\.htm$", lista_files)]

df_asl <- NULL

for(f in lista_files){
# percorso al file scaricato

doc <- read_html(file.path(directory_files_asl, f), encoding = "windows-1252")

# La tabella ha la classe CSS 'tabellaLOV' e intestazioni "Codice" e "Denominazione"
df <- doc %>%
	html_elements("table.tabellaLOV") %>%
	html_table(header = TRUE, fill = TRUE) %>%
	.[[1]] %>%
	mutate(across(everything(), ~ gsub("\u00A0", " ", .) |> trimws()))

df_asl <- rbind(df, df_asl)
}
rm(df)
# ordino per colonna "Codice"
df_asl <- df_asl[order(df_asl$Codice), ]

write.csv(df_asl, file = "data_static/ASL_Italia.csv", row.names = FALSE)
