source("R/data_dictionary.R")

STATIC_SPECIE <- read.csv(
  file = "data_static/decodifiche/specie.csv",
  stringsAsFactors = FALSE
)

# importazione motivi di ingresso
STATIC_MOTIVI_INGRESSO <- read.csv(
  file = "data_static/decodifiche/motivi_ingresso.csv",
  stringsAsFactors = FALSE
)

# importazione province italiane
STATIC_PROVINCE <- read.csv(
	  file = "data_static/geo/df_province.csv",
	  stringsAsFactors = FALSE
)

# importazione prefissi stabilimenti
STATIC_CODICI_STABILIMENTO <- read.csv(
  file = "data_static/df_prefissi_stab.csv",
  stringsAsFactors = FALSE
)