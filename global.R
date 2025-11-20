source("R/data_dictionary.R")

STATIC_SPECIE <- read.csv(
  file = "data_static/decodifiche/specie.csv",
  stringsAsFactors = FALSE
)

gruppi_specie <- unique(STATIC_SPECIE$GRUPPO)

# importazione motivi di ingresso
STATIC_MOTIVI_INGRESSO <- read.csv(
  file = "data_static/decodifiche/motivi_ingresso.csv",
  stringsAsFactors = FALSE
)

# importazione dati statici -----------------------
## tabelle geografiche ------
# "df_comuni.csv" "df_prefissi_stab.csv" "df_province.csv" "df_regioni.csv" "df_stati_iso3166.csv" (UTF8)
# prefissi codice di stabilimento
df_stab          <- read.csv(
  "data_static/geo/df_prefissi_stab.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)
# tabella stati esteri
df_stati        <- read.csv(
  "data_static/geo/df_stati_iso3166.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  fileEncoding = "UTF-8"
)
# tabella regioni
df_regioni      <- read.csv(
  "data_static/geo/df_regioni.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)
# tabella province
df_province     <- read.csv(
  "data_static/geo/df_province.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)
# tabella comuni
df_comuni       <- read.csv(
  "data_static/geo/df_comuni.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  fileEncoding = "UTF-8"
)

# Load utility functions and modules
source("R/utils_pipeline.R")
source("R/mod_pipeline_controlli.R")