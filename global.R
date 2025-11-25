# =============================================================================
# GLOBAL.R - CONFIGURAZIONE GLOBALE E CARICAMENTO DATI STATICI
# =============================================================================
# Questo file viene eseguito una sola volta all'avvio dell'applicazione.
# Carica tutti i dati statici di riferimento e le funzioni utility necessarie.
#
# CONTENUTI:
# 1. Dizionario dati (colonne standard movimentazioni)
# 2. Tabelle decodifica (specie, motivi ingresso)
# 3. Tabelle geografiche (comuni, province, regioni, stabilimenti)
# 4. Funzioni utility per la pipeline
# =============================================================================

# Carica il dizionario dati con definizioni colonne per ogni gruppo specie
source("R/data_dictionary.R")

# =============================================================================
# TABELLE DECODIFICA
# =============================================================================

# Tabella specie animali (gruppi: bovini, ovicaprini, etc.)
STATIC_SPECIE <- read.csv(
  file = "data_static/decodifiche/specie.csv",
  stringsAsFactors = FALSE
)
gruppi_specie <- unique(STATIC_SPECIE$GRUPPO)

# Tabella motivi di ingresso con flag provenienza Italia
# Usata per classificare animali come Italia/Estero
# Colonne: Codice, Descrizione, prov_italia (TRUE/FALSE)
STATIC_MOTIVI_INGRESSO <- read.csv(
  file = "data_static/decodifiche/motivi_ingresso.csv",
  stringsAsFactors = FALSE
)

# =============================================================================
# TABELLE GEOGRAFICHE
# =============================================================================
# Tutte le tabelle geografiche sono caricate come character per evitare
# problemi con codici che iniziano con 0 (es. 001, 002)

# Prefissi codice stabilimento → comune ISTAT
# Usata per mappare codice stabilimento a PRO_COM_T
# Formato cod_stab: XXXsigla (es. 001TO)
df_stab <- read.csv(
  "data_static/geo/df_prefissi_stab.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

# Tabella stati esteri (codici ISO 3166)
df_stati <- read.csv(
  "data_static/geo/df_stati_iso3166.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  fileEncoding = "UTF-8"
)

# Tabella regioni italiane
df_regioni <- read.csv(
  "data_static/geo/df_regioni.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

# Tabella province italiane
# Contiene mapping COD_PROV_STORICO → COD_UTS per conversione
# marchio auricolare → codice provincia attuale
df_province <- read.csv(
  "data_static/geo/df_province.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

# Tabella comuni italiani con codice ISTAT
df_comuni <- read.csv(
  "data_static/geo/df_comuni.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  fileEncoding = "UTF-8"
)

# =============================================================================
# MODULI E FUNZIONI UTILITY
# =============================================================================

# Funzioni utility per la pipeline di elaborazione
# (classificazione origine, estrazione codici, merge malattie)
source("R/utils_pipeline.R")

# Modulo pipeline controlli movimentazioni
source("R/mod_pipeline_controlli.R")