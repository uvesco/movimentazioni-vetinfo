# =============================================================================
# FUNZIONI UTILITY PER LA PIPELINE DI ELABORAZIONE MOVIMENTAZIONI
# =============================================================================
# Questo file contiene le funzioni utility usate dalla pipeline di controllo
# delle movimentazioni animali. 
#
# NOTA: A partire dalla refactoring per migliorare la leggibilità, alcune 
# funzioni non sono più utilizzate nella pipeline principale (mod_pipeline_controlli.R)
# che ora esegue i merge direttamente in modo lineare.
#
# FUNZIONI ANCORA IN USO:
# - normalize_stab_code: Normalizzazione prefissi codici stabilimento
# - estrai_provincia_nascita: Estrazione provincia da marchio auricolare
# - crea_dataframe_validazione: Creazione dataframe per validazione
# - filtra_animali_non_indenni: Filtro animali da zone non indenni
#
# FUNZIONI DEPRECATE (ora integrate nella pipeline):
# - classifica_origine: Logica ora inline nella pipeline
# - estrai_comune_provenienza: Merge ora eseguito direttamente nella pipeline
# - merge_malattie_con_prefisso: Merge ora eseguito direttamente nella pipeline
# =============================================================================

# =============================================================================
# FUNZIONE 0: NORMALIZZA CODICE STABILIMENTO
# =============================================================================
# Normalizza i codici stabilimento al prefisso a 5 caratteri (es. 008BZ210 → 008BZ).
# PARAMETRI:
# - x: vettore di codici stabilimento (character/factor) con eventuali suffissi.
# RITORNA:
# - vettore character con prefisso a 5 caratteri, oppure NA se mancante.
# NOTA:
# - I prefissi in df_prefissi_stab.csv sono a 5 caratteri (3 cifre + sigla provincia).
# - Se il codice ha meno di 5 caratteri, viene restituito così com'è dopo normalizzazione.
# - I valori non character vengono convertiti e le stringhe vuote diventano NA.
normalize_stab_code <- function(x) {
	normalized <- toupper(trimws(as.character(x)))
	normalized[normalized == ""] <- NA_character_
	prefix <- substr(normalized, 1, 5)
	short_idx <- !is.na(normalized) & nchar(normalized) < 5
	prefix[short_idx] <- normalized[short_idx]
	prefix
}

# =============================================================================
# FUNZIONE 0B: PARSE DATA INGRESSO
# =============================================================================
# Converte valori data (Date o numerici Excel) in Date.
# Excel memorizza le date come giorni trascorsi dal 1899-12-30 (epoca Windows).
parse_ingresso_date <- function(values) {
	excel_epoch <- as.Date("1899-12-30")
	if (inherits(values, "Date")) {
		return(values)
	}
	parsed <- suppressWarnings(as.Date(values))
	numeric_idx <- is.na(parsed) & is.numeric(values)
	if (any(numeric_idx)) {
		parsed[numeric_idx] <- suppressWarnings(as.Date(values[numeric_idx], origin = excel_epoch))
	}
	parsed
}

# =============================================================================
# FUNZIONE 1 [DEPRECATA]: CLASSIFICA ORIGINE
# =============================================================================
# NOTA: Questa funzione non è più utilizzata nella pipeline principale.
# La logica è stata integrata direttamente in mod_pipeline_controlli.R per
# migliorare la leggibilità del codice.
#
# Classifica ogni animale come proveniente da "italia" o "estero"
# 
# LOGICA DI CLASSIFICAZIONE (in ordine di priorità):
# 1. Se orig_stabilimento_cod non è nullo → indicatore provenienza italiana
# 2. Se il motivo ingresso indica provenienza italiana (prov_italia=TRUE) → italia
# 3. Se il motivo ingresso indica provenienza estera (prov_italia=FALSE) → estero
# 4. Altrimenti → NA (ignoto)
#
# PARAMETRI:
# - df: dataframe con colonne 'orig_stabilimento_cod' e 'ingresso_motivo'
# - motivi_ingresso_table: tabella decodifica con colonne 'Descrizione' e 'prov_italia'
#
# RITORNA:
# - df con nuove colonne 'orig_italia' e 'orig_italia_motivo'
# =============================================================================
classifica_origine <- function(df, motivi_ingresso_table) {
	# # Prima verifica: il marchio auricolare inizia con IT?
	# # Questo è l'indicatore più affidabile per animali italiani
	# ear_tag <- as.character(df$capo_identificativo)
	# is_italian_ear_tag <- grepl("^IT", ear_tag, ignore.case = TRUE)
	
	
	# prima verifica: orig_stabilimento_cod non è nullo
	
	is_italian_establishment <- !is.na(df$orig_stabilimento_cod)
	
	# Seconda verifica: lookup nella tabella motivi ingresso
	# Merge per ottenere il flag prov_italia dal codice motivo
	normalize_string <- function(x) {
		x |>
			tolower() |>                 # tutto minuscolo
			gsub("\\s+", "", x = _) |>   # elimina TUTTI gli spazi
			trimws()                     # rimuove spazi iniziali/finali se restano
	}
	
	
	# Creo copie normalizzate per il match su codice o descrizione
	df$ingresso_motivo_norm <- normalize_string(df$ingresso_motivo)
	motivi_ingresso_table$prov_italia <- as.logical(motivi_ingresso_table$prov_italia)
	motivi_ingresso_table$Codice_norm <- normalize_string(motivi_ingresso_table$Codice)
	motivi_ingresso_table$Descrizione_norm <- normalize_string(motivi_ingresso_table$Descrizione)
	
	lookup_cod <- setNames(motivi_ingresso_table$prov_italia, motivi_ingresso_table$Codice_norm)
	lookup_desc <- setNames(motivi_ingresso_table$prov_italia, motivi_ingresso_table$Descrizione_norm)
	
	df$orig_italia_motivo <- lookup_cod[df$ingresso_motivo_norm]
	missing_idx <- is.na(df$orig_italia_motivo)
	df$orig_italia_motivo[missing_idx] <- lookup_desc[df$ingresso_motivo_norm][missing_idx]
	
	missing_idx <- is.na(df$orig_italia_motivo)
	df$orig_italia_motivo[missing_idx] <- is_italian_establishment[missing_idx]
	
	# Elimino modifica temporanea
	df$ingresso_motivo_norm <- NULL
	
	df$orig_italia <- df$orig_italia_motivo
	

	
	return(df)
}

# =============================================================================
# FUNZIONE 2: ESTRAI PROVINCIA NASCITA
# =============================================================================
# Estrae il codice provincia di nascita dal marchio auricolare italiano.
# 
# FORMATO MARCHIO AURICOLARE ITALIANO:
# - Struttura: IT<3 cifre provincia><altri caratteri>
# - Esempio: IT001234567890 → provincia 001 (Torino storico)
# 
# MAPPING PROVINCE:
# Il marchio contiene COD_PROV_STORICO (codice storico 001-110)
# che deve essere mappato a COD_UTS (codice attuale, es. 201 per Torino metro)
# tramite la tabella df_province
#
# PARAMETRI:
# - capo_identificativo: vettore di marchi auricolari
# - df_province_table: tabella province con COD_PROV_STORICO e COD_UTS (opzionale)
#
# RITORNA:
# - vettore di COD_UTS se df_province_table fornita
# - vettore di COD_PROV_STORICO (raw) se df_province_table NULL
# =============================================================================
estrai_provincia_nascita <- function(capo_identificativo, df_province_table = NULL) {
	# Converte a carattere per operazioni su stringhe
	ear_tag <- as.character(capo_identificativo)
	
	# Verifica se inizia con IT
	is_italian <- grepl("^IT", ear_tag, ignore.case = TRUE)
	
	# Estrae le 3 cifre dopo IT (posizioni 3-5)
	# Questo è il COD_PROV_STORICO (codice provincia storico)
	cod_prov_storico <- ifelse(
		is_italian,
		substr(ear_tag, 3, 5),
		NA_character_
	)
	
	# Se abbiamo la tabella province, mappa COD_PROV_STORICO → COD_UTS

		# Crea lookup table (rimuove duplicati per efficienza)
		lookup <- df_province_table[, c("COD_PROV_STORICO", "COD_UTS")]
		# rimuove i duplicati completi
		loookup <- lookup[!duplicated(lookup[, c("COD_PROV_STORICO", "COD_UTS")]), ]
		# in caso di ulteriore presenza di cod_prov_storico duplicati, impongo COD_UTS = NA in modo da forzare un controllo manuale
		dup_prov <- unique(lookup$COD_PROV_STORICO[duplicated(lookup$COD_PROV_STORICO)])
		lookup$COD_UTS[lookup$COD_PROV_STORICO %in% dup_prov] <- NA_character_
		# elimino nuovamente i duplicati
		lookup <- lookup[!duplicated(lookup$COD_PROV_STORICO), ]
		
		# Usa match() per preservare l'ordine originale delle righe
		match_idx <- match(cod_prov_storico, lookup$COD_PROV_STORICO)
		cod_uts <- lookup$COD_UTS[match_idx]
		
		return(cod_uts)

}

# =============================================================================
# FUNZIONE 3 [DEPRECATA]: ESTRAI COMUNE PROVENIENZA
# =============================================================================
# NOTA: Questa funzione non è più utilizzata nella pipeline principale.
# Il merge è ora eseguito direttamente in mod_pipeline_controlli.R per
# migliorare la leggibilità del codice.
#
# Estrae il codice ISTAT del comune di provenienza dal codice stabilimento.
#
# FORMATO CODICE STABILIMENTO:
# - Struttura: <3+ cifre><2 lettere sigla provincia>
# - Esempio: 001TO → comune nella provincia di Torino
# - La mappatura avviene tramite df_stab (tabella prefissi stabilimento)
#
# PARAMETRI:
# - orig_stabilimento_cod: vettore di codici stabilimento origine
# - df_stab_table: tabella con cod_stab e PRO_COM_T
#
# RITORNA:
# - vettore di PRO_COM_T (codici ISTAT comune a 6 cifre)
# =============================================================================
estrai_comune_provenienza <- function(orig_stabilimento_cod, df_stab_table) {
	# Crea dataframe temporaneo per il merge
	result <- data.frame(
		orig_stabilimento_cod = normalize_stab_code(orig_stabilimento_cod),
		stringsAsFactors = FALSE
	)
	
	# Merge con tabella stabilimenti per ottenere PRO_COM_T
	result <- merge(
		result,
		df_stab_table[, c("cod_stab", "PRO_COM_T")],
		by.x = "orig_stabilimento_cod",
		by.y = "cod_stab",
		all.x = TRUE  # Mantiene tutti i record, anche senza match
	)
	
	# Ritorna solo la colonna PRO_COM_T #2do verificare che non servano le province
	return(result$PRO_COM_T)
}

# =============================================================================
# FUNZIONE 4 [DEPRECATA]: MERGE MALATTIE CON PREFISSO
# =============================================================================
# NOTA: Questa funzione non è più utilizzata nella pipeline principale.
# I merge con dati malattie sono ora eseguiti direttamente in mod_pipeline_controlli.R
# per migliorare la leggibilità del codice.
#
# Esegue il merge tra dataframe animali e dati malattie, aggiungendo un prefisso
# alle colonne delle malattie per distinguere provenienza da nascita.
#
# PREFISSI USATI:
# - "prov_" per status sanitario del comune di provenienza
# - "nascita_" per status sanitario della provincia di nascita
#
# COLONNE ESCLUSE DAL PREFISSO:
# Le colonne geografiche (COD_REG, COD_UTS, PRO_COM_T) non vengono rinominate
#
# PARAMETRI:
# - df_animali: dataframe animali con colonna chiave
# - df_malattie: dataframe malattie con colonne boolean per ogni malattia
# - by_animali: nome colonna chiave in df_animali
# - by_malattie: nome colonna chiave in df_malattie
# - prefisso: prefisso da aggiungere (es. "prov_" o "nascita_")
#
# RITORNA:
# - df_animali arricchito con colonne malattie prefissate
# =============================================================================
merge_malattie_con_prefisso <- function(df_animali, df_malattie, by_animali, by_malattie, prefisso) {
	# Colonne geografiche da non rinominare
	geo_cols <- c("COD_REG", "COD_UTS", "PRO_COM_T")
	
	# Identifica le colonne malattie da rinominare
	# (esclude chiavi join e colonne geografiche)
	all_cols <- names(df_malattie)
	exclude_cols <- c(by_malattie, geo_cols)
	disease_cols <- setdiff(all_cols, exclude_cols)
	
	# Esegue il merge (left join per mantenere tutti gli animali)
	result <- merge(
		df_animali,
		df_malattie,
		by.x = by_animali,
		by.y = by_malattie,
		all.x = TRUE,
		suffixes = c("", ".y")  # Suffisso per colonne duplicate
	)
	
	# Rimuove eventuali colonne geografiche duplicate (con suffisso .y)
	duplicate_geo_cols <- paste0(geo_cols, ".y")
	result <- result[, !(names(result) %in% duplicate_geo_cols), drop = FALSE]
	
	# Rinomina le colonne malattie con il prefisso specificato
	for (col in disease_cols) {
		old_name <- col
		new_name <- paste0(prefisso, col)
		if (old_name %in% names(result)) {
			names(result)[names(result) == old_name] <- new_name
		}
	}
	
	return(result)
}

# =============================================================================
# FUNZIONE 5: CREA DATAFRAME VALIDAZIONE
# =============================================================================
# Crea un dataframe contenente gli animali italiani con dati geografici non validi.
# Usata per identificare animali che richiedono controllo manuale.
#
# CRITERI DI FILTRO:
# - orig_italia == TRUE (solo animali italiani)
# - campo_geografico è NA (codice non trovato/mappato)
#
# PARAMETRI:
# - df_animali: dataframe animali con colonna Italia (default orig_italia)
# - campo_geografico: nome della colonna da verificare (es. "orig_comune_cod")
# - tipo_validazione: etichetta per il tipo di errore
# - colonna_italia: nome colonna booleana per filtro Italia (default "orig_italia")
#
# RITORNA:
# - dataframe con animali non validi e colonna tipo_errore
# =============================================================================
crea_dataframe_validazione <- function(df_animali, campo_geografico, tipo_validazione, colonna_italia = "orig_italia") {
	# Filtra: animali italiani con campo geografico NA
	is_italia <- df_animali[[colonna_italia]]
	df_invalid <- df_animali[
		is.na(df_animali[[campo_geografico]]) & 
		is_italia == TRUE & !is.na(is_italia),
	]
	
	# Aggiunge colonna descrittiva del tipo di errore
	df_invalid$tipo_errore <- rep(tipo_validazione, nrow(df_invalid))
	
	return(df_invalid)
}

# =============================================================================
# FUNZIONE 6: FILTRA ANIMALI NON INDENNI
# =============================================================================
# Filtra gli animali provenienti/nati in zone non indenni per una specifica malattia.
#
# LOGICA BOOLEAN:
# - TRUE = zona indenne (disease-free) → animale OK
# - FALSE = zona non indenne → animale da segnalare
# - NA = dato mancante → non incluso nel filtro
#
# PARAMETRI:
# - df_animali: dataframe con colonne malattie (es. prov_Ind_MTBC)
# - campo_malattia: nome colonna boolean da filtrare (es. "prov_Ind_MTBC")
#
# RITORNA:
# - dataframe con solo animali da zone non indenni (campo_malattia == FALSE)
# =============================================================================
filtra_animali_non_indenni <- function(df_animali, campo_malattia) {
	# Verifica esistenza della colonna
	if (!campo_malattia %in% names(df_animali)) {
		return(data.frame())  # Ritorna vuoto se colonna non esiste
	}
	
	# Filtra: valore FALSE = zona non indenne
	df_non_indenni <- df_animali[
		!is.na(df_animali[[campo_malattia]]) & 
		df_animali[[campo_malattia]] == FALSE,
	]
	
	return(df_non_indenni)
}
