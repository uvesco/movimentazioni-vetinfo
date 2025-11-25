# =============================================================================
# FUNZIONI UTILITY PER LA PIPELINE DI ELABORAZIONE MOVIMENTAZIONI
# =============================================================================
# Questo file contiene le funzioni utility usate dalla pipeline di controllo
# delle movimentazioni animali. Le funzioni gestiscono:
# - Classificazione origine Italia/Estero
# - Estrazione codici geografici (provincia nascita, comune provenienza)
# - Merge con dati malattie
# - Filtri per animali da zone non indenni
# =============================================================================

# =============================================================================
# FUNZIONE 1: CLASSIFICA ORIGINE
# =============================================================================
# Classifica ogni animale come proveniente da "italia" o "estero"
# 
# LOGICA DI CLASSIFICAZIONE (in ordine di priorità):
# 1. Se il marchio auricolare inizia con "IT" → italia
# 2. Se il motivo ingresso indica provenienza italiana (prov_italia=TRUE) → italia
# 3. Se il motivo ingresso indica provenienza estera (prov_italia=FALSE) → estero
# 4. Se il motivo ingresso è sconosciuto e non è IT → estero
#
# PARAMETRI:
# - df: dataframe con colonne 'capo_identificativo' e 'ingresso_motivo'
# - motivi_ingresso_table: tabella decodifica con colonne 'Codice' e 'prov_italia'
#
# RITORNA:
# - df con nuova colonna 'origine' contenente "italia" o "estero"
# =============================================================================
classifica_origine <- function(df, motivi_ingresso_table) {
	# Prima verifica: il marchio auricolare inizia con IT?
	# Questo è l'indicatore più affidabile per animali italiani
	ear_tag <- as.character(df$capo_identificativo)
	is_italian_ear_tag <- grepl("^IT", ear_tag, ignore.case = TRUE)
	
	# Seconda verifica: lookup nella tabella motivi ingresso
	# Merge per ottenere il flag prov_italia dal codice motivo
	df <- merge(
		df,
		motivi_ingresso_table[, c("Codice", "prov_italia")],
		by.x = "ingresso_motivo",
		by.y = "Codice",
		all.x = TRUE  # Mantiene tutti gli animali anche se motivo sconosciuto
	)
	
	# Crea il campo origine applicando la logica di priorità:
	# - IT nel marchio → sempre italia
	# - Altrimenti usa prov_italia dalla tabella decodifica
	# - Se NA (motivo sconosciuto) → estero per sicurezza
	df$origine <- ifelse(
		is_italian_ear_tag,
		"italia",
		ifelse(
			!is.na(df$prov_italia) & df$prov_italia == TRUE,
			"italia",
			"estero"
		)
	)
	
	# Rimuove la colonna temporanea prov_italia
	df$prov_italia <- NULL
	
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
	
	# Verifica se inizia con IT seguito da 3 cifre
	is_italian <- grepl("^IT[0-9]{3}", ear_tag, ignore.case = TRUE)
	
	# Estrae le 3 cifre dopo IT (posizioni 3-5)
	# Questo è il COD_PROV_STORICO (codice provincia storico)
	cod_prov_storico <- ifelse(
		is_italian,
		substr(ear_tag, 3, 5),
		NA_character_
	)
	
	# Se abbiamo la tabella province, mappa COD_PROV_STORICO → COD_UTS
	if (!is.null(df_province_table)) {
		# Crea lookup table (rimuove duplicati per efficienza)
		lookup <- df_province_table[, c("COD_PROV_STORICO", "COD_UTS")]
		lookup <- lookup[!duplicated(lookup$COD_PROV_STORICO), ]
		
		# Usa match() per preservare l'ordine originale delle righe
		match_idx <- match(cod_prov_storico, lookup$COD_PROV_STORICO)
		cod_uts <- lookup$COD_UTS[match_idx]
		
		return(cod_uts)
	} else {
		# Senza tabella, ritorna il codice raw (backward compatibility)
		return(cod_prov_storico)
	}
}

# =============================================================================
# FUNZIONE 3: ESTRAI COMUNE PROVENIENZA
# =============================================================================
# Estrae il codice ISTAT del comune di provenienza dal codice stabilimento.
#
# FORMATO CODICE STABILIMENTO:
# - Struttura: <3+ cifre><2 lettere sigla provincia>
# - Esempio: 001TO → comune nel comune di Torino
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
		orig_stabilimento_cod = orig_stabilimento_cod,
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
	
	# Ritorna solo la colonna PRO_COM_T
	return(result$PRO_COM_T)
}

# =============================================================================
# FUNZIONE 4: MERGE MALATTIE CON PREFISSO
# =============================================================================
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
# - origine == "italia" (solo animali italiani)
# - campo_geografico è NA (codice non trovato/mappato)
#
# PARAMETRI:
# - df_animali: dataframe animali con colonna 'origine'
# - campo_geografico: nome della colonna da verificare (es. "PRO_COM_T_prov")
# - tipo_validazione: etichetta per il tipo di errore
#
# RITORNA:
# - dataframe con animali non validi e colonna tipo_errore
# =============================================================================
crea_dataframe_validazione <- function(df_animali, campo_geografico, tipo_validazione) {
	# Filtra: animali italiani con campo geografico NA
	df_invalid <- df_animali[
		is.na(df_animali[[campo_geografico]]) & 
		df_animali$origine == "italia",
	]
	
	# Aggiunge colonna descrittiva del tipo di errore
	df_invalid$tipo_errore <- tipo_validazione
	
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
