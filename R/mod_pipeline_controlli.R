# =============================================================================
# MODULO PIPELINE CONTROLLI MOVIMENTAZIONI
# =============================================================================
# Questo modulo Shiny esegue l'intera pipeline di elaborazione e controllo
# delle movimentazioni animali. 
#
# FLUSSO ELABORAZIONE:
# 1. Classificazione origine (Italia/Estero) per ogni animale
# 2. Estrazione provincia di nascita dal marchio auricolare
# 3. Estrazione comune di provenienza dal codice stabilimento
# 4. Merge con dati malattie per provenienza (prefisso prov_)
# 5. Merge con dati malattie per nascita (prefisso nascita_)
# 6. Identificazione animali con dati non validi
# 7. Identificazione animali da zone non indenni
#
# OUTPUT REATTIVI:
# - dati_processati: dataset completo con tutti i merge
# - casi_provenienza_non_trovati: IDs animali con comune non valido
# - df_provenienza_non_trovati: dataframe completo animali senza comune
# - casi_nascita_non_trovati: IDs animali con provincia non valida
# - df_nascita_non_trovati: dataframe completo animali senza provincia
# - animali_provenienza_non_indenni: lista di dataframe per malattia (provenienza)
# - animali_nascita_non_indenni: lista di dataframe per malattia (nascita)
# =============================================================================

mod_pipeline_controlli_server <- function(id, animali, gruppo, malattie_data) {
	moduleServer(id, function(input, output, session) {
		
		# =====================================================================
		# PIPELINE PRINCIPALE: ELABORAZIONE DATI
		# =====================================================================
		# Questo reactive esegue tutti i passaggi di elaborazione e ritorna
		# il dataframe finale con tutti i dati animali e status malattie
		
		dati_processati <- reactive({
			# Richiede che i dati di input siano disponibili
			req(animali())
			req(gruppo())
			req(malattie_data())
			
			# Estrae i dati reattivi
			df <- animali()
			grp <- gruppo()
			malattie <- malattie_data()
			
			# Verifica disponibilità dati malattie per questo gruppo
			if (is.null(malattie[[grp]])) {
				return(NULL)
			}
			
			# -----------------------------------------------------------------
			# STEP 1: Classificazione Italia/Estero
			# -----------------------------------------------------------------
			# Verifica se il codice stabilimento origine non è nullo
			# (indicatore di provenienza italiana)
			is_italian_establishment <- !is.na(df$orig_stabilimento_cod)
			
			# Normalizza stringhe per merge robusto (rimuove spazi, minuscolo)
			normalize_string <- function(x) {
				x <- tolower(x)
				x <- gsub("\\s+", "", x)
				x <- trimws(x)
				x
			}
			
			# Prepara colonne normalizzate per il merge
			df$ingresso_motivo_norm <- normalize_string(df$ingresso_motivo)
			motivi_norm <- STATIC_MOTIVI_INGRESSO
			motivi_norm$Descrizione_norm <- normalize_string(motivi_norm$Descrizione)
			
			# Merge con tabella motivi ingresso per ottenere flag prov_italia
			df <- merge(
				df,
				motivi_norm[, c("Descrizione_norm", "prov_italia")],
				by.x = "ingresso_motivo_norm",
				by.y = "Descrizione_norm",
				all.x = TRUE,
				all.y = FALSE
			)
			
			# Rinomina colonna per chiarezza
			names(df)[names(df) == "prov_italia"] <- "prov_italia_motivo"
			
			# Rimuove colonna temporanea
			df$ingresso_motivo_norm <- NULL
			
			# Calcola provenienza italia con logica AND stretta:
			# - Se entrambi TRUE → TRUE (italia)
			# - Se entrambi FALSE → FALSE (estero)
			# - Altrimenti → NA (ignoto)
			and_strict <- function(A, B) {
				if (isTRUE(A) && isTRUE(B)) {
					TRUE
				} else if (isFALSE(A) && isFALSE(B)) {
					FALSE
				} else {
					NA
				}
			}
			
			df$orig_italia <- mapply(
				and_strict,
				is_italian_establishment,
				df$prov_italia_motivo
			)
			
			# -----------------------------------------------------------------
			# STEP 2: Estrazione provincia di nascita
			# -----------------------------------------------------------------
			# Estrae COD_UTS dal marchio auricolare usando mapping storico
			df$cod_uts_nascita <- estrai_provincia_nascita(df$capo_identificativo, df_province)
			
			# -----------------------------------------------------------------
			# STEP 3: Estrazione comune di provenienza
			# -----------------------------------------------------------------
			# Merge diretto con tabella stabilimenti per ottenere PRO_COM_T
			# Questo collega i codici allevamento di provenienza con il comune
			df <- merge(
				df,
				df_stab[, c("cod_stab", "PRO_COM_T")],
				by.x = "orig_stabilimento_cod",
				by.y = "cod_stab",
				all.x = TRUE
			)
			
			# Rinomina per distinguere da altre colonne PRO_COM_T
			names(df)[names(df) == "PRO_COM_T"] <- "PRO_COM_T_prov"
			
			# -----------------------------------------------------------------
			# STEP 4: Merge malattie sulla PROVENIENZA (comune ISTAT)
			# -----------------------------------------------------------------
			# Merge diretto con dati malattie dei comuni
			df_comuni_malattie <- malattie[[grp]][["comuni"]]
			if (!is.null(df_comuni_malattie) && nrow(df_comuni_malattie) > 0) {
				# Identifica colonne geografiche da non rinominare
				geo_cols <- c("COD_REG", "COD_UTS", "PRO_COM_T")
				
				# Colonne malattie da prefissare (escluse chiavi e geo)
				disease_cols <- setdiff(
					names(df_comuni_malattie),
					c("PRO_COM_T", geo_cols)
				)
				
				# Esegue merge
				df <- merge(
					df,
					df_comuni_malattie,
					by.x = "PRO_COM_T_prov",
					by.y = "PRO_COM_T",
					all.x = TRUE,
					suffixes = c("", ".y")
				)
				
				# Rimuove colonne geografiche duplicate
				duplicate_geo_cols <- paste0(geo_cols, ".y")
				df <- df[, !(names(df) %in% duplicate_geo_cols), drop = FALSE]
				
				# Aggiunge prefisso "prov_" alle colonne malattie
				for (col in disease_cols) {
					if (col %in% names(df)) {
						names(df)[names(df) == col] <- paste0("prov_", col)
					}
				}
			}
			
			# -----------------------------------------------------------------
			# STEP 5: Merge malattie sulla NASCITA (codice UTS provincia)
			# -----------------------------------------------------------------
			# Merge diretto con dati malattie delle province
			df_province_malattie <- malattie[[grp]][["province"]]
			if (!is.null(df_province_malattie) && nrow(df_province_malattie) > 0) {
				# Identifica colonne geografiche da non rinominare
				geo_cols <- c("COD_REG", "COD_UTS", "PRO_COM_T")
				
				# Colonne malattie da prefissare (escluse chiavi e geo)
				disease_cols <- setdiff(
					names(df_province_malattie),
					c("COD_UTS", geo_cols)
				)
				
				# Esegue merge
				df <- merge(
					df,
					df_province_malattie,
					by.x = "cod_uts_nascita",
					by.y = "COD_UTS",
					all.x = TRUE,
					suffixes = c("", ".y")
				)
				
				# Rimuove colonne geografiche duplicate
				duplicate_geo_cols <- paste0(geo_cols, ".y")
				df <- df[, !(names(df) %in% duplicate_geo_cols), drop = FALSE]
				
				# Aggiunge prefisso "nascita_" alle colonne malattie
				for (col in disease_cols) {
					if (col %in% names(df)) {
						names(df)[names(df) == col] <- paste0("nascita_", col)
					}
				}
			}
			
			return(df)
		})
		
		# =====================================================================
		# VALIDAZIONE: COMUNE PROVENIENZA NON VALIDO
		# =====================================================================
		# Identifica animali italiani con codice stabilimento non mappabile
		
		# Vettore dei soli identificativi (per conteggio rapido)
		casi_provenienza_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Filtra: italiani (orig_italia == TRUE) con PRO_COM_T_prov = NA
			animali_invalid <- df[
				is.na(df$PRO_COM_T_prov) & df$orig_italia == TRUE & !is.na(df$orig_italia),
				"capo_identificativo"
			]
			
			return(animali_invalid)
		})
		
		# Dataframe completo per visualizzazione e download
		df_provenienza_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Crea dataframe con tipo errore
			df_invalid <- crea_dataframe_validazione(
				df,
				campo_geografico = "PRO_COM_T_prov",
				tipo_validazione = "comune_provenienza_non_valido"
			)
			
			return(df_invalid)
		})
		
		# =====================================================================
		# VALIDAZIONE: PROVINCIA NASCITA NON VALIDA
		# =====================================================================
		# Identifica animali italiani con marchio auricolare non mappabile
		
		# Vettore dei soli identificativi
		casi_nascita_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Filtra: italiani (orig_italia == TRUE) con cod_uts_nascita = NA
			animali_invalid <- df[
				is.na(df$cod_uts_nascita) & df$orig_italia == TRUE & !is.na(df$orig_italia),
				"capo_identificativo"
			]
			
			return(animali_invalid)
		})
		
		# Dataframe completo per visualizzazione e download
		df_nascita_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			df_invalid <- crea_dataframe_validazione(
				df,
				campo_geografico = "cod_uts_nascita",
				tipo_validazione = "provincia_nascita_non_valida"
			)
			
			return(df_invalid)
		})
		
		# =====================================================================
		# FILTRO: ANIMALI DA ZONE NON INDENNI PER PROVENIENZA
		# =====================================================================
		# Per ogni malattia, crea un dataframe con animali da comuni non indenni
		# Output: lista named con chiave = nome malattia, valore = dataframe
		
		animali_provenienza_non_indenni <- reactive({
			req(dati_processati())
			req(malattie_data())
			req(gruppo())
			
			df <- dati_processati()
			grp <- gruppo()
			malattie <- malattie_data()
			
			# Ottiene metadati malattie per questo gruppo
			df_meta <- malattie[["metadati"]]
			malattie_gruppo <- df_meta[df_meta$specie == grp, ]
			
			if (nrow(malattie_gruppo) == 0) {
				return(list())
			}
			
			# Itera su ogni malattia del gruppo
			result <- list()
			for (i in 1:nrow(malattie_gruppo)) {
				# Nome colonna con prefisso prov_
				campo_malattia <- paste0("prov_", malattie_gruppo$campo[i])
				nome_malattia <- malattie_gruppo$malattia[i]
				
				# Filtra animali da zone non indenni (FALSE)
				df_filtered <- filtra_animali_non_indenni(df, campo_malattia)
				
				# Aggiunge alla lista solo se ci sono risultati
				if (nrow(df_filtered) > 0) {
					result[[nome_malattia]] <- df_filtered
				}
			}
			
			return(result)
		})
		
		# =====================================================================
		# FILTRO: ANIMALI DA ZONE NON INDENNI PER NASCITA
		# =====================================================================
		# Per ogni malattia, crea un dataframe con animali nati in province non indenni
		
		animali_nascita_non_indenni <- reactive({
			req(dati_processati())
			req(malattie_data())
			req(gruppo())
			
			df <- dati_processati()
			grp <- gruppo()
			malattie <- malattie_data()
			
			# Ottiene metadati malattie per questo gruppo
			df_meta <- malattie[["metadati"]]
			malattie_gruppo <- df_meta[df_meta$specie == grp, ]
			
			if (nrow(malattie_gruppo) == 0) {
				return(list())
			}
			
			# Itera su ogni malattia del gruppo
			result <- list()
			for (i in 1:nrow(malattie_gruppo)) {
				# Nome colonna con prefisso nascita_
				campo_malattia <- paste0("nascita_", malattie_gruppo$campo[i])
				nome_malattia <- malattie_gruppo$malattia[i]
				
				# Filtra animali da zone non indenni (FALSE)
				df_filtered <- filtra_animali_non_indenni(df, campo_malattia)
				
				# Aggiunge alla lista solo se ci sono risultati
				if (nrow(df_filtered) > 0) {
					result[[nome_malattia]] <- df_filtered
				}
			}
			
			return(result)
		})
		
		# =====================================================================
		# RETURN: ESPORTA TUTTI I VALORI REATTIVI
		# =====================================================================
		# Questi valori sono accessibili dal server principale come pipeline$nome
		
		list(
			dati_processati = dati_processati,                           # Dataset completo
			casi_provenienza_non_trovati = casi_provenienza_non_trovati, # IDs provenienza invalid
			df_provenienza_non_trovati = df_provenienza_non_trovati,     # DF provenienza invalid
			casi_nascita_non_trovati = casi_nascita_non_trovati,         # IDs nascita invalid
			df_nascita_non_trovati = df_nascita_non_trovati,             # DF nascita invalid
			animali_provenienza_non_indenni = animali_provenienza_non_indenni, # Lista per malattia (prov)
			animali_nascita_non_indenni = animali_nascita_non_indenni    # Lista per malattia (nascita)
		)
	})
}
