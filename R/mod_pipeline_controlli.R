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
			
			# Prepara colonne normalizzate per il match su codice o descrizione
			df$ingresso_motivo_norm <- normalize_string(df$ingresso_motivo)
			motivi_norm <- STATIC_MOTIVI_INGRESSO
			motivi_norm$prov_italia <- as.logical(motivi_norm$prov_italia)
			motivi_norm$Codice_norm <- normalize_string(motivi_norm$Codice)
			motivi_norm$Descrizione_norm <- normalize_string(motivi_norm$Descrizione)
			
			lookup_cod <- setNames(motivi_norm$prov_italia, motivi_norm$Codice_norm)
			lookup_desc <- setNames(motivi_norm$prov_italia, motivi_norm$Descrizione_norm)
			
			df$orig_italia_motivo <- lookup_cod[df$ingresso_motivo_norm]
			missing_idx <- is.na(df$orig_italia_motivo)
			df$orig_italia_motivo[missing_idx] <- lookup_desc[df$ingresso_motivo_norm][missing_idx]
			
			missing_idx <- is.na(df$orig_italia_motivo)
			df$orig_italia_motivo[missing_idx] <- is_italian_establishment[missing_idx]
			
			# Rimuove colonna temporanea
			df$ingresso_motivo_norm <- NULL
			
			df$orig_italia <- df$orig_italia_motivo
			
			# -----------------------------------------------------------------
			# STEP 2: Estrazione provincia di nascita
			# -----------------------------------------------------------------
			# Estrae COD_UTS dal marchio auricolare usando mapping storico
			df$nascita_uts_cod <- estrai_provincia_nascita(df$capo_identificativo, df_province)
			ear_tag <- as.character(df$capo_identificativo)
			df$nascita_italia <- ifelse(is.na(ear_tag), FALSE, grepl("^IT", ear_tag, ignore.case = TRUE))
			
			# -----------------------------------------------------------------
			# STEP 3: Estrazione comune di provenienza
			# -----------------------------------------------------------------
			# Merge diretto con tabella stabilimenti per ottenere PRO_COM_T
			# Questo collega i codici allevamento di provenienza con il comune
			df$orig_stabilimento_cod_norm <- normalize_stab_code(df$orig_stabilimento_cod)
			df <- merge(
				df,
				df_stab[, c("cod_stab", "PRO_COM_T")],
				by.x = "orig_stabilimento_cod_norm",
				by.y = "cod_stab",
				all.x = TRUE
			)
			df$orig_stabilimento_cod_norm <- NULL
			
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
					by.x = "nascita_uts_cod",
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
			
			# -----------------------------------------------------------------
			# STEP 6: Rinomina colonne origine e aggiunge nomi geografici
			# -----------------------------------------------------------------
			if ("PRO_COM_T_prov" %in% names(df)) {
				names(df)[names(df) == "PRO_COM_T_prov"] <- "orig_comune_cod"
			}
			if ("COD_REG" %in% names(df)) {
				names(df)[names(df) == "COD_REG"] <- "orig_reg_cod"
			}
			if ("COD_UTS" %in% names(df)) {
				names(df)[names(df) == "COD_UTS"] <- "orig_uts_cod"
			}
			if (!"orig_reg_cod" %in% names(df) && "orig_comune_cod" %in% names(df)) {
				df$orig_reg_cod <- df_comuni$COD_REG[match(df$orig_comune_cod, df_comuni$PRO_COM_T)]
			}
			if (!"orig_uts_cod" %in% names(df) && "orig_comune_cod" %in% names(df)) {
				df$orig_uts_cod <- df_comuni$COD_UTS[match(df$orig_comune_cod, df_comuni$PRO_COM_T)]
			}
			if ("orig_reg_cod" %in% names(df)) {
				df$orig_reg_nome <- df_regioni$DEN_REG[match(df$orig_reg_cod, df_regioni$COD_REG)]
			}
			if ("orig_uts_cod" %in% names(df)) {
				df$orig_uts_nome <- df_province$DEN_UTS[match(df$orig_uts_cod, df_province$COD_UTS)]
			}
			if ("orig_comune_cod" %in% names(df)) {
				df$orig_comune_nome <- df_comuni$COMUNE[match(df$orig_comune_cod, df_comuni$PRO_COM_T)]
			}
			
			# Ordina colonne per leggibilità
			current_cols <- names(df)
			orig_cols <- current_cols[grepl("^orig_", current_cols)]
			orig_geo_order <- c(
				"orig_reg_cod",
				"orig_uts_cod",
				"orig_comune_cod",
				"orig_reg_nome",
				"orig_uts_nome",
				"orig_comune_nome"
			)
			orig_cols_base <- orig_cols[!orig_cols %in% orig_geo_order]
			orig_cols_ordered <- c(orig_cols_base, orig_geo_order[orig_geo_order %in% orig_cols])
			prov_cols <- current_cols[grepl("^prov_", current_cols)]
			nascita_cols <- current_cols[grepl("^nascita_", current_cols)]
			nascita_disease_cols <- setdiff(nascita_cols, c("nascita_italia", "nascita_uts_cod"))
			other_cols <- current_cols[!current_cols %in% c(orig_cols, prov_cols, nascita_cols)]
			
			new_order <- c(
				other_cols,
				orig_cols_ordered,
				prov_cols,
				intersect("nascita_italia", current_cols),
				intersect("nascita_uts_cod", current_cols),
				nascita_disease_cols
			)
			df <- df[, unique(new_order), drop = FALSE]
			
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
			
			# Filtra: italiani (orig_italia == TRUE) con orig_comune_cod = NA
			animali_invalid <- df[
				is.na(df$orig_comune_cod) & df$orig_italia == TRUE & !is.na(df$orig_italia),
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
				campo_geografico = "orig_comune_cod",
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
			
			# Filtra: italiani (orig_italia == TRUE) con nascita_uts_cod = NA
			animali_invalid <- df[
				is.na(df$nascita_uts_cod) & df$orig_italia == TRUE & !is.na(df$orig_italia),
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
				campo_geografico = "nascita_uts_cod",
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
