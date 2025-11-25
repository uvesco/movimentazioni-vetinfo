# Module for processing animal movements with disease data validation
# This module creates the complete pipeline with all validation checks

mod_pipeline_controlli_server <- function(id, animali, gruppo, malattie_data) {
	moduleServer(id, function(input, output, session) {
		
		# Main reactive that processes the complete pipeline
		dati_processati <- reactive({
			req(animali())
			req(gruppo())
			req(malattie_data())
			
			df <- animali()
			grp <- gruppo()
			malattie <- malattie_data()
			
			# Check if we have disease data for this group
			if (is.null(malattie[[grp]])) {
				return(NULL)
			}
			
			# 1. Classify Italia/Estero
			df <- classifica_origine(df, STATIC_MOTIVI_INGRESSO)
			
			# 2. Extract provincia di nascita from ear tag
			# Pass df_province to map COD_PROV_STORICO to COD_UTS
			df$cod_uts_nascita <- estrai_provincia_nascita(df$capo_identificativo, df_province)
			
			# 3. Extract comune di provenienza from stabilimento code
			df$PRO_COM_T_prov <- estrai_comune_provenienza(df$orig_stabilimento_cod, df_stab)
			
			# 4. Merge malattie sulla provenienza (comune ISTAT)
			df_comuni_malattie <- malattie[[grp]][["comuni"]]
			if (!is.null(df_comuni_malattie) && nrow(df_comuni_malattie) > 0) {
				df <- merge_malattie_con_prefisso(
					df,
					df_comuni_malattie,
					by_animali = "PRO_COM_T_prov",
					by_malattie = "PRO_COM_T",
					prefisso = "prov_"
				)
			}
			
			# 5. Merge malattie sulla nascita (codice UTS provincia)
			df_province_malattie <- malattie[[grp]][["province"]]
			if (!is.null(df_province_malattie) && nrow(df_province_malattie) > 0) {
				df <- merge_malattie_con_prefisso(
					df,
					df_province_malattie,
					by_animali = "cod_uts_nascita",
					by_malattie = "COD_UTS",
					prefisso = "nascita_"
				)
			}
			
			return(df)
		})
		
		# Validation: animali italiani con comune di provenienza non valido
		casi_provenienza_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Get IDs of Italian animals with invalid provenance comune
			animali_invalid <- df[
				is.na(df$PRO_COM_T_prov) & df$origine == "italia",
				"capo_identificativo"
			]
			
			return(animali_invalid)
		})
		
		df_provenienza_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Create full dataframe
			df_invalid <- crea_dataframe_validazione(
				df,
				campo_geografico = "PRO_COM_T_prov",
				tipo_validazione = "comune_provenienza_non_valido"
			)
			
			return(df_invalid)
		})
		
		# Validation: animali italiani con provincia di nascita non valida
		casi_nascita_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Get IDs of Italian animals with invalid birth province
			animali_invalid <- df[
				is.na(df$cod_uts_nascita) & df$origine == "italia",
				"capo_identificativo"
			]
			
			return(animali_invalid)
		})
		
		df_nascita_non_trovati <- reactive({
			req(dati_processati())
			df <- dati_processati()
			
			# Create full dataframe
			df_invalid <- crea_dataframe_validazione(
				df,
				campo_geografico = "cod_uts_nascita",
				tipo_validazione = "provincia_nascita_non_valida"
			)
			
			return(df_invalid)
		})
		
		# Filter animals from non disease-free zones by provenance
		animali_provenienza_non_indenni <- reactive({
			req(dati_processati())
			req(malattie_data())
			req(gruppo())
			
			df <- dati_processati()
			grp <- gruppo()
			malattie <- malattie_data()
			
			# Get metadata for this group
			df_meta <- malattie[["metadati"]]
			malattie_gruppo <- df_meta[df_meta$specie == grp, ]
			
			if (nrow(malattie_gruppo) == 0) {
				return(list())
			}
			
			# Create a list with one dataframe per disease
			result <- list()
			for (i in 1:nrow(malattie_gruppo)) {
				campo_malattia <- paste0("prov_", malattie_gruppo$campo[i])
				nome_malattia <- malattie_gruppo$malattia[i]
				
				df_filtered <- filtra_animali_non_indenni(df, campo_malattia)
				
				if (nrow(df_filtered) > 0) {
					result[[nome_malattia]] <- df_filtered
				}
			}
			
			return(result)
		})
		
		# Filter animals from non disease-free zones by birth province
		animali_nascita_non_indenni <- reactive({
			req(dati_processati())
			req(malattie_data())
			req(gruppo())
			
			df <- dati_processati()
			grp <- gruppo()
			malattie <- malattie_data()
			
			# Get metadata for this group
			df_meta <- malattie[["metadati"]]
			malattie_gruppo <- df_meta[df_meta$specie == grp, ]
			
			if (nrow(malattie_gruppo) == 0) {
				return(list())
			}
			
			# Create a list with one dataframe per disease
			result <- list()
			for (i in 1:nrow(malattie_gruppo)) {
				campo_malattia <- paste0("nascita_", malattie_gruppo$campo[i])
				nome_malattia <- malattie_gruppo$malattia[i]
				
				df_filtered <- filtra_animali_non_indenni(df, campo_malattia)
				
				if (nrow(df_filtered) > 0) {
					result[[nome_malattia]] <- df_filtered
				}
			}
			
			return(result)
		})
		
		# Return all reactive values
		list(
			dati_processati = dati_processati,
			casi_provenienza_non_trovati = casi_provenienza_non_trovati,
			df_provenienza_non_trovati = df_provenienza_non_trovati,
			casi_nascita_non_trovati = casi_nascita_non_trovati,
			df_nascita_non_trovati = df_nascita_non_trovati,
			animali_provenienza_non_indenni = animali_provenienza_non_indenni,
			animali_nascita_non_indenni = animali_nascita_non_indenni
		)
	})
}
