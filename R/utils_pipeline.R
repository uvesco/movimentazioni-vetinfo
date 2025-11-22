# Utility functions for the movimentazioni pipeline

# Function 1: Classify animal origin as "italia" or "estero"
# Based on: motivo_ingresso, codice_stabilimento, or pattern in capo_identificativo
classifica_origine <- function(df, motivi_ingresso_table) {
	# Start with assumption based on motivo_ingresso
	df <- merge(
		df,
		motivi_ingresso_table[, c("Codice", "prov_italia")],
		by.x = "ingresso_motivo",
		by.y = "Codice",
		all.x = TRUE
	)
	
	# Create the origine field based on prov_italia
	df$origine <- ifelse(
		is.na(df$prov_italia) | df$prov_italia == FALSE,
		"estero",
		"italia"
	)
	
	# Remove the temporary prov_italia column
	df$prov_italia <- NULL
	
	return(df)
}

# Function 2: Extract provincia code (COD_UTS) from ear tag (marchio auricolare)
# Italian ear tags typically have format: IT<digits>XXXX where digits encode province
# The first 3 digits after IT represent the province code
estrai_provincia_nascita <- function(capo_identificativo) {
	# Extract COD_UTS from ear tag pattern
	# Pattern: IT followed by digits, first 3 digits are province identifier
	# Example: IT001... -> 001 (numeric province code that needs to be mapped to UTS)
	
	# Convert to character to ensure string operations work
	ear_tag <- as.character(capo_identificativo)
	
	# Check if it starts with IT (Italian animals)
	# Italian ear tags are typically IT followed by 12 digits
	is_italian <- grepl("^IT[0-9]{3}", ear_tag, ignore.case = TRUE)
	
	# Extract the 3-digit code after IT
	cod_provincia <- ifelse(
		is_italian,
		substr(ear_tag, 3, 5),  # positions 3-5 (after IT)
		NA_character_
	)
	
	# The extracted code is a 3-digit numeric province code
	# This needs to be formatted as a zero-padded 3-character string
	# which should match COD_UTS in most cases
	cod_uts <- ifelse(
		!is.na(cod_provincia),
		sprintf("%03s", cod_provincia),
		NA_character_
	)
	
	return(cod_uts)
}

# Function 3: Extract comune ISTAT code from stabilimento code
# Stabilimento codes are in format: XXXUU where XXX is numeric and UU is province abbreviation
# This needs to be matched against df_stab table
estrai_comune_provenienza <- function(orig_stabilimento_cod, df_stab_table) {
	# Merge with the stabilimento table to get PRO_COM_T
	result <- data.frame(
		orig_stabilimento_cod = orig_stabilimento_cod,
		stringsAsFactors = FALSE
	)
	
	result <- merge(
		result,
		df_stab_table[, c("cod_stab", "PRO_COM_T")],
		by.x = "orig_stabilimento_cod",
		by.y = "cod_stab",
		all.x = TRUE
	)
	
	# Return just the PRO_COM_T column
	return(result$PRO_COM_T)
}

# Function 4: Merge disease data with prefix
merge_malattie_con_prefisso <- function(df_animali, df_malattie, by_animali, by_malattie, prefisso) {
	# Geographic columns that should not be renamed
	geo_cols <- c("COD_REG", "COD_UTS", "PRO_COM_T")
	
	# Get all disease columns (excluding the join keys and geographic columns)
	all_cols <- names(df_malattie)
	exclude_cols <- c(by_malattie, geo_cols)
	disease_cols <- setdiff(all_cols, exclude_cols)
	
	# Create the merge
	result <- merge(
		df_animali,
		df_malattie,
		by.x = by_animali,
		by.y = by_malattie,
		all.x = TRUE,
		suffixes = c("", ".y")  # Add suffix to duplicate columns from malattie
	)
	
	# Remove duplicate geographic columns from malattie table (those with .y suffix)
	duplicate_geo_cols <- paste0(geo_cols, ".y")
	result <- result[, !(names(result) %in% duplicate_geo_cols), drop = FALSE]
	
	# Rename disease columns with prefix
	for (col in disease_cols) {
		old_name <- col
		new_name <- paste0(prefisso, col)
		if (old_name %in% names(result)) {
			names(result)[names(result) == old_name] <- new_name
		}
	}
	
	return(result)
}

# Function 5: Create validation dataframe for animals with invalid geographic codes
crea_dataframe_validazione <- function(df_animali, campo_geografico, tipo_validazione) {
	# Filter animals where geographic field is NA and origin is "italia"
	df_invalid <- df_animali[
		is.na(df_animali[[campo_geografico]]) & 
		df_animali$origine == "italia",
	]
	
	# Add a validation type column
	df_invalid$tipo_errore <- tipo_validazione
	
	return(df_invalid)
}

# Function 6: Filter animals from non disease-free zones
filtra_animali_non_indenni <- function(df_animali, campo_malattia) {
	# Check if the disease field exists
	if (!campo_malattia %in% names(df_animali)) {
		return(data.frame())  # Return empty dataframe if field doesn't exist
	}
	
	# Filter animals where disease field is FALSE (non-disease-free)
	df_non_indenni <- df_animali[
		!is.na(df_animali[[campo_malattia]]) & 
		df_animali[[campo_malattia]] == FALSE,
	]
	
	return(df_non_indenni)
}
