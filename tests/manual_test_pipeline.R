#!/usr/bin/env Rscript
# Manual validation script for pipeline functions
# This script tests the core utility functions without requiring Shiny

# Source the necessary files
source("R/data_dictionary.R")
source("global.R")

cat("=== Testing Pipeline Utility Functions ===\n\n")

# Test 1: classifica_origine
cat("Test 1: classifica_origine\n")
test_df <- data.frame(
	ingresso_motivo = c("M", "C", "E", "N", "S", "T"),
	capo_identificativo = c("IT001", "FR123", "DE456", "IT002", "US789", "XX000"),
	stringsAsFactors = FALSE
)

result <- classifica_origine(test_df, STATIC_MOTIVI_INGRESSO)
cat("Input motivi:", test_df$ingresso_motivo, "\n")
cat("Output origine:", result$origine, "\n")
cat("Expected: italia, estero, estero, italia, estero, estero\n")
cat("✓ Test 1 completed\n\n")

# Test 2: estrai_provincia_nascita
cat("Test 2: estrai_provincia_nascita\n")
test_ear_tags <- c("IT001234567890", "IT201345678901", "FR123456789012", "IT099887766554", NA)
result <- estrai_provincia_nascita(test_ear_tags)
cat("Input ear tags:", test_ear_tags, "\n")
cat("Output COD_UTS:", result, "\n")
cat("Expected: 001, 201, NA, 099, NA\n")
cat("✓ Test 2 completed\n\n")

# Test 3: estrai_comune_provenienza
cat("Test 3: estrai_comune_provenienza\n")
test_stab_codes <- c("001TO", "002TO", "INVALID", NA)
result <- estrai_comune_provenienza(test_stab_codes, df_stab)
cat("Input stab codes:", test_stab_codes, "\n")
cat("Output PRO_COM_T:", result, "\n")
cat("Expected: 001001, 001002, NA, NA (or similar valid comune codes)\n")
cat("✓ Test 3 completed\n\n")

# Test 4: merge_malattie_con_prefisso
cat("Test 4: merge_malattie_con_prefisso\n")
test_animali <- data.frame(
	id = 1:3,
	PRO_COM_T = c("001001", "001002", "001003"),
	stringsAsFactors = FALSE
)
test_malattie <- data.frame(
	PRO_COM_T = c("001001", "001002", "001003"),
	BRC = c(TRUE, FALSE, TRUE),
	TBC = c(TRUE, TRUE, FALSE),
	stringsAsFactors = FALSE
)
result <- merge_malattie_con_prefisso(
	test_animali,
	test_malattie,
	by_animali = "PRO_COM_T",
	by_malattie = "PRO_COM_T",
	prefisso = "prov_"
)
cat("Input animali columns:", names(test_animali), "\n")
cat("Input malattie columns:", names(test_malattie), "\n")
cat("Output columns:", names(result), "\n")
cat("Expected: id, PRO_COM_T, prov_BRC, prov_TBC\n")
cat("✓ Test 4 completed\n\n")

# Test 5: filtra_animali_non_indenni
cat("Test 5: filtra_animali_non_indenni\n")
test_animali_malattie <- data.frame(
	id = 1:5,
	prov_BRC = c(TRUE, FALSE, TRUE, FALSE, TRUE),
	prov_TBC = c(TRUE, TRUE, FALSE, FALSE, TRUE),
	stringsAsFactors = FALSE
)
result_brc <- filtra_animali_non_indenni(test_animali_malattie, "prov_BRC")
result_tbc <- filtra_animali_non_indenni(test_animali_malattie, "prov_TBC")
cat("Input BRC values:", test_animali_malattie$prov_BRC, "\n")
cat("Filtered BRC (FALSE only):", result_brc$id, "\n")
cat("Expected BRC: 2, 4\n")
cat("Input TBC values:", test_animali_malattie$prov_TBC, "\n")
cat("Filtered TBC (FALSE only):", result_tbc$id, "\n")
cat("Expected TBC: 3, 4\n")
cat("✓ Test 5 completed\n\n")

# Test 6: Check malattie data structure
cat("Test 6: Verify malattie data loading\n")
cat("Loading disease data...\n")

# This would need to be tested in a reactive context, so we'll just verify the files exist
malattie_files <- list.files("data_static/malattie", pattern = "\\.xlsx$", full.names = TRUE)
cat("Found", length(malattie_files), "disease files:\n")
cat(basename(malattie_files), sep = "\n")
cat("✓ Test 6 completed\n\n")

cat("=== All manual tests completed ===\n")
cat("Note: Full pipeline testing requires running the Shiny app\n")
