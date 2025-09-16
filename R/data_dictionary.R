# i nomi delle colonne non richiamati in mod_file_check.R per cui il nome del file
# deve essere in ordine alfabetico prima di mod_file_check.R

# colonne input ovicaprini

col_ovicaprini <- c("ROWNUM", "REGIONE", "CODICE ASL", "ASL DENOMINAZIONE",
										"AZIENDA CODICE", "CODICE FISCALE", "SPECIE", "PROV",
										"COMUNE", "MOTIVO INGRESSO", "DATA INGRESSO", "CAPO CODICE",
										"CAPO CODICE ELETTRONICO", "AZI PROVENIENZA",
										"AZI REGIONE PROVENIENZA", "AZI ASL CODICE PROVENIENZA")

# colonne input bovini

col_bovini     <- c("REGIONE", "CODICE ASL", "DENOMINAZIONE ASL", "CODICE AZIENDA",
										"CODICE FISCALE", "SPECIE", "PROV", "COMUNE", "MOTIVO INGRESSO",
										"DATA INGRESSO", "CODICE CAPO", "CODICE ELETTRONICO", "RAZZA",
										"SESSO", "CODICE MADRE", "DATA NASCITA", "AZIENDA PROVENIENZA",
										"COD FISCALE PROVENIENZA", "SPECIE ALLEV PROVENIENZA",
										"REGIONE AZ. PROVENIENZA", "ASL AZ. PROVENIENZA")

# colonne standard in cui trasformare i dataframes

col_standard_bovini   <- c(
	"dest_regione_denom",
	"dest_asl_cod",
	"dest_asl_denom",
	"dest_stabilimento_cod",
	"dest_cf",
	"dest_specie",
	"dest_prov_cod_autom",
	"dest_com",
	"ingresso_motivo",
	"ingresso_data",
	"capo_identificativo",
	"capo_identificativo_elettronico",
	"capo_razza",
	"capo_sesso",
	"capo_identificativo_madre",
	"capo_data_nascita",
	"orig_stabilimento_cod",
	"orig_cf",
	"orig_specie",
	"orig_regione_cod",
	"orig_asl_cod"
)

col_standard_ovicaprini <- c(
	"num_riga",
	"dest_regione_denom",
	"dest_asl_cod",
	"dest_asl_denom",
	"dest_stabilimento_cod",
	"dest_cf",
	"dest_specie",
	"dest_prov_cod_autom",
	"dest_com",
	"ingresso_motivo",
	"ingresso_data",
	"capo_identificativo",
	"capo_identificativo_elettronico",
	"orig_stabilimento_cod",
	"orig_regione_cod",
	"orig_asl_cod"
)