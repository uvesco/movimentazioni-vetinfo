# Nomi delle colonne standard e originali per i diversi gruppi di specie
# Questo file deve essere caricato prima di altri moduli che usano queste variabili

# colonne originali input 

col_orig_gruppi <- list(
	ovicaprini = c("ROWNUM", "REGIONE", "CODICE ASL", "ASL DENOMINAZIONE",
									"AZIENDA CODICE", "CODICE FISCALE", "SPECIE", "PROV",
									"COMUNE", "MOTIVO INGRESSO", "DATA INGRESSO", "CAPO CODICE",
									"CAPO CODICE ELETTRONICO", "AZI PROVENIENZA",
									"AZI REGIONE PROVENIENZA", "AZI ASL CODICE PROVENIENZA"),
	bovini = c("REGIONE", "CODICE ASL", "DENOMINAZIONE ASL", "CODICE AZIENDA",
							"CODICE FISCALE", "SPECIE", "PROV", "COMUNE", "MOTIVO INGRESSO",
							"DATA INGRESSO", "CODICE CAPO", "CODICE ELETTRONICO", "RAZZA",
							"SESSO", "CODICE MADRE", "DATA NASCITA", "AZIENDA PROVENIENZA",
							"COD FISCALE PROVENIENZA", "SPECIE ALLEV PROVENIENZA",
							"REGIONE AZ. PROVENIENZA", "ASL AZ. PROVENIENZA")
)

# colonne standard in cui trasformare i dataframes
# nomi colonne da applicare a un file di gruppo specie bovini
col_standard_gruppi <- list(
	bovini = c(
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
	),
	ovicaprini = c(
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
)


# colonne standard da applicare a tutti i gruppi di specie
col_standard <- c(
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

# colonne generate per il controllo malattie in utils_pipeline.R
col_connect <- c(
	"orig_italia",
	"orig_italia_motivo",
	"orig_reg_cod",
	"orig_uts_cod",
	"orig_comune_cod",
	"orig_reg_nome",
	"orig_uts_nome",
	"orig_comune_nome",
	"nascita_italia",
	"nascita_uts_cod")

# nomi dei fogli che distinguono l'approccio provincia indenne dall'approccio a blocchi
tipi_files_malattie_fogli <- list(
	province_indenni = c("province", "metadati"),
	blocchi = c("regioni", "province", "comuni", "metadati")
)
