# Modulo per controllare le colonne del file caricato                  # descrizione del modulo

mod_file_check_server <- function(id, animali, gruppo) {               # definizione del server del modulo
	moduleServer(id, function(input, output, session) {            # modulo server vero e proprio
		
		reactive({                                             # valore reattivo restituito
			req(animali())                                 # richiede che i dati siano presenti
			req(gruppo())                                  # richiede che il gruppo sia definito
			
			# verifica che le colonne del dataframe corrispondano a quelle standard per il gruppo
						identical(colnames(animali()), 
								col_standard_gruppi[[gruppo()]]
								) &&        # verifica corrispondenza colonne
				nrow(animali()) > 0                    # e presenza di almeno una riga
		})
	})
}