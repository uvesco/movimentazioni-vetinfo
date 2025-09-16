# Modulo per controllare le colonne del file caricato                  # descrizione del modulo

mod_file_check_server <- function(id, animali, gruppo) {               # definizione del server del modulo
	moduleServer(id, function(input, output, session) {            # modulo server vero e proprio
		
		reactive({                                             # valore reattivo restituito
			req(animali())                                 # richiede che i dati siano presenti
			req(gruppo())                                  # richiede che il gruppo sia definito
			
			cols_attuali <- colnames(animali())            # colonne presenti nel file
			
			cols_attese <- switch(                         # colonne attese in base al gruppo
				gruppo(),
				ovicaprini = col_ovicaprini,
				bovini = col_bovini,
				character(0)                           # default se gruppo non previsto
			)
			
			identical(cols_attuali, cols_attese) &&        # verifica corrispondenza colonne
				nrow(animali()) > 0                    # e presenza di almeno una riga
		})
	})
}