# modulo per standardizzare le colonne

mod_standardize_server <- function(id, animali, gruppo) {               # definizione del server del modulo
	moduleServer(id, function(input, output, session) {            # modulo server vero e proprio
		
		reactive({                                             # valore reattivo restituito
			req(animali())                                 # richiede che i dati siano presenti
			req(gruppo())                                  # richiede che il gruppo sia definito
			
		dati <- animali()
		
		if(gruppo() == "ovicaprini"){
			colnames(dati) <- col_standard_ovicaprini
		}
		if(gruppo() == "bovini"){
			colnames(dati) <- col_standard_bovini
		}
		dati[, col_standard]
		})
	})
}