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
		dati <- dati[, col_standard]
		# origine ------
		# ricavo l'origine dal codice di stalla italiano
		dati$orig_com_stor <- substr(dati$orig_stabilimento_cod, 1, 5) # parte comunale del codice di stalla storico
		dati <- merge(dati, df_codici_stabilimento[,-1], 
									by.x = "orig_com_stor",
									by.y = "COD_STABILIMENTO",
									all.x = T,
									all.y = F)
		# nascita --------
		# nato in italia
		dati$IT_n <- grepl("^IT", dati$capo_identificativo)
		
		})
	})
}