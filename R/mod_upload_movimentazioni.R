# Modulo Upload Movimentazioni (accetta .xls e .gz)
# Dipendenze: readxl, R.utils, bslib, shiny

mod_upload_movimentazioni_ui <- function(id) {
	ns <- NS(id)
	bslib::card(
		title = "Carica file movimentazioni",
		class = "mb-3",
		shiny::fileInput(
			ns("file"),
			"Seleziona file (.xls oppure .gz)",
			accept = c(".xls", ".gz"),
			buttonLabel = "Sfoglia…"
		),
		shiny::helpText("Il file deve essere il .xls originale oppure lo stesso compresso come .gz.")
	)
}

mod_upload_movimentazioni_server <- function(id) {
	moduleServer(id, function(input, output, session) {
		
		read_mov_xls_or_gz <- function(path, orig_name = NULL) {
			if (is.null(orig_name)) orig_name <- path
			nm <- tolower(orig_name)
			
			if (grepl("\\.gz$", nm)) {
				# Decompressione SOLO base R
				tmp_xls <- tempfile(fileext = ".xls")
				in_con  <- gzfile(path, open = "rb")
				out_con <- file(tmp_xls, open = "wb")
				
				# copia a chunk
				repeat {
					chunk <- readBin(in_con, what = "raw", n = 262144L)  # 256 KB per giro
					if (length(chunk) == 0L) break
					writeBin(chunk, out_con)
				}
				
				# CHIUDI SEMPRE PRIMA DI LEGGERE
				close(in_con)
				close(out_con)
				
				# leggi: prima read_excel (auto), fallback a read_xls
				df <- tryCatch(
					readxl::read_excel(tmp_xls),
					error = function(e) readxl::read_xls(tmp_xls)
				)
				return(df)
				
			} else if (grepl("\\.xls$", nm)) {
				return(readxl::read_excel(path))  # auto-detect, ok anche per .xls
				
			} else {
				stop("Formato non supportato: usa .xls o .gz")
			}
		}
		
		
		
		dati <- reactiveVal(NULL)
		
		observeEvent(input$file, {
			req(input$file$datapath)
			withProgress(message = "Lettura file…", value = 0.1, {
				df <- read_mov_xls_or_gz(input$file$datapath, orig_name = input$file$name)
				incProgress(0.8)
				dati(df)
			})
		}, ignoreInit = TRUE)
		
		# restituisce il data.frame caricato (o NULL)
		reactive(dati())
	})
}
