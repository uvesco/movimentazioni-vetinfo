# Imposta timezone coerente con te
Sys.setenv(TZ = "Europe/Rome")

library(shiny)

ui <- fluidPage(
	titlePanel("La mia Shiny su shinyapps.io"),
	sidebarLayout(
		sidebarPanel(
			textInput("name", "Come ti chiami?", ""),
			actionButton("go", "Saluta")
		),
		mainPanel(
			verbatimTextOutput("greet")
		)
	)
)

server <- function(input, output, session) {
	# Esempio uso variabili d'ambiente (impostale su shinyapps.io)
	api_key <- Sys.getenv("MY_API_KEY", unset = NA)
	
	observeEvent(input$go, {
		msg <- if (!is.na(api_key)) {
			paste("Ciao", input$name, "- ho anche una API key configurata.")
		} else {
			paste("Ciao", input$name, "- nessuna API key configurata (ok in dev).")
		}
		output$greet <- renderText(msg)
	})
}

shinyApp(ui, server)
