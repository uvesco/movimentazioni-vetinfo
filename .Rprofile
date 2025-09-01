source("renv/activate.R")
if (file.exists("renv/activate.R")) source("renv/activate.R")
options(shiny.fullstacktrace = TRUE)  # utile in staging

options(
	repos = c(CRAN = "https://cran.rstudio.com"),
	download.file.method = "libcurl",
	pkgType = "binary"   # forza i binari Windows, evita compilazioni
)

# evita messaggi extra su Windows
options(internet.info = 0)
