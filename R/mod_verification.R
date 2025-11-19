# Modulo Verifica e Debug - Sistema di backup per verificare oggetti in tutte le fasi dell'elaborazione
# Questo modulo fornisce strumenti diagnostici per tracciare i dati durante il processo di elaborazione

# UI del modulo di verifica
mod_verification_ui <- function(id) {
  ns <- NS(id)
  
  bslib::card(
    title = "Sistema di Verifica e Debug",
    class = "mb-3",
    
    # Pulsante per generare il report di verifica
    actionButton(ns("genera_report"), "Genera Report di Verifica", 
                 class = "btn-primary mb-3"),
    
    # Area per mostrare i risultati
    uiOutput(ns("report_output"))
  )
}

# Server del modulo di verifica
mod_verification_server <- function(id, animali_reactive, gruppo_reactive, partite_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Funzione per analizzare il matching con motivi_ingresso
    analyze_motivi_matching <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(list(
          status = "empty",
          message = "Nessun dato disponibile per l'analisi"
        ))
      }
      
      # Verifica presenza della colonna ingresso_motivo
      if (!"ingresso_motivo" %in% colnames(df)) {
        return(list(
          status = "error",
          message = "Colonna 'ingresso_motivo' non trovata nei dati"
        ))
      }
      
      # Analizza i valori unici prima della standardizzazione (se disponibili)
      # Nota: qui lavoriamo sui dati già standardizzati, ma tracciamo cosa abbiamo
      valori_unici <- unique(df$ingresso_motivo)
      valori_unici <- valori_unici[!is.na(valori_unici)]
      
      # Carica i motivi di riferimento
      motivi_ref <- STATIC_MOTIVI_INGRESSO
      motivi_ref$Descrizione_std <- trimws(toupper(motivi_ref$Descrizione))
      
      # Crea versione standardizzata dei valori nel dataset
      df$ingresso_motivo_std <- trimws(toupper(df$ingresso_motivo))
      
      # Trova match e non-match
      valori_std_unici <- unique(df$ingresso_motivo_std[!is.na(df$ingresso_motivo_std)])
      
      matched <- valori_std_unici[valori_std_unici %in% motivi_ref$Descrizione_std]
      unmatched <- valori_std_unici[!valori_std_unici %in% motivi_ref$Descrizione_std]
      
      # Conta frequenze per valori non matchati
      if (length(unmatched) > 0) {
        freq_unmatched <- table(df$ingresso_motivo_std[df$ingresso_motivo_std %in% unmatched])
        freq_unmatched <- sort(freq_unmatched, decreasing = TRUE)
      } else {
        freq_unmatched <- NULL
      }
      
      # Conta animali con/senza match
      n_matched <- sum(df$ingresso_motivo_std %in% motivi_ref$Descrizione_std, na.rm = TRUE)
      n_unmatched <- sum(!is.na(df$ingresso_motivo_std) & 
                          !(df$ingresso_motivo_std %in% motivi_ref$Descrizione_std), na.rm = TRUE)
      n_na <- sum(is.na(df$ingresso_motivo_std))
      
      # Verifica presenza campo prov_italia
      has_prov_italia <- "prov_italia" %in% colnames(df)
      
      if (has_prov_italia) {
        # Statistiche su prov_italia
        n_prov_italia_true <- sum(!is.na(df$prov_italia) & df$prov_italia == TRUE, na.rm = TRUE)
        n_prov_italia_false <- sum(!is.na(df$prov_italia) & df$prov_italia == FALSE, na.rm = TRUE)
        n_prov_italia_na <- sum(is.na(df$prov_italia))
      } else {
        n_prov_italia_true <- NA
        n_prov_italia_false <- NA
        n_prov_italia_na <- nrow(df)
      }
      
      # Crea il risultato
      list(
        status = "success",
        n_totale = nrow(df),
        n_valori_unici = length(valori_std_unici),
        n_matched = n_matched,
        n_unmatched = n_unmatched,
        n_na = n_na,
        matched_values = matched,
        unmatched_values = unmatched,
        freq_unmatched = freq_unmatched,
        has_prov_italia = has_prov_italia,
        n_prov_italia_true = n_prov_italia_true,
        n_prov_italia_false = n_prov_italia_false,
        n_prov_italia_na = n_prov_italia_na,
        motivi_disponibili = motivi_ref$Descrizione_std
      )
    }
    
    # Funzione per analizzare le partite
    analyze_partite <- function(df_partite) {
      if (is.null(df_partite) || nrow(df_partite) == 0) {
        return(list(
          status = "empty",
          message = "Nessuna partita disponibile per l'analisi"
        ))
      }
      
      has_prov_italia <- "prov_italia" %in% colnames(df_partite)
      
      if (has_prov_italia) {
        n_partite_italia <- sum(!is.na(df_partite$prov_italia) & df_partite$prov_italia == TRUE, na.rm = TRUE)
        n_partite_estero <- sum(!is.na(df_partite$prov_italia) & df_partite$prov_italia == FALSE, na.rm = TRUE)
        n_partite_na <- sum(is.na(df_partite$prov_italia))
      } else {
        n_partite_italia <- NA
        n_partite_estero <- NA
        n_partite_na <- nrow(df_partite)
      }
      
      list(
        status = "success",
        n_partite_totale = nrow(df_partite),
        has_prov_italia = has_prov_italia,
        n_partite_italia = n_partite_italia,
        n_partite_estero = n_partite_estero,
        n_partite_na = n_partite_na
      )
    }
    
    # Generazione del report
    report_data <- eventReactive(input$genera_report, {
      df <- animali_reactive()
      grp <- gruppo_reactive()
      df_partite <- partite_reactive()
      
      if (is.null(df) || is.null(grp)) {
        return(list(
          status = "error",
          message = "Nessun dato disponibile. Carica prima un file di movimentazioni."
        ))
      }
      
      # Analizza motivi di ingresso
      motivi_analysis <- analyze_motivi_matching(df)
      
      # Analizza partite
      partite_analysis <- analyze_partite(df_partite)
      
      # Analizza nascita
      nascita_analysis <- list()
      if ("nascita_stato" %in% colnames(df)) {
        nascita_analysis$n_nati_italia <- sum(!is.na(df$nascita_stato) & df$nascita_stato == "IT", na.rm = TRUE)
        nascita_analysis$n_nati_estero <- sum(!is.na(df$nascita_stato) & df$nascita_stato != "IT", na.rm = TRUE)
        nascita_analysis$n_nascita_na <- sum(is.na(df$nascita_stato))
      }
      
      list(
        gruppo = grp,
        motivi = motivi_analysis,
        partite = partite_analysis,
        nascita = nascita_analysis
      )
    })
    
    # Rendering del report
    output$report_output <- renderUI({
      report <- report_data()
      
      if (is.null(report)) {
        return(p("Clicca su 'Genera Report di Verifica' per iniziare l'analisi."))
      }
      
      if (!is.null(report$status) && report$status == "error") {
        return(div(
          class = "alert alert-danger",
          bs_icon("exclamation-triangle"),
          " ", report$message
        ))
      }
      
      motivi <- report$motivi
      partite <- report$partite
      nascita <- report$nascita
      
      # Costruisci il report HTML
      tagList(
        h4(bs_icon("clipboard-check"), " Report di Verifica - Gruppo: ", report$gruppo),
        
        # Sezione 1: Analisi Motivi di Ingresso
        bslib::card(
          title = "1. Analisi Motivi di Ingresso",
          class = "mb-3",
          
          if (motivi$status == "success") {
            tagList(
              p(strong("Statistiche generali:")),
              tags$ul(
                tags$li("Numero totale animali: ", motivi$n_totale),
                tags$li("Valori unici in ingresso_motivo: ", motivi$n_valori_unici),
                tags$li(
                  "Animali con motivo riconosciuto: ", 
                  motivi$n_matched, 
                  " (", round(100 * motivi$n_matched / motivi$n_totale, 1), "%)",
                  if (motivi$n_matched > 0) {
                    span(class = "text-success", " ✓")
                  } else {
                    span(class = "text-danger", " ⚠")
                  }
                ),
                tags$li(
                  "Animali con motivo NON riconosciuto: ", 
                  motivi$n_unmatched, 
                  " (", round(100 * motivi$n_unmatched / motivi$n_totale, 1), "%)",
                  if (motivi$n_unmatched > 0) {
                    span(class = "text-warning", " ⚠ ATTENZIONE!")
                  } else {
                    span(class = "text-success", " ✓")
                  }
                ),
                tags$li("Animali con motivo mancante (NA): ", motivi$n_na)
              ),
              
              if (length(motivi$unmatched_values) > 0) {
                tagList(
                  hr(),
                  div(
                    class = "alert alert-warning",
                    h5(bs_icon("exclamation-triangle"), " Valori NON riconosciuti trovati!"),
                    p("I seguenti valori di 'ingresso_motivo' non corrispondono a nessuna voce in STATIC_MOTIVI_INGRESSO:"),
                    tags$pre(
                      paste(
                        sapply(names(motivi$freq_unmatched), function(val) {
                          paste0("  \"", val, "\" (", motivi$freq_unmatched[val], " occorrenze)")
                        }),
                        collapse = "\n"
                      )
                    ),
                    p(strong("Possibili cause:")),
                    tags$ul(
                      tags$li("Differenze negli spazi (iniziali, finali o multipli)"),
                      tags$li("Differenze nelle maiuscole/minuscole (già normalizzate)"),
                      tags$li("Caratteri speciali o problemi di encoding"),
                      tags$li("Valori non presenti nel file di riferimento")
                    )
                  )
                )
              } else {
                div(
                  class = "alert alert-success",
                  bs_icon("check-circle"), " Tutti i motivi di ingresso sono stati riconosciuti correttamente!"
                )
              },
              
              hr(),
              p(strong("Campo 'prov_italia' (derivato dal merge):")),
              if (motivi$has_prov_italia) {
                tags$ul(
                  tags$li(
                    "Animali da Italia (prov_italia = TRUE): ", 
                    motivi$n_prov_italia_true,
                    if (motivi$n_prov_italia_true == 0 && motivi$n_matched > 0) {
                      span(class = "text-danger", " ⚠ PROBLEMA: ci sono motivi riconosciuti ma nessuno marcato come Italia!")
                    }
                  ),
                  tags$li("Animali da Estero (prov_italia = FALSE): ", motivi$n_prov_italia_false),
                  tags$li(
                    "Animali senza informazione (prov_italia = NA): ", 
                    motivi$n_prov_italia_na,
                    if (motivi$n_prov_italia_na > 0) {
                      span(class = "text-warning", " ⚠ Questi animali non hanno origine determinata!")
                    }
                  )
                )
              } else {
                div(
                  class = "alert alert-danger",
                  bs_icon("x-circle"), " Il campo 'prov_italia' NON è presente nei dati! Il merge non è riuscito."
                )
              }
            )
          } else {
            div(class = "alert alert-warning", motivi$message)
          }
        ),
        
        # Sezione 2: Analisi Partite
        bslib::card(
          title = "2. Analisi Partite",
          class = "mb-3",
          
          if (partite$status == "success") {
            tagList(
              p(strong("Statistiche partite:")),
              tags$ul(
                tags$li("Numero totale partite: ", partite$n_partite_totale),
                if (partite$has_prov_italia) {
                  tagList(
                    tags$li("Partite da Italia: ", partite$n_partite_italia),
                    tags$li("Partite da Estero: ", partite$n_partite_estero),
                    tags$li("Partite senza informazione: ", partite$n_partite_na)
                  )
                } else {
                  tags$li(
                    class = "text-danger",
                    "Campo 'prov_italia' NON presente nelle partite"
                  )
                }
              )
            )
          } else {
            div(class = "alert alert-warning", partite$message)
          }
        ),
        
        # Sezione 3: Analisi Nascita
        bslib::card(
          title = "3. Analisi Luogo di Nascita",
          class = "mb-3",
          
          if (length(nascita) > 0) {
            tagList(
              p(strong("Statistiche nascita (campo capo_identificativo):")),
              tags$ul(
                tags$li("Animali nati in Italia (IT): ", nascita$n_nati_italia),
                tags$li("Animali nati all'estero: ", nascita$n_nati_estero),
                tags$li("Animali senza informazione nascita: ", nascita$n_nascita_na)
              )
            )
          } else {
            p("Informazioni sulla nascita non disponibili")
          }
        ),
        
        # Sezione 4: Motivi di Ingresso Disponibili
        bslib::card(
          title = "4. Motivi di Ingresso Disponibili nel Sistema",
          class = "mb-3",
          collapsible = TRUE,
          
          p("Questi sono i valori validi che il sistema riconosce:"),
          tags$pre(
            style = "max-height: 300px; overflow-y: auto;",
            paste(motivi$motivi_disponibili, collapse = "\n")
          )
        )
      )
    })
    
    # Restituisce il report data per uso esterno se necessario
    list(
      report = report_data
    )
  })
}
