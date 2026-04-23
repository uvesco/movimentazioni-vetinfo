# =============================================================================
# mod_download_vetinfo.R
# Modulo Shiny: Download Movimentazioni da Vetinfo BDN
#
# Genera bookmarklet JavaScript da trascinare nella barra dei preferiti.
# Il bookmarklet:
#   1. Naviga alla pagina del form di ricerca Vetinfo (stampa_movimentazioni_ric.pl)
#   2. Compila radio button e <select> già presenti nel form
#   3. Imposta data AL = oggi
#   4. Lascia solo la data DAL all'utente (un solo campo da compilare)
#   5. L'utente clicca "Invio" → pagina intermedia → scelta Excel/Gzip
#
# Approccio: il bookmarklet gira nel contesto di vetinfo.it (stesso sito),
# quindi i cookie di sessione sono sempre inclusi — nessun problema SameSite.
#
# Prerequisito: essere autenticati su vetinfo.it (SPID/CIE).
# =============================================================================

# ---- Configurazione specie -------------------------------------------------

VETINFO_BOVINI <- list(
  form_url     = "https://www.vetinfo.it/bovini/stampe/stampa_movimentazioni_ric.pl",
  tipo_report  = "ingressi_bovini",
  label        = "Bovini e Bufalini"
)

# NOTA: "ingressi_ovini_capi_singoli" = capi singoli; NON usare "ingressi_ovini" (insiemi)
VETINFO_OVICAPRINI <- list(
  form_url     = "https://www.vetinfo.it/ovicaprini/stampe/stampa_movimentazioni_ric.pl",
  tipo_report  = "ingressi_ovini_capi_singoli",
  label        = "Ovicaprini capi singoli"
)

# ---- Helper: genera javascript: URL del bookmarklet -----------------------
# Segue lo stesso pattern del bookmarklet ovicaprini verificato e funzionante.
# Il JS usa virgolette singole ovunque: le virgolette doppie nei selettori CSS
# vengono gestite correttamente da htmltools quando il valore finisce in href="".

.vetinfo_bookmarklet <- function(cfg) {
  url         <- cfg$form_url
  tipo_report <- cfg$tipo_report
  label       <- cfg$label

  paste0(
    'javascript:(function(){',
      'var T="', url, '";',
      'function a(){',
        'var f=document.querySelector(\'form[name="ricerca"]\');',
        'if(!f){',
          'alert("Pagina non riconosciuta. Aprire prima la pagina Movimentazioni ', label, ' su VetInfo.");',
          'return;',
        '}',
        'function r(n,v){',
          'var x=f.querySelector(\'input[name="\'+n+\'"][value="\'+v+\'"]\');',
          'if(x)x.checked=true;',
        '}',
        'r("P_DOVE","altre_regioni");',
        'r("P_TIPO_REPORT","', tipo_report, '");',
        'r("P_TIPO_STAMPA","EXCEL");',
        'var h=new Date();',
        'var gg=("0"+h.getDate()).slice(-2);',
        'var mm=("0"+(h.getMonth()+1)).slice(-2);',
        'var aa=h.getFullYear();',
        'f.querySelector(\'select[name="P_DT_CONTROLLO_GG_A"]\').value=gg;',
        'f.querySelector(\'select[name="P_DT_CONTROLLO_MM_A"]\').value=mm;',
        'f.querySelector(\'select[name="P_DT_CONTROLLO_AA_A"]\').value=aa;',
        'alert("\\u2705 Impostato:\\n',
          '\\u2022 Solo movim. verso altre regioni\\n',
          '\\u2022 ', label, ' IN INGRESSO\\n',
          '\\u2022 Formato EXCEL\\n',
          '\\u2022 Data AL: "+gg+"/"+mm+"/"+aa+',
          '"\\n\\nOra imposta la data DAL e clicca Invio.");',
      '}',
      'if(window.location.href.indexOf(T)!==-1){',
        # già sulla pagina giusta: compila direttamente
        'a();',
      '}else if(window.location.hostname==="www.vetinfo.it"||window.location.hostname==="vetinfo.it"){',
        # su Vetinfo ma pagina diversa: naviga nella stessa scheda (same-origin → setInterval funziona)
        'window.location.href=T;',
        'var t=0;',
        'var w=setInterval(function(){',
          't+=500;',
          'if(document.querySelector(\'form[name="ricerca"]\')){clearInterval(w);a();}',
          'if(t>10000){clearInterval(w);}',
        '},500);',
      '}else{',
        # non su Vetinfo: apre nuova scheda, istruzioni per ricliccare
        'window.open(T,"_blank");',
        'alert("Vetinfo aperto in nuova scheda.\\nNella nuova scheda clicca di nuovo il segnalibro per pre-compilare il form.");',
      '}',
    '})();'
  )
}

# ---- UI --------------------------------------------------------------------

mod_download_vetinfo_ui <- function(id) {
  ns <- NS(id)

  bm_bovini     <- .vetinfo_bookmarklet(VETINFO_BOVINI)
  bm_ovicaprini <- .vetinfo_bookmarklet(VETINFO_OVICAPRINI)

  tagList(
    bslib::card(
      bslib::card_header(
        tags$span(icon("download"), " Download movimentazioni da Vetinfo BDN")
      ),
      bslib::card_body(

        # Istruzioni principali
        div(
          class = "alert alert-info mb-3",
          tags$strong("Come usare i bookmarklet:"),
          tags$ol(
            class = "mb-0",
            tags$li(
              "Trascinare uno dei pulsanti qui sotto nella ",
              tags$strong("barra dei preferiti"), " del browser (operazione una-tantum)."
            ),
            tags$li(
              "Autenticarsi su Vetinfo (SPID/CIE) e tenerlo aperto in una scheda."
            ),
            tags$li(
              tags$strong("Dalla scheda Vetinfo"), " cliccare il segnalibro: il form viene ",
              "pre-compilato automaticamente (P_DOVE, tipo report, formato, data AL = oggi)."
            ),
            tags$li(
              "Se si clicca il segnalibro da un'altra scheda (es. questa app), Vetinfo si apre in ",
              "una nuova scheda: spostarsi lì e ", tags$strong("ricliccare il segnalibro"),
              " per pre-compilare il form."
            ),
            tags$li(
              "Impostare solo la ", tags$strong("data DAL"), " e cliccare ",
              tags$strong("Invio"), "."
            ),
            tags$li(
              "Nella pagina successiva scegliere ",
              tags$strong("Visualizza excel"), " (.xls) oppure ",
              tags$strong("Scarica File Gzip"), " (.gz)."
            )
          )
        ),

        # Pulsanti bookmarklet (draggable)
        p(tags$em("Trascinare nella barra dei preferiti:")),
        div(
          class = "d-flex gap-3 mb-3",
          tags$a(
            href  = bm_bovini,
            title = "Trascina nella barra dei preferiti del browser",
            class = "btn btn-primary",
            icon("cow"), " Bovini e Bufalini"
          ),
          tags$a(
            href  = bm_ovicaprini,
            title = "Trascina nella barra dei preferiti del browser",
            class = "btn btn-success",
            icon("sheep"), " Ovicaprini"
          )
        ),

        # Codice per copia-incolla manuale
        tags$details(
          tags$summary(
            tags$small(tags$em(
              "Copia-incolla manuale (se il drag-and-drop è bloccato dal browser)"
            ))
          ),
          div(
            class = "mt-2",
            tags$p(tags$small(tags$strong("Bovini:"))),
            tags$textarea(
              class    = "form-control font-monospace",
              style    = "font-size:0.65em; height:80px;",
              readonly = NA,
              bm_bovini
            ),
            tags$p(class = "mt-2", tags$small(tags$strong("Ovicaprini:"))),
            tags$textarea(
              class    = "form-control font-monospace",
              style    = "font-size:0.65em; height:80px;",
              readonly = NA,
              bm_ovicaprini
            ),
            tags$p(
              class = "mt-1",
              tags$small(
                "Per aggiungere manualmente: Gestione segnalibri → Nuovo segnalibro → ",
                "incollare il codice nel campo URL."
              )
            )
          )
        )
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------

mod_download_vetinfo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Nessuna logica server: i bookmarklet sono URL statici generati all'avvio.
  })
}
