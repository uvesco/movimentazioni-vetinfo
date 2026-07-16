# =============================================================================
# mod_download_vetinfo.R
# Modulo Shiny: Download Movimentazioni da Vetinfo BDN
#
# Genera bookmarklet JavaScript da trascinare nella barra dei preferiti
# (o da copiare come URL di un nuovo segnalibro tramite i link "Copia codice").
# Il bookmarklet:
#   1. Sulla pagina del form di ricerca Vetinfo (stampa_movimentazioni_ric.pl)
#      compila radio button e <select> già presenti nel form e imposta
#      data AL = oggi; resta da compilare solo la data DAL
#   2. Da un'altra pagina apre il form (stessa scheda se già su Vetinfo,
#      nuova scheda altrimenti) e chiede di ricliccare il segnalibro:
#      i timer del documento muoiono alla navigazione, non si può pollare
#   3. L'utente clicca "Invio" → pagina intermedia → scelta Excel/Gzip
#
# Varianti disponibili (radio P_DOVE del form, verificato identico per
# bovini e ovicaprini): "tutte" = tutte le movimentazioni;
# "altre_regioni" = solo quelle in provenienza da altre regioni.
# (Esiste anche "stessa_regione", non usata qui.)
#
# Approccio: il bookmarklet gira nel contesto di vetinfo.it (stesso sito),
# quindi i cookie di sessione sono sempre inclusi — nessun problema SameSite.
#
# Prerequisito: essere autenticati su vetinfo.it (SPID/CIE) ed essere entrati
# nell'applicativo di specie con il proprio ruolo (es. Servizi Veterinari).
# =============================================================================

# ---- Configurazione specie -------------------------------------------------

VETINFO_BOVINI <- list(
  form_url     = "https://www.vetinfo.it/bovini/stampe/stampa_movimentazioni_ric.pl",
  tipo_report  = "ingressi_bovini",
  label        = "Bovini e Bufalini",
  icona        = "cow",
  classe_btn   = "btn-primary"
)

# NOTA: "ingressi_ovini_capi_singoli" = capi singoli; NON usare "ingressi_ovini" (insiemi)
VETINFO_OVICAPRINI <- list(
  form_url     = "https://www.vetinfo.it/ovicaprini/stampe/stampa_movimentazioni_ric.pl",
  tipo_report  = "ingressi_ovini_capi_singoli",
  label        = "Ovicaprini capi singoli",
  icona        = "paw",   # Font Awesome free non ha un'icona pecora
  classe_btn   = "btn-success"
)

# ---- Configurazione varianti filtro P_DOVE ---------------------------------

VETINFO_VARIANTI <- list(
  list(chiave = "tutte", dove = "tutte",
       titolo = "Tutte le movimentazioni in ingresso"),
  list(chiave = "altre", dove = "altre_regioni",
       titolo = "Solo ingressi da altre regioni")
)

# ---- Helper: genera javascript: URL del bookmarklet -----------------------
# Segue lo stesso pattern del bookmarklet ovicaprini verificato e funzionante.
# Il JS usa virgolette singole ovunque: le virgolette doppie nei selettori CSS
# vengono gestite correttamente da htmltools quando il valore finisce in href="".

.vetinfo_bookmarklet <- function(cfg, dove = c("altre_regioni", "tutte")) {
  dove        <- match.arg(dove)
  url         <- cfg$form_url
  tipo_report <- cfg$tipo_report
  label       <- cfg$label
  dove_alert  <- if (dove == "tutte") {
    "Tutte le movim. (anche stessa regione)"
  } else {
    "Solo movim. da altre regioni"
  }

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
        'r("P_DOVE","', dove, '");',
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
          '\\u2022 ', dove_alert, '\\n',
          '\\u2022 ', label, ' IN INGRESSO\\n',
          '\\u2022 Formato EXCEL\\n',
          '\\u2022 Data AL: "+gg+"/"+mm+"/"+aa+',
          '"\\n\\nOra imposta la data DAL e clicca Invio.");',
      '}',
      'if(window.location.href.indexOf(T)!==-1){',
        # già sulla pagina giusta: compila direttamente
        'a();',
      '}else if(window.location.hostname==="www.vetinfo.it"||window.location.hostname==="vetinfo.it"){',
        # su Vetinfo ma pagina diversa: i timer del documento corrente muoiono
        # al commit della navigazione, quindi non si può pollare il nuovo form:
        # si avvisa e si naviga; l'utente riclicca il segnalibro sul form
        'alert("Apro la pagina del form movimentazioni ', label, '.\\nA caricamento avvenuto cliccare di nuovo il segnalibro per pre-compilare il form.");',
        'window.location.href=T;',
      '}else{',
        # non su Vetinfo: apre nuova scheda, istruzioni per ricliccare
        'window.open(T,"_blank");',
        'alert("Vetinfo aperto in nuova scheda.\\nNella nuova scheda clicca di nuovo il segnalibro per pre-compilare il form.");',
      '}',
    '})();'
  )
}

# ---- Helper: blocco UI con bookmarklet, link "Copia codice" e textarea -----
# Usato sia dal modulo sia dalla pagina Help (app_ui.R). id_prefix garantisce
# id univoci se il blocco compare più volte nella stessa pagina.

.vetinfo_bookmarklet_block <- function(id_prefix = "bm") {
  specie <- list(VETINFO_BOVINI, VETINFO_OVICAPRINI)

  snippet_id <- function(v, s) {
    paste(id_prefix, v$chiave, sub(" .*", "", tolower(s$label)), sep = "_")
  }

  copia_link <- function(id) {
    tags$a(
      href    = "#",
      class   = "small",
      onclick = sprintf("window.copiaSnippetVetinfo('%s',this);return false;", id),
      icon("copy"), " Copia codice"
    )
  }

  # Pulsanti trascinabili raggruppati per variante, ognuno con link di copia
  righe <- lapply(VETINFO_VARIANTI, function(v) {
    div(
      class = "mb-3",
      tags$strong(v$titolo),
      div(
        class = "d-flex gap-3 align-items-center flex-wrap mt-1",
        lapply(specie, function(s) {
          span(
            tags$a(
              href  = .vetinfo_bookmarklet(s, v$dove),
              title = "Trascina nella barra dei preferiti del browser",
              class = paste("btn btn-sm", s$classe_btn),
              icon(s$icona), " ", s$label
            ),
            " ",
            copia_link(snippet_id(v, s))
          )
        })
      )
    )
  })

  # Codice completo dei segnalibri, per copia manuale
  dettagli <- tags$details(
    tags$summary(tags$small(tags$em(
      "Codice dei segnalibri (copia-incolla manuale se il drag-and-drop è bloccato)"
    ))),
    div(
      class = "mt-2",
      lapply(VETINFO_VARIANTI, function(v) {
        lapply(specie, function(s) {
          id <- snippet_id(v, s)
          tagList(
            tags$p(
              class = "mt-2 mb-1",
              tags$small(
                tags$strong(paste0(s$label, " — ", v$titolo, ": ")),
                copia_link(id)
              )
            ),
            tags$textarea(
              id       = id,
              class    = "form-control font-monospace",
              style    = "font-size:0.65em; height:80px;",
              readonly = NA,
              .vetinfo_bookmarklet(s, v$dove)
            )
          )
        })
      }),
      tags$p(
        class = "mt-1",
        tags$small(
          "Per aggiungere manualmente: Gestione segnalibri → Nuovo segnalibro → ",
          "incollare il codice nel campo URL."
        )
      )
    )
  )

  # Helper JS di copia negli appunti (con fallback per contesti non sicuri).
  # L'etichetta originale è salvata una sola volta (data-orig) e il timer
  # precedente viene cancellato: click ravvicinati non congelano il feedback.
  script <- tags$script(HTML(paste0(
    "window.copiaSnippetVetinfo=function(id,link){",
      "var t=document.getElementById(id);",
      "if(!t)return;",
      "if(!link.dataset.orig)link.dataset.orig=link.innerHTML;",
      "var setMsg=function(m,ms){",
        "link.innerHTML=m;",
        "clearTimeout(link.cpTimer);",
        "link.cpTimer=setTimeout(function(){link.innerHTML=link.dataset.orig;},ms||1500);",
      "};",
      "var done=function(){setMsg('Copiato \\u2713');};",
      "var fallback=function(){",
        "var d=t.closest('details');",
        "if(d)d.open=true;",
        "t.focus();t.select();",
        "try{if(document.execCommand('copy')){done();return;}}catch(e){}",
        "setMsg('Copia non riuscita: copiare dal riquadro',4000);",
      "};",
      "if(navigator.clipboard&&window.isSecureContext){",
        "navigator.clipboard.writeText(t.value).then(done,fallback);",
      "}else{fallback();}",
    "};"
  )))

  tagList(righe, dettagli, script)
}

# ---- UI --------------------------------------------------------------------

mod_download_vetinfo_ui <- function(id) {
  ns <- NS(id)

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
              "Trascinare i pulsanti desiderati nella ",
              tags$strong("barra dei preferiti"), " del browser (operazione una-tantum), ",
              "oppure usare ", tags$strong("Copia codice"), " e incollare il codice ",
              "nel campo URL di un nuovo segnalibro."
            ),
            tags$li(
              "Scegliere la variante: ", tags$strong("tutte le movimentazioni"),
              " oppure ", tags$strong("solo ingressi da altre regioni"), "."
            ),
            tags$li(
              "Autenticarsi su Vetinfo (SPID/CIE) ed entrare nell'",
              tags$strong("applicativo della specie"), " con il proprio ruolo ",
              "(senza ruolo la pagina del form risponde \"RUOLO NON ASSOCIATO ALL'UTENTE\")."
            ),
            tags$li(
              tags$strong("Dalla pagina del form movimentazioni"), " cliccare il segnalibro: ",
              "il form viene pre-compilato automaticamente (filtro movimentazioni, ",
              "tipo report, formato EXCEL, data AL = oggi)."
            ),
            tags$li(
              "Se si clicca il segnalibro da un'altra pagina o scheda, il bookmarklet apre ",
              "la pagina del form (stessa scheda se già su Vetinfo, nuova scheda altrimenti): ",
              "una volta caricata, ", tags$strong("ricliccare il segnalibro"),
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

        .vetinfo_bookmarklet_block(ns("bm"))
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
