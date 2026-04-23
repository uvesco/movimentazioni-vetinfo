# Modulo Download Movimentazioni da Vetinfo BDN
# Apre le form di Vetinfo con date e parametri pre-compilati via JavaScript.
# Flusso two-step by design di Vetinfo (form → pagina intermedia → scelta Excel/Gzip).
#
# CONFIGURAZIONE: completare le costanti VETINFO_* con i valori reali osservati
# in DevTools → Rete → clic su "Invio" → Request Payload / Form Data.

# ---- Costanti di configurazione Vetinfo ----

# URL del form di estrazione dati (action del <form> in Vetinfo)
VETINFO_URL    <- "https://www.vetinfo.it/PLACEHOLDER"   # DA CONFIGURARE
VETINFO_METHOD <- "POST"                                  # POST o GET - DA VERIFICARE

# Nomi dei campi data nel form Vetinfo
VETINFO_DAL <- "P_DATA_DAL"   # nome campo data inizio - DA VERIFICARE
VETINFO_AL  <- "P_DATA_AL"    # nome campo data fine   - DA VERIFICARE

# Parametri specifici bovini e bufalini
VETINFO_BOVINI_PARAMS <- list(
  P_TIPO_REPORT       = "ingressi_bovini",
  P_AMBIENTE          = "",   # DA CONFIGURARE (valore osservato in DevTools)
  P_GRSPE_DESCRIZIONE = "",   # DA CONFIGURARE
  P_GRSPE_ID          = ""    # DA CONFIGURARE
)

# Parametri specifici ovicaprini
# ATTENZIONE: usare ingressi_ovini_capi_singoli (capi singoli), NON ingressi_ovini
VETINFO_OVICAPRINI_PARAMS <- list(
  P_TIPO_REPORT       = "ingressi_ovini_capi_singoli",
  P_AMBIENTE          = "",   # DA CONFIGURARE (valore osservato in DevTools)
  P_GRSPE_DESCRIZIONE = "",   # DA CONFIGURARE
  P_GRSPE_ID          = ""    # DA CONFIGURARE
)

# ---- UI ----

mod_download_vetinfo_ui <- function(id) {
  ns <- NS(id)

  # Inietta la configurazione R come variabile globale JavaScript
  js_cfg <- tags$script(HTML(paste0(
    "var VETINFO_CFG = {",
    "  url:    \"", VETINFO_URL, "\",",
    "  method: \"", VETINFO_METHOD, "\",",
    "  dal:    \"", VETINFO_DAL, "\",",
    "  al:     \"", VETINFO_AL, "\",",
    "  ns:     \"", id, "\",",
    "  bovini: {",
    "    P_TIPO_REPORT:       \"", VETINFO_BOVINI_PARAMS$P_TIPO_REPORT, "\",",
    "    P_AMBIENTE:          \"", VETINFO_BOVINI_PARAMS$P_AMBIENTE, "\",",
    "    P_GRSPE_DESCRIZIONE: \"", VETINFO_BOVINI_PARAMS$P_GRSPE_DESCRIZIONE, "\",",
    "    P_GRSPE_ID:          \"", VETINFO_BOVINI_PARAMS$P_GRSPE_ID, "\"",
    "  },",
    "  ovicaprini: {",
    "    P_TIPO_REPORT:       \"", VETINFO_OVICAPRINI_PARAMS$P_TIPO_REPORT, "\",",
    "    P_AMBIENTE:          \"", VETINFO_OVICAPRINI_PARAMS$P_AMBIENTE, "\",",
    "    P_GRSPE_DESCRIZIONE: \"", VETINFO_OVICAPRINI_PARAMS$P_GRSPE_DESCRIZIONE, "\",",
    "    P_GRSPE_ID:          \"", VETINFO_OVICAPRINI_PARAMS$P_GRSPE_ID, "\"",
    "  }",
    "};"
  )))

  # Logica JavaScript: legge le date dal datepicker Shiny in formato ISO yyyy-mm-dd
  # e sottomette il form a Vetinfo in una nuova scheda del browser.
  # IMPORTANTE: si legge dal datepicker interno (Date object → ISO), NON dal
  # testo formattato visibile all'utente (che sarebbe dd/mm/yyyy).
  js_logic <- tags$script(HTML('
    $(function() {

      function getISODate(inputId) {
        var dp = $("#" + inputId).data("datepicker");
        if (!dp || !dp.dates || dp.dates.length === 0) return null;
        var d = dp.dates[dp.dates.length - 1];
        if (!d) return null;
        // Usa metodi UTC per evitare shift di fuso orario
        var y   = d.getUTCFullYear();
        var m   = ("0" + (d.getUTCMonth() + 1)).slice(-2);
        var day = ("0" + d.getUTCDate()).slice(-2);
        return y + "-" + m + "-" + day;
      }

      function submitVetinfoForm(specie) {
        var cfg = VETINFO_CFG;
        var pfx = cfg.ns + "-";

        var dataDal = getISODate(pfx + "data_inizio");
        var dataAl  = getISODate(pfx + "data_fine");

        if (!dataDal || !dataAl) {
          alert("Selezionare le date di inizio e fine prima di procedere.");
          return;
        }

        var p = Object.assign(
          {},
          specie === "bovini" ? cfg.bovini : cfg.ovicaprini
        );
        p[cfg.dal] = dataDal;
        p[cfg.al]  = dataAl;

        var f = document.createElement("form");
        f.method = cfg.method;
        f.action = cfg.url;
        f.target = "_blank";

        Object.keys(p).forEach(function(k) {
          var inp = document.createElement("input");
          inp.type  = "hidden";
          inp.name  = k;
          inp.value = p[k];
          f.appendChild(inp);
        });

        document.body.appendChild(f);
        f.submit();
        document.body.removeChild(f);
      }

      $(document).on("click", "#" + VETINFO_CFG.ns + "-btn_bovini", function() {
        submitVetinfoForm("bovini");
      });
      $(document).on("click", "#" + VETINFO_CFG.ns + "-btn_ovicaprini", function() {
        submitVetinfoForm("ovicaprini");
      });
    });
  '))

  tagList(
    js_cfg,
    js_logic,

    bslib::card(
      title = "Download movimentazioni da Vetinfo BDN",
      class = "mb-3",

      p("Impostare le date e cliccare il pulsante per aprire il form di Vetinfo ",
        "pre-compilato in una nuova scheda. La ", tags$strong("data fine"),
        " è preimpostata a oggi."),

      fluidRow(
        column(4,
          dateInput(ns("data_inizio"),
                    "Data inizio",
                    value    = Sys.Date() - 30,
                    format   = "dd/mm/yyyy",
                    language = "it")
        ),
        column(4,
          dateInput(ns("data_fine"),
                    "Data fine",
                    value    = Sys.Date(),
                    format   = "dd/mm/yyyy",
                    language = "it")
        )
      ),

      div(
        class = "d-flex gap-2 mt-2 mb-3",
        actionButton(ns("btn_bovini"),
                     "Scarica Bovini e Bufalini",
                     icon  = icon("download"),
                     class = "btn-primary"),
        actionButton(ns("btn_ovicaprini"),
                     "Scarica Ovicaprini",
                     icon  = icon("download"),
                     class = "btn-success")
      ),

      hr(),

      div(
        class = "alert alert-info",
        role  = "alert",
        tags$strong("Come funziona:"),
        tags$ol(
          tags$li("Assicurarsi di essere autenticati su Vetinfo nel browser."),
          tags$li("Impostare la ", tags$strong("data di inizio"),
                  " (la data fine è già impostata a oggi)."),
          tags$li("Cliccare su un pulsante: si apre una nuova scheda con il form Vetinfo pre-compilato."),
          tags$li("Nella pagina intermedia di Vetinfo, scegliere il formato (Excel o Gzip) per avviare il download."),
          tags$li("Ripetere per il secondo gruppo di specie.")
        )
      )
    )
  )
}

# ---- Server ----

mod_download_vetinfo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Tutto gestito nel browser via JavaScript.
    # Nessuna logica server necessaria: il browser è già autenticato in Vetinfo
    # e gestisce autonomamente la sessione e il download.
  })
}
