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
# Oltre ai 4 bookmarklet semplici (variante x specie) esiste un quinto
# bookmarklet "doppia specie" (.vetinfo_bookmarklet_doppio): chiede intervallo
# date e filtro, poi processa in sequenza bovini e ovicaprini fino al download
# automatico dei due .gz, pilotando UNA finestra dedicata (window.open riusata)
# con un setInterval che gira nell'opener.
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

# ---- Helper: bookmarklet "doppia specie" con download automatico ----------
# Un unico segnalibro che chiede intervallo date (DAL obbligatoria, AL default
# oggi) e filtro provenienza, poi processa in sequenza BOVINI e OVICAPRINI fino
# al download automatico dei due .gz. Design:
#   - i timer del documento muoiono alla navigazione stessa-scheda, quindi
#     l'automazione gira nell'OPENER (setInterval) e pilota UNA finestra
#     dedicata aperta con window.open (riferimento mantenuto, riusata per
#     entrambe le specie); il window.open avviene SUBITO dentro la user
#     gesture, prima dei dialoghi (l'attivazione transitoria scade in pochi
#     secondi e il popup verrebbe bloccato);
#   - fine() setta lo stato terminale "STOP": callback asincrone (fetch,
#     setTimeout della pausa) e fallisci() escono se il run e' terminato o la
#     finestra e' chiusa, per non riaprire dialoghi dopo un abort;
#   - limiti noti e accettati: i dialoghi girano nella scheda di partenza
#     (se in background Chrome non la porta in primo piano) e il throttling
#     dei timer di tab nascoste puo' rallentare i tick su run molto lunghi;
#   - submit programmatico: f.target="_self" + ricerca_valori() (fallback
#     f.submit()) -> attendere.pl (auto-reload) -> stampa_movimentazioni_fin.pl;
#   - il path del .gz si estrae dagli ATTRIBUTI onclick dei link (non da
#     innerHTML, dove le virgolette diventano &quot;);
#   - i .gz rispondono 503 senza Referer: la fetch va eseguita nel contesto
#     della finestra (w.fetch), download su disco via blob + <a download>
#     creato nel documento della finestra;
#   - ogni accesso a w.document/w.location e' in try/catch: un documento
#     cross-origin (es. redirect SPID a sessione scaduta) lancia SecurityError
#     e viene trattato come "non pronto" (ci pensa il timeout di fase).
# Stessa convenzione del generatore semplice: una istruzione JS per riga R,
# apici singoli R fuori / doppi JS dentro, \\uXXXX per unicode, \\n nei
# dialoghi, niente "%" ne' newline letterali nel risultato.

.vetinfo_bookmarklet_doppio <- function() {
  b <- VETINFO_BOVINI
  o <- VETINFO_OVICAPRINI

  # Guardie (solo su www.vetinfo.it: il popup lavora su www e da un altro
  # host sarebbe cross-origin), lock anti doppio avvio, config specie
  js_guardie <- paste0(
    'var H=window.location.hostname;',
    'if(H!=="www.vetinfo.it"){',
      'alert("Questo segnalibro va cliccato da una pagina del sito www.vetinfo.it.\\nAutenticarsi su Vetinfo (SPID/CIE), entrare negli applicativi di specie e ricliccare il segnalibro.");',
      'return;',
    '}',
    'if(window.__vetinfoDL_run){',
      'alert("Download automatico gi\\u00e0 in corso.\\nAttendere il riepilogo finale, oppure chiudere la finestra di lavoro per interrompere.");',
      'return;',
    '}',
    'var SPECIE=[',
      '{nome:"', b$label, '",url:"', b$form_url, '",report:"', b$tipo_report, '"},',
      '{nome:"', o$label, '",url:"', o$form_url, '",report:"', o$tipo_report, '"}',
    '];'
  )

  # Finestra di lavoro aperta SUBITO (l'attivazione utente transitoria scade
  # in pochi secondi: aprire dopo i prompt farebbe scattare il popup blocker),
  # poi input sincrono: date con riprompt e filtro provenienza
  js_input <- paste0(
    'var w=window.open(SPECIE[0].url,"vetinfoDL");',
    'if(!w){',
      'alert("Popup bloccato: consentire i popup per www.vetinfo.it e ricliccare il segnalibro.");',
      'return;',
    '}',
    'window.__vetinfoDL_run=true;',
    'function abbandona(){',
      'window.__vetinfoDL_run=false;',
      'try{w.close();}catch(e){}',
    '}',
    'function parseData(s){',
      'var m=/^([0-3]?\\d)\\/([01]?\\d)\\/(\\d{4})$/.exec(s?s.trim():"");',
      'if(!m)return null;',
      'var g=parseInt(m[1],10),me=parseInt(m[2],10),a=parseInt(m[3],10);',
      'var d=new Date(a,me-1,g);',
      'if(d.getFullYear()!==a||d.getMonth()!==me-1||d.getDate()!==g)return null;',
      'return{gg:("0"+g).slice(-2),mm:("0"+me).slice(-2),aa:String(a),t:d.getTime()};',
    '}',
    'function askData(msg,def){',
      'for(;;){',
        'var r=prompt(msg,def);',
        'if(r===null)return null;',
        'var d=parseData(r);',
        'if(d)return d;',
        'alert("Data non valida: usare il formato gg/mm/aaaa (es. 01/07/2026).");',
      '}',
    '}',
    'var oggi=new Date();',
    'var defAl=("0"+oggi.getDate()).slice(-2)+"/"+("0"+(oggi.getMonth()+1)).slice(-2)+"/"+oggi.getFullYear();',
    'var dal=askData("Data DAL (inizio periodo, formato gg/mm/aaaa):","");',
    'if(!dal){',
      'abbandona();',
      'return;',
    '}',
    'var al;',
    'for(;;){',
      'al=askData("Data AL (fine periodo, formato gg/mm/aaaa):",defAl);',
      'if(!al){',
        'abbandona();',
        'return;',
      '}',
      'if(al.t>=dal.t)break;',
      'alert("La data AL deve essere uguale o successiva alla data DAL.");',
    '}',
    'var dove=confirm("Filtro movimentazioni in ingresso:\\nOK = SOLO ingressi da altre regioni\\nAnnulla = TUTTE le movimentazioni")?"altre_regioni":"tutte";'
  )

  # Helper della macchina a stati: esiti, avanzamento specie, compilazione form
  js_helper <- paste0(
    'var idx=0,stato="FORM",t0=Date.now(),esiti=[],dlAvviato=false,gzPath="",timer=null;',
    'var LIMITI={FORM:30000,FIN:180000,DL:60000};',
    'function fine(){',
      'stato="STOP";',
      'clearInterval(timer);',
      'window.__vetinfoDL_run=false;',
      'var msg="Download automatico Vetinfo, riepilogo:\\n\\n"+esiti.join("\\n");',
      'if(!w.closed)msg=msg+"\\n\\nLa finestra di lavoro resta aperta: in caso di problemi completare a mano con Scarica File Gzip.";',
      'alert(msg);',
    '}',
    'function prossima(){',
      'if(stato==="STOP")return;',
      'idx=idx+1;',
      'if(idx>=SPECIE.length){',
        'fine();',
        'return;',
      '}',
      'stato="FORM";',
      't0=Date.now();',
      'dlAvviato=false;',
      'try{w.location.href=SPECIE[idx].url;}catch(e){}',
    '}',
    'function fallisci(msg){',
      'if(stato==="STOP")return;',
      'esiti.push("\\u274c "+SPECIE[idx].nome+": "+msg);',
      'if(idx+1<SPECIE.length&&!w.closed&&confirm(SPECIE[idx].nome+": "+msg+"\\n\\nOK = continua con "+SPECIE[idx+1].nome+"\\nAnnulla = interrompi")){',
        'prossima();',
      '}else{',
        'fine();',
      '}',
    '}',
    'function setRadio(f,n,v){',
      'var x=f.querySelector(\'input[name="\'+n+\'"][value="\'+v+\'"]\');',
      'if(!x)return"manca l\\u2019opzione "+v+" del campo "+n;',
      'x.checked=true;',
      'return null;',
    '}',
    'function setSel(f,n,v){',
      'var s=f.querySelector(\'select[name="\'+n+\'"]\');',
      'if(!s)return"manca il campo "+n;',
      's.value=v;',
      'if(s.value!==v)return"valore "+v+" non disponibile nel campo "+n+" (anno fuori dalle opzioni del form?)";',
      'return null;',
    '}',
    'function compila(f){',
      'var errs=[',
        'setRadio(f,"P_DOVE",dove),',
        'setRadio(f,"P_TIPO_REPORT",SPECIE[idx].report),',
        'setRadio(f,"P_TIPO_STAMPA","EXCEL"),',
        'setSel(f,"P_DT_CONTROLLO_GG_DA",dal.gg),',
        'setSel(f,"P_DT_CONTROLLO_MM_DA",dal.mm),',
        'setSel(f,"P_DT_CONTROLLO_AA_DA",dal.aa),',
        'setSel(f,"P_DT_CONTROLLO_GG_A",al.gg),',
        'setSel(f,"P_DT_CONTROLLO_MM_A",al.mm),',
        'setSel(f,"P_DT_CONTROLLO_AA_A",al.aa)',
      '].filter(function(x){return x;});',
      'return errs.length?errs.join("; "):null;',
    '}'
  )

  # Macchina a stati: FORM -> FIN -> DL per ogni specie, un tick ogni 800 ms
  js_stati <- paste0(
    'function scarica(d){',
      'var mio=idx;',
      'w.fetch(gzPath).then(function(r){',
        'if(!r.ok)throw new Error("HTTP "+r.status);',
        'return r.blob();',
      '}).then(function(bl){',
        'if(w.closed||idx!==mio||stato!=="DL")return;',
        'var u=w.URL.createObjectURL(bl);',
        'var a=d.createElement("a");',
        'a.href=u;',
        'a.download=gzPath.split("/").pop();',
        'd.body.appendChild(a);',
        'a.click();',
        'a.remove();',
        'esiti.push("\\u2705 "+SPECIE[mio].nome+": avviato il download di "+a.download);',
        'stato="PAUSA";',
        'setTimeout(function(){',
          'try{w.URL.revokeObjectURL(u);}catch(e){}',
          'prossima();',
        '},1500);',
      '}).catch(function(e){',
        'if(w.closed||idx!==mio||stato!=="DL")return;',
        'fallisci("download non riuscito ("+e.message+")");',
      '});',
    '}',
    'function tick(){',
      'if(stato==="PAUSA")return;',
      'if(w.closed){',
        'esiti.push("\\u274c interrotto: finestra di lavoro chiusa");',
        'fine();',
        'return;',
      '}',
      'var d=null,path="";',
      'try{',
        'if(w.document&&w.document.readyState!=="loading"){',
          'd=w.document;',
          'path=w.location.pathname;',
        '}',
      '}catch(e){',
        'd=null;',
      '}',
      'if(d){',
        'if(stato==="FORM"){',
          'if(!d.__bmGestito&&d.body&&d.body.textContent.indexOf("RUOLO NON ASSOCIATO")!==-1){',
            'd.__bmGestito=true;',
            'fallisci("ruolo non associato: entrare prima nell\\u2019applicativo di specie dal portale Vetinfo");',
            'return;',
          '}',
          'if(path.indexOf("stampa_movimentazioni_ric")!==-1){',
            'var f=d.querySelector(\'form[name="ricerca"]\');',
            'if(f&&!d.__bmFatto){',
              'd.__bmFatto=true;',
              'var err=compila(f);',
              'if(err){',
                'fallisci(err);',
                'return;',
              '}',
              'f.target="_self";',
              'stato="FIN";',
              't0=Date.now();',
              'try{w.ricerca_valori();}catch(e1){',
                'try{f.submit();}catch(e2){fallisci("invio del form non riuscito");}',
              '}',
              'return;',
            '}',
          '}',
        '}else if(stato==="FIN"){',
          'if(path.indexOf("stampa_movimentazioni_fin")!==-1){',
            'var gz=null,ls=d.querySelectorAll("a[onclick]");',
            'for(var i=0;i<ls.length;i++){',
              'var m=/\\/[a-z]+\\/tmp_files\\/[^"\')\\s]+\\.gz/.exec(ls[i].getAttribute("onclick")||"");',
              'if(m){',
                'gz=m[0];',
                'break;',
              '}',
            '}',
            'if(!gz){',
              'esiti.push("\\u26a0\\ufe0f "+SPECIE[idx].nome+": nessun file .gz trovato (forse zero movimentazioni nel periodo)");',
              'prossima();',
              'return;',
            '}',
            'gzPath=gz;',
            'stato="DL";',
            't0=Date.now();',
            'return;',
          '}',
        '}else if(stato==="DL"){',
          'if(!dlAvviato){',
            'dlAvviato=true;',
            'scarica(d);',
          '}',
        '}',
      '}',
      'if(Date.now()-t0>LIMITI[stato]){',
        'fallisci("tempo scaduto (fase "+stato+")");',
      '}',
    '}',
    'timer=setInterval(tick,800);'
  )

  paste0(
    'javascript:(function(){',
      js_guardie,
      js_input,
      js_helper,
      js_stati,
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

  # Pulsante "doppia specie": chiede date e filtro, poi scarica da solo i .gz
  # di entrambe le specie in una finestra dedicata (vedi
  # .vetinfo_bookmarklet_doppio per il funzionamento)
  id_doppio <- paste0(id_prefix, "_doppio")
  riga_doppio <- div(
    class = "mb-3",
    tags$strong("Download automatico doppia specie"),
    div(
      class = "d-flex gap-3 align-items-center flex-wrap mt-1",
      span(
        tags$a(
          href  = .vetinfo_bookmarklet_doppio(),
          title = "Trascina nella barra dei preferiti del browser",
          class = "btn btn-sm btn-warning",
          icon("wand-magic-sparkles"), " Bovini + Ovicaprini (2 file .gz)"
        ),
        " ",
        copia_link(id_doppio)
      )
    ),
    div(tags$small(
      class = "text-muted",
      "Chiede intervallo di date e filtro, poi scarica da solo i file .gz di ",
      "entrambe le specie in una finestra dedicata. Consentire i popup per ",
      "vetinfo.it e non chiudere la scheda di partenza durante l'esecuzione."
    ))
  )

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
      tags$p(
        class = "mt-2 mb-1",
        tags$small(
          tags$strong("Download automatico doppia specie: "),
          copia_link(id_doppio)
        )
      ),
      tags$textarea(
        id       = id_doppio,
        class    = "form-control font-monospace",
        style    = "font-size:0.65em; height:80px;",
        readonly = NA,
        .vetinfo_bookmarklet_doppio()
      ),
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

  tagList(riga_doppio, righe, dettagli, script)
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
