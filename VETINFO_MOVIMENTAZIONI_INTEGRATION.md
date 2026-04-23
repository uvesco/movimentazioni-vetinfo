---
title: "VETINFO_MOVIMENTAZIONI_INTEGRATION"
output: html_document
---

# Integrazione VetInfo: Download Movimentazioni in App Shiny

## Contesto e obiettivo

L'app Shiny deve permettere all'utente di scaricare direttamente i file Excel delle
movimentazioni da VetInfo (portale del Ministero della Salute) senza uscire dall'app.
Vanno implementati due widget: uno per gli **ovicaprini** e uno per i **bovini**.

## Vincoli architetturali CRITICI (da rispettare sempre)

1. **VetInfo richiede autenticazione forte** (SPID o CIE) prima di ogni sessione.
   L'utente deve essere già loggato su vetinfo.it in un'altra scheda dello stesso browser.
   L'app NON può gestire l'autenticazione — può solo aprire le pagine nel browser.

2. **I form usano POST** (non GET): non è possibile un link URL diretto.
   La soluzione è un `<form method="POST" target="_blank">` che apre vetinfo in una nuova
   scheda mantenendo l'app Shiny attiva.

3. **Il download è in due step**:
   - Step 1: submit del form → VetInfo elabora e mostra una pagina intermedia
   - Step 2: la pagina intermedia mostra due pulsanti:
     - **"Visualizza excel"** → apre il file `.xls` direttamente nel browser
     - **"Scarica File Gzip"** → scarica il file compresso `.gz`

   L'utente deve cliccare manualmente su uno dei due pulsanti nella nuova scheda.
   **Non è possibile automatizzare il secondo step** (i link contengono token di sessione
   generati dinamicamente dal server con timestamp).

4. **Le date** devono essere passate come campi separati gg/mm/aaaa (non come stringa unica).
   Usare JavaScript inline per splittare il valore del `dateInput` di Shiny al momento del submit.

---

## SEZIONE 1 — OVICAPRINI

### Endpoint
- **Form action (step 1):** `https://www.vetinfo.it/ovicaprini/stampe/attendere.pl`
- **Pagina risultato (step 2):** `https://www.vetinfo.it/ovicaprini/stampe/stampa_movimentazioni_fin.pl`

### Parametri fissi (campi hidden pre-compilati)

| Nome campo | Valore | Note |
|---|---|---|
| `P_TIPO_RUOLO` | `ASL_ID` | Ruolo utente |
| `V_REG_CODICE_PER_DISPLAY` | `A` | Codice regione display |
| `V_REG_ID` | `` | Vuoto |
| `V_ASL_CODICE_PER_DISPLAY` | `A203` | Codice ASL TO3 |
| `P_PAGINA` | `stampa_movimentazioni_fin.pl` | Pagina di destinazione risultato |
| `P_CODICE_ASL` | `A203` | Codice ASL |
| `P_CODICE_ASL_X_FIN` | `` | Vuoto |
| `P_AMBIENTE` | `OVICAPRINI` | **Discriminante specie** |
| `titolo` | `` | Vuoto |
| `P_CODICE_REGIONE` | `` | Vuoto |
| `P_DESCRIZIONE_REGIONE` | `PIEMONTE` | |
| `P_REG_COD_INTERNO` | `A` | |
| `P_REG_ID` | `2` | ID regione Piemonte |
| `P_ASL_ID` | `544` | ID ASL TO3 |
| `P_ASL_DENOMINAZIONE` | `AZIENDA SANITARIA LOCALE TO3` | |
| `P_COM_ID` | `` | Vuoto |
| `P_COMUNE_DENOMINAZIONE` | `` | Vuoto |
| `P_GRSPE_DESCRIZIONE` | `OVINI E CAPRINI` | |
| `P_GRSPE_ID` | `2` | ID gruppo specie ovicaprini |
| `P_GRSPE_CODICE` | `` | Vuoto |
| `P_SPECIE_DESCRIZIONE` | `` | Vuoto |
| `P_SPE_ID` | `` | Vuoto |
| `P_SPECIE_CODICE` | `` | Vuoto |
| `P_TIP_PROD_CODICE` | `` | Vuoto |
| `P_TIP_PROD_DESCRIZIONE` | `` | Vuoto |
| `P_TIP_PROD_ID` | `` | Vuoto |
| `P_ORIENT_ID` | `` | Vuoto |
| `P_TIP_TIPPR_CODICE` | `` | Vuoto |
| `P_TIP_TIPPR_ID` | `` | Vuoto |
| `P_TIP_TIPPR` | `` | Vuoto |
| `P_ORIENTAMENTO_DESC` | `` | Vuoto |

### Parametri filtro (pre-impostati, non modificabili dall'utente)

| Nome campo | Valore | Significato |
|---|---|---|
| `P_DOVE` | `altre_regioni` | Solo movimentazioni verso altre regioni |
| `P_TIPO_REPORT` | `ingressi_ovini_capi_singoli` | **Movimentazioni capi singoli – in ingresso** (NON `ingressi_ovini` che è per gli insiemi) |
| `P_TIPO_STAMPA` | `EXCEL` | Formato output |

### Parametri data (compilati dinamicamente da JS)

Shiny usa `dateInput` che restituisce `yyyy-mm-dd`. Il JS deve splittare e popolare:

| Nome campo | Valore (da calcolare) |
|---|---|
| `P_DT_CONTROLLO_GG_DA` | giorno (2 cifre) della data Dal |
| `P_DT_CONTROLLO_MM_DA` | mese (2 cifre) della data Dal |
| `P_DT_CONTROLLO_AA_DA` | anno (4 cifre) della data Dal |
| `P_DT_CONTROLLO_GG_A` | giorno (2 cifre) della data Al |
| `P_DT_CONTROLLO_MM_A` | mese (2 cifre) della data Al |
| `P_DT_CONTROLLO_AA_A` | anno (4 cifre) della data Al |

---

## SEZIONE 2 — BOVINI

### Endpoint
- **Form action (step 1):** `https://www.vetinfo.it/bovini/stampe/attendere.pl`
- **Pagina risultato (step 2):** `https://www.vetinfo.it/bovini/stampe/stampa_movimentazioni_fin.pl`

### Parametri fissi (campi hidden pre-compilati)

Identici agli ovicaprini **tranne** i seguenti:

| Nome campo | Valore | Differenza rispetto a ovicaprini |
|---|---|---|
| `P_AMBIENTE` | `ANAGES` | ← DIVERSO |
| `P_GRSPE_DESCRIZIONE` | `BOVINI E BUFALINI` | ← DIVERSO |
| `P_GRSPE_ID` | `1` | ← DIVERSO |

Tutti gli altri campi fissi sono identici agli ovicaprini.

### Parametri filtro

| Nome campo | Valore | Significato |
|---|---|---|
| `P_DOVE` | `altre_regioni` | Solo movimentazioni verso altre regioni |
| `P_TIPO_REPORT` | `ingressi_bovini` | Movimentazioni bovini in ingresso |
| `P_TIPO_STAMPA` | `EXCEL` | Formato output |

### Parametri data
Identici agli ovicaprini (stessi nomi campo, stessa logica JS).

---

## Comportamento pagina risultato (step 2) — uguale per entrambi

Dopo il submit, VetInfo elabora e carica la pagina finale che mostra:

E' STATO PRODOTTO E COMPRESSO IL REPORT DI STAMPA IN FORMATO EXCEL
[Visualizza excel]                    [Scarica File Gzip]

- **"Visualizza excel"**: apre `window.open()` su un URL tipo
  `/bovini/tmp_files/{username}_{tipo_report}{data_ora}.xls`
- **"Scarica File Gzip"**: apre `window.open()` su URL analogo con estensione `.gz`

Gli URL contengono timestamp e sono quindi **non prevedibili** — l'utente deve
cliccare manualmente. Raccomandare "Visualizza excel" per uso immediato in Excel,
"Scarica File Gzip" per file grandi o archiviazione.

---

## Implementazione Shiny suggerita

### Pattern generale

Creare una funzione helper `vetinfo_download_widget(id, specie)` che restituisce:

1. Due `dateInput` Shiny ("Dal" e "Al") con default ragionevoli (es. primo giorno
   del mese corrente → oggi)
2. Un `tags$form` con `method="POST"`, `target="_blank"`, e tutti i campi hidden
3. Un `tags$script` con listener `submit` che popola i 6 campi data hidden splittando
   il valore ISO del `dateInput` Shiny
4. Un pulsante submit stilizzato differentemente per le due specie

### Struttura UI consigliata

sidebar o pannello dedicato "Download VetInfo"
├── [widget ovicaprini]  🐑 Scarica EXCEL Ovicaprini
│     dateInput Dal / Al
│     [POST form nascosto]
└── [widget bovini]      🐄 Scarica EXCEL Bovini
dateInput Dal / Al
[POST form nascosto]
Le date dei due widget possono essere **sincronizzate** (osservatore Shiny che
copia `input$ovi_dal` → `input$bov_dal`) oppure indipendenti, a scelta.

### Note per il JS nel form

Il `dateInput` di Shiny rende nel DOM un `<input type="text">` con id=`{inputId}`
e formato visivo `dd/mm/yyyy` (con `language="it"`), ma il **valore effettivo**
leggibile da JS tramite `document.getElementById(id).value` è in formato
**`yyyy-mm-dd`** (ISO, standard HTML). Splittare su `-`.

### Avvertenza da mostrare in UI

Aggiungere un `helpText` o `tags$small` vicino ai pulsanti:
> "⚠️ Accertarsi di essere già autenticati su vetinfo.it (SPID/CIE) in un'altra
> scheda del browser prima di cliccare."

---

## Istruzioni per Claude Code

### Task da eseguire

1. **Creare il file `R/vetinfo_widgets.R`** con la funzione helper
   `vetinfo_download_widget(widget_id, specie = c("ovicaprini", "bovini"))`.
   La funzione deve usare solo `shiny` e `htmltools` (già dipendenze standard).

2. **Modificare `ui.R` (o la ui in `app.R`)** per includere il pannello
   "Download VetInfo" con i due widget, usando `vetinfo_download_widget()`.

3. **Non modificare `server.R`** — il meccanismo è puramente client-side (form POST).
   Le date rimangono accessibili come `input$ovi_dal`, `input$ovi_al`,
   `input$bov_dal`, `input$bov_al` nel server se servono per altri scopi.

4. **Test**: verificare che il form faccia il submit corretto aprendo
   `https://www.vetinfo.it/ovicaprini/stampe/attendere.pl` in nuova scheda
   (con `target="_blank"`). La pagina arriverà alla schermata di selezione
   formato solo se l'utente è autenticato.

### Dipendenze R necessarie
Nessuna aggiuntiva — `shiny` e `htmltools` sono già inclusi.

### Parametri specifici ASL (da NON hardcodare, o da rendere configurabili)
Se l'app potrebbe essere usata da altre ASL in futuro, questi parametri
andrebbero estratti in costanti o configurazione:
- `P_ASL_ID = "544"` (ASL TO3)
- `P_CODICE_ASL = "A203"`
- `P_ASL_DENOMINAZIONE = "AZIENDA SANITARIA LOCALE TO3"`
- `P_REG_ID = "2"` (Piemonte)
- `P_REG_COD_INTERNO = "A"`
- `P_DESCRIZIONE_REGIONE = "PIEMONTE"`

Per ora possono essere hardcodati come costanti in cima a `vetinfo_widgets.R`.

