# Guida al Sistema di Verifica e Debug

## Panoramica

Il sistema di verifica e debug è stato implementato per risolvere problemi di riconoscimento dell'origine italiana degli animali e fornire uno strumento di diagnostica completo per tutte le fasi di elaborazione dei dati.

## Il Problema Originale

Nel sistema venivano riportati:
- **Numero di animali importati**: 1057
- **Numero di animali movimentati dall'Italia**: 0 ❌ (dovrebbe essere > 0)
- **Numero di partite movimentate dall'Italia**: 0 ❌ (dovrebbe essere > 0)

La causa principale era che il merge tra `ingresso_motivo` (dal file caricato) e `Descrizione` (da `STATIC_MOTIVI_INGRESSO`) non funzionava correttamente, impedendo l'assegnazione del campo `prov_italia`.

## Soluzione Implementata

### 1. Modulo di Verifica (`R/mod_verification.R`)

Un nuovo modulo Shiny che fornisce un report diagnostico dettagliato.

#### Funzionalità:

1. **Analisi Motivi di Ingresso**
   - Numero totale di animali
   - Valori unici in `ingresso_motivo`
   - Animali con motivo riconosciuto vs non riconosciuto
   - Lista completa dei valori NON riconosciuti con frequenze
   - Stato del campo `prov_italia` derivato

2. **Analisi Partite**
   - Numero totale di partite
   - Partite da Italia vs Estero
   - Partite senza informazione

3. **Analisi Luogo di Nascita**
   - Animali nati in Italia vs Estero
   - Derivato dal campo `capo_identificativo`

4. **Riferimento Motivi Disponibili**
   - Lista completa dei motivi validi nel sistema

### 2. Normalizzazione Migliorata del Testo

Nel file `R/mod_upload_movimentazioni.R`, è stata implementata una funzione `normalize_text()` più robusta:

```r
normalize_text <- function(x) {
    # Converti in carattere
    x <- as.character(x)
    # Rimuovi spazi iniziali e finali
    x <- trimws(x)
    # Converti in maiuscolo
    x <- toupper(x)
    # Sostituisci spazi multipli con uno singolo
    x <- gsub("\\s+", " ", x)
    # Rimuovi eventuali caratteri di controllo invisibili
    x <- gsub("[\\x00-\\x1F\\x7F]", "", x)
    return(x)
}
```

Questa funzione gestisce:
- ✅ Spazi multipli consecutivi
- ✅ Caratteri di controllo invisibili (es. tab, newline)
- ✅ Spazi iniziali e finali
- ✅ Differenze maiuscole/minuscole

## Come Usare il Sistema di Verifica

### Passo 1: Caricare un File

1. Avvia l'applicazione Shiny
2. Nella scheda "Input", carica un file `.xls` o `.gz` di movimentazioni

### Passo 2: Generare il Report

1. Scorri verso il basso nella scheda "Input"
2. Trova la sezione "Sistema di Verifica e Debug"
3. Clicca su **"Genera Report di Verifica"**

### Passo 3: Analizzare i Risultati

Il report mostrerà diverse sezioni con codici colore:

- 🟢 **Verde**: Tutto OK
- 🟡 **Giallo**: Attenzione, possibile problema
- 🔴 **Rosso**: Problema critico

#### Esempio di Output Positivo:

```
1. Analisi Motivi di Ingresso
✓ Animali con motivo riconosciuto: 1057 (100%) ✓
✓ Animali con motivo NON riconosciuto: 0 (0%) ✓
✓ Tutti i motivi di ingresso sono stati riconosciuti correttamente!

Campo 'prov_italia' (derivato dal merge):
- Animali da Italia (prov_italia = TRUE): 328
- Animali da Estero (prov_italia = FALSE): 729
```

#### Esempio di Output con Problemi:

```
1. Analisi Motivi di Ingresso
⚠ Animali con motivo riconosciuto: 0 (0%) ⚠
⚠ Animali con motivo NON riconosciuto: 1057 (100%) ⚠ ATTENZIONE!

⚠ Valori NON riconosciuti trovati!
I seguenti valori di 'ingresso_motivo' non corrispondono a nessuna voce:
  "ACQUISTATO DA PAESI  UE" (500 occorrenze)  ← spazio doppio!
  "ACQUISTATO DA ALL.  ITALIANO" (200 occorrenze)

Possibili cause:
- Differenze negli spazi (iniziali, finali o multipli)
- Caratteri speciali o problemi di encoding
```

### Passo 4: Risolvere i Problemi

Se il report mostra valori non riconosciuti:

1. **Verifica spazi multipli**: Il sistema ora dovrebbe gestirli automaticamente
2. **Verifica il file di riferimento**: Controlla `data_static/decodifiche/motivi_ingresso.csv`
3. **Aggiungi nuovi motivi**: Se necessario, aggiungi le voci mancanti al file CSV
4. **Ricarica il file**: Dopo modifiche ai file di riferimento, riavvia l'app

## Struttura dei File di Riferimento

### `data_static/decodifiche/motivi_ingresso.csv`

```csv
"Codice","Descrizione","prov_italia"
"M","ACQUISTATO DA ALL. ITALIANO",TRUE
"C","ACQUISTATO DA PAESI UE CON CEDOLA",FALSE
...
```

- **Codice**: Codice breve del motivo
- **Descrizione**: Testo completo (deve corrispondere a quello nel file XLS)
- **prov_italia**: TRUE se l'origine è italiana, FALSE se estera

## Flusso di Elaborazione Dati

```
1. File XLS/GZ caricato
   ↓
2. Lettura e conversione in dataframe
   ↓
3. Standardizzazione colonne (mod_upload_movimentazioni.R)
   ↓
4. Normalizzazione testo con normalize_text()
   ↓
5. Merge con STATIC_MOTIVI_INGRESSO
   ├─→ Match trovato: campo prov_italia assegnato ✓
   └─→ Match NON trovato: prov_italia = NA ✗
   ↓
6. Arricchimento con dati geografici
   ↓
7. Creazione partite (aggregazione)
   ↓
8. Visualizzazione statistiche e report
```

## Diagnostica Avanzata

### Verificare i Dati Grezzi

Se il problema persiste, puoi ispezionare manualmente i dati:

```r
# In R console
# 1. Carica i motivi di riferimento
motivi <- read.csv("data_static/decodifiche/motivi_ingresso.csv", stringsAsFactors = FALSE)
print(motivi$Descrizione)

# 2. Carica il file XLS e ispeziona i motivi
library(readxl)
df <- read_excel("path/to/file.xls")
unique(df$`MOTIVO INGRESSO`)  # per bovini
```

### Verificare Caratteri Invisibili

```r
# Trova caratteri non ASCII
motivo <- "ACQUISTATO DA ALL. ITALIANO"
charToRaw(motivo)  # mostra i byte

# Controlla encoding
Encoding(motivo)
```

## Aggiornamenti Futuri

Possibili miglioramenti:

1. ✅ **Normalizzazione robusta** - Implementata
2. ✅ **Report diagnostico** - Implementato
3. 🔲 Export del report in PDF/Excel
4. 🔲 Suggerimenti automatici per correzioni
5. 🔲 Modalità di apprendimento automatico per nuovi motivi

## Supporto

Per problemi o domande:
1. Consulta questo documento
2. Genera un report di verifica
3. Controlla i file di log dell'applicazione Shiny
4. Crea un'issue su GitHub con il report di verifica allegato

---

**Versione**: 1.0  
**Data**: 2025-11-19  
**Autore**: Sistema di debug automatizzato
