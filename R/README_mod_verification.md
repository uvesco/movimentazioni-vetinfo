# Modulo di Verifica e Debug

## Descrizione

Il modulo `mod_verification.R` fornisce un sistema completo di verifica e debug per tracciare i dati durante tutte le fasi dell'elaborazione delle movimentazioni.

## Funzionalità Principali

### 1. Analisi Matching Motivi di Ingresso

Verifica che i valori nel campo `ingresso_motivo` corrispondano alle voci in `STATIC_MOTIVI_INGRESSO`:

- Conta animali con motivi riconosciuti vs non riconosciuti
- Mostra valori non matchati con relative frequenze
- Identifica problemi di spazi, encoding o valori mancanti
- Verifica presenza e correttezza del campo `prov_italia`

### 2. Analisi Partite

Fornisce statistiche sulle partite (aggregazioni di animali):

- Numero totale di partite
- Partite da Italia vs Estero
- Partite senza informazione sull'origine

### 3. Analisi Nascita

Statistiche sul luogo di nascita degli animali:

- Animali nati in Italia (nascita_stato == "IT")
- Animali nati all'estero
- Animali senza informazione

### 4. Riferimento Motivi Disponibili

Lista completa dei motivi di ingresso validi nel sistema per confronto manuale.

## Struttura del Modulo

### UI Function

```r
mod_verification_ui(id)
```

**Parametri:**
- `id`: Namespace ID del modulo

**Output:**
- Card Bootstrap con pulsante per generare il report
- Area per mostrare i risultati dell'analisi

### Server Function

```r
mod_verification_server(id, animali_reactive, gruppo_reactive, partite_reactive)
```

**Parametri:**
- `id`: Namespace ID del modulo
- `animali_reactive`: Reactive expression che ritorna il dataframe degli animali
- `gruppo_reactive`: Reactive expression che ritorna il gruppo di specie
- `partite_reactive`: Reactive expression che ritorna il dataframe delle partite

**Output:**
- Lista con:
  - `report`: Reactive con i dati del report

## Funzioni Interne

### `analyze_motivi_matching(df)`

Analizza il matching tra `ingresso_motivo` e `STATIC_MOTIVI_INGRESSO`.

**Input:**
- `df`: Dataframe degli animali

**Output:**
- Lista con:
  - `status`: "success", "empty", o "error"
  - `n_totale`: Numero totale animali
  - `n_matched`: Animali con motivo riconosciuto
  - `n_unmatched`: Animali con motivo non riconosciuto
  - `matched_values`: Vettore valori matchati
  - `unmatched_values`: Vettore valori non matchati
  - `freq_unmatched`: Tabella frequenze valori non matchati
  - `n_prov_italia_true/false/na`: Statistiche campo prov_italia

### `analyze_partite(df_partite)`

Analizza le statistiche delle partite.

**Input:**
- `df_partite`: Dataframe delle partite

**Output:**
- Lista con:
  - `status`: "success", "empty", o "error"
  - `n_partite_totale`: Numero totale partite
  - `n_partite_italia/estero/na`: Statistiche per origine

## Integrazione nell'App

### 1. In `app_ui.R`

```r
mainPanel(
  # ... altri output ...
  
  # Modulo di verifica e debug
  hr(),
  mod_verification_ui("verification")
)
```

### 2. In `app_server.R`

```r
# Modulo di verifica e debug
verification <- mod_verification_server(
  "verification", 
  animali,    # reactive degli animali
  gruppo,     # reactive del gruppo
  partite     # reactive delle partite
)
```

### 3. In `global.R`

```r
source("R/mod_verification.R")
```

## Esempio di Utilizzo

```r
# Nell'app Shiny, dopo aver caricato un file:
# 1. L'utente clicca su "Genera Report di Verifica"
# 2. Il modulo analizza i dati
# 3. Mostra un report dettagliato con:
#    - Statistiche di matching
#    - Valori problematici evidenziati
#    - Suggerimenti per la risoluzione
```

## Output Report

Il report generato include sezioni con codici colore:

### Successo (Verde)
```
✓ Tutti i motivi di ingresso sono stati riconosciuti correttamente!
```

### Avviso (Giallo)
```
⚠ Animali senza informazione (prov_italia = NA): 100
   Questi animali non hanno origine determinata!
```

### Errore (Rosso)
```
⚠ PROBLEMA: ci sono motivi riconosciuti ma nessuno marcato come Italia!
```

## Note Tecniche

### Normalizzazione del Testo

Il modulo utilizza la stessa logica di normalizzazione di `mod_upload_movimentazioni.R`:

```r
# Standardizzazione
motivi_ref$Descrizione_std <- trimws(toupper(motivi_ref$Descrizione))
df$ingresso_motivo_std <- trimws(toupper(df$ingresso_motivo))
```

Nota: La normalizzazione avanzata con `normalize_text()` è applicata durante il caricamento, non nella verifica.

### Performance

- Il report è generato on-demand (clic pulsante)
- Non influisce sulle prestazioni di caricamento file
- Analisi cache per evitare ricalcoli inutili

## Troubleshooting

### Il campo `prov_italia` è sempre NA

**Causa:** Il merge non sta funzionando.

**Soluzione:**
1. Verifica che `STATIC_MOTIVI_INGRESSO` sia caricato correttamente
2. Controlla i valori in `unmatched_values` nel report
3. Verifica spazi o caratteri speciali nei dati

### Report non si genera

**Causa:** Dati non caricati o errore nei reactive.

**Soluzione:**
1. Verifica che un file sia stato caricato
2. Controlla console R per errori
3. Verifica che i reactive `animali`, `gruppo`, `partite` siano definiti

### Statistiche non corrispondono

**Causa:** Possibile problema nell'aggregazione delle partite.

**Soluzione:**
1. Verifica la funzione `enrich_animali_data()` in `mod_upload_movimentazioni.R`
2. Controlla che le colonne necessarie siano presenti

## Estensioni Future

Possibili miglioramenti:

1. Export del report in formato PDF o Excel
2. Grafici interattivi per visualizzare le statistiche
3. Confronto tra più file caricati
4. Storico dei report generati
5. Suggerimenti automatici per correggere i valori non matchati

## Dipendenze

- `shiny`: Framework base
- `bslib`: Card e componenti Bootstrap
- Nessuna dipendenza esterna aggiuntiva

## Licenza

Stesso della applicazione principale.

---

**Versione**: 1.0  
**Data**: 2025-11-19  
**Manutenzione**: Sistema integrato nell'applicazione principale
