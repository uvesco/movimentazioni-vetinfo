# Guida al Testing del Sistema di Verifica

## Prerequisiti

- R (versione >= 4.0)
- RStudio (opzionale ma raccomandato)
- Pacchetti R richiesti:
  - shiny
  - readxl
  - dplyr
  - bslib
  - bsicons
  - DT

## Installazione Dipendenze

```r
install.packages(c("shiny", "readxl", "dplyr", "bslib", "bsicons", "DT"))
```

## Test 1: Verifica Sintassi

### Obiettivo
Verificare che non ci siano errori di sintassi nel codice R.

### Procedura

```r
# In R console
setwd("/path/to/movimentazioni-vetinfo")

# Test caricamento moduli
source("R/data_dictionary.R")
source("R/mod_upload_movimentazioni.R")
source("R/mod_import_malattie.R")
source("R/mod_verification.R")

# Se non ci sono errori, la sintassi è corretta
message("✓ Tutti i moduli caricati correttamente")
```

### Risultato Atteso
Nessun errore di sintassi.

## Test 2: Test Funzione normalize_text()

### Obiettivo
Verificare che la funzione di normalizzazione gestisca correttamente vari casi edge.

### Procedura

```r
# Definisci la funzione di test
normalize_text <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- toupper(x)
    x <- gsub("\\s+", " ", x)
    x <- gsub("[\\x00-\\x1F\\x7F]", "", x)
    return(x)
}

# Test cases
test_cases <- list(
    list(input = "  ACQUISTATO DA ALL. ITALIANO  ", expected = "ACQUISTATO DA ALL. ITALIANO"),
    list(input = "ACQUISTATO DA  PAESI  UE", expected = "ACQUISTATO DA PAESI UE"),
    list(input = "acquistato da all. italiano", expected = "ACQUISTATO DA ALL. ITALIANO"),
    list(input = "\tACQUISTATO\tDA\tPAESI\tUE", expected = "ACQUISTATO DA PAESI UE"),
    list(input = "ACQUISTATO DA\nPAESI UE", expected = "ACQUISTATO DA PAESI UE")
)

# Esegui i test
all_passed <- TRUE
for (i in seq_along(test_cases)) {
    result <- normalize_text(test_cases[[i]]$input)
    expected <- test_cases[[i]]$expected
    
    if (result == expected) {
        message("✓ Test ", i, " PASSED")
    } else {
        message("✗ Test ", i, " FAILED")
        message("  Input: '", test_cases[[i]]$input, "'")
        message("  Expected: '", expected, "'")
        message("  Got: '", result, "'")
        all_passed <- FALSE
    }
}

if (all_passed) {
    message("\n✓ Tutti i test di normalizzazione sono passati!")
} else {
    message("\n✗ Alcuni test di normalizzazione sono falliti")
}
```

### Risultato Atteso
Tutti i test devono passare.

## Test 3: Test Caricamento Dati Statici

### Obiettivo
Verificare che i dati statici vengano caricati correttamente.

### Procedura

```r
# Carica global.R
source("global.R")

# Verifica STATIC_MOTIVI_INGRESSO
if (exists("STATIC_MOTIVI_INGRESSO")) {
    message("✓ STATIC_MOTIVI_INGRESSO caricato")
    message("  Righe: ", nrow(STATIC_MOTIVI_INGRESSO))
    message("  Colonne: ", paste(colnames(STATIC_MOTIVI_INGRESSO), collapse = ", "))
    
    # Verifica presenza colonne richieste
    required_cols <- c("Descrizione", "prov_italia")
    if (all(required_cols %in% colnames(STATIC_MOTIVI_INGRESSO))) {
        message("✓ Tutte le colonne richieste sono presenti")
    } else {
        message("✗ Mancano alcune colonne richieste")
    }
    
    # Mostra alcuni esempi
    message("\nEsempi di motivi:")
    print(head(STATIC_MOTIVI_INGRESSO[, c("Descrizione", "prov_italia")], 10))
} else {
    message("✗ STATIC_MOTIVI_INGRESSO non caricato")
}
```

### Risultato Atteso
- STATIC_MOTIVI_INGRESSO caricato con almeno 10 righe
- Colonne "Descrizione" e "prov_italia" presenti

## Test 4: Test Avvio Applicazione

### Obiettivo
Verificare che l'applicazione Shiny si avvii senza errori.

### Procedura

```r
# Avvia l'app in modalità test
library(shiny)
setwd("/path/to/movimentazioni-vetinfo")

# Prova ad avviare l'app
runApp(launch.browser = TRUE)
```

### Risultato Atteso
- L'applicazione si avvia senza errori
- L'interfaccia mostra:
  - Scheda "Input" con caricamento file
  - Sezione "Sistema di Verifica e Debug" nella scheda Input

## Test 5: Test Caricamento File (Manuale)

### Obiettivo
Verificare il flusso completo di caricamento e analisi di un file.

### Procedura

1. Avvia l'applicazione: `runApp()`
2. Carica un file di test (.xls o .gz) nella scheda "Input"
3. Osserva la console R per i messaggi di debug:
   - `[DEBUG] Valori unici in ingresso_motivo (file): X`
   - `[DEBUG] Valori unici in motivi_lookup (riferimento): Y`
   - `[OK]` se tutti matchano o `[WARNING]` se ci sono problemi
4. Nella UI, verifica che mostri:
   - "File importato correttamente per il gruppo bovini/ovicaprini"
   - Statistiche con numeri sensati
5. Scorri verso il basso e clicca su "Genera Report di Verifica"
6. Verifica il report mostrato

### Risultato Atteso

**Caso Successo:**
- Console mostra `[OK] Tutti i valori di ingresso_motivo sono riconosciuti!`
- Console mostra `[DEBUG] Animali con prov_italia assegnato: X / X` (dove X = totale)
- Report mostra:
  - ✓ Tutti i motivi riconosciuti
  - Numero di animali da Italia > 0
  - Numero di partite da Italia > 0

**Caso Problema:**
- Console mostra `[WARNING] Trovati N valori di ingresso_motivo che NON corrispondono`
- Lista dei valori problematici
- Report mostra:
  - ⚠ Valori non riconosciuti con frequenze
  - Campo prov_italia = NA per alcuni/tutti gli animali

## Test 6: Test Modulo Verification (Unit Test)

### Obiettivo
Testare le funzioni del modulo di verifica isolatamente.

### Procedura

```r
# Carica i moduli necessari
source("global.R")
source("R/mod_verification.R")

# Crea dati di test
df_test <- data.frame(
    ingresso_motivo = c(
        "ACQUISTATO DA ALL. ITALIANO",
        "ACQUISTATO DA ALL. ITALIANO",
        "ACQUISTATO DA PAESI UE",
        "VALORE NON VALIDO",
        NA
    ),
    nascita_stato = c("IT", "IT", "FR", "IT", "DE"),
    stringsAsFactors = FALSE
)

# Aggiungi il campo normalizzato (come farebbe mod_upload_movimentazioni)
normalize_text <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- toupper(x)
    x <- gsub("\\s+", " ", x)
    x <- gsub("[\\x00-\\x1F\\x7F]", "", x)
    return(x)
}

df_test$ingresso_motivo_norm <- normalize_text(df_test$ingresso_motivo)

# Simula il merge con STATIC_MOTIVI_INGRESSO
motivi_lookup <- STATIC_MOTIVI_INGRESSO[, c("Descrizione", "prov_italia")]
motivi_lookup$Descrizione_norm <- normalize_text(motivi_lookup$Descrizione)

df_test <- merge(df_test, 
                motivi_lookup[, c("Descrizione_norm", "prov_italia")], 
                by.x = "ingresso_motivo_norm", 
                by.y = "Descrizione_norm", 
                all.x = TRUE, 
                sort = FALSE)

# Test la funzione analyze_motivi_matching (se disponibile come funzione standalone)
# Nota: questa è parte del modulo server, quindi potrebbe richiedere refactoring

message("Dataset di test creato:")
print(df_test)

# Verifica manualmente
n_matched <- sum(!is.na(df_test$prov_italia))
n_unmatched <- sum(is.na(df_test$prov_italia) & !is.na(df_test$ingresso_motivo))
n_na <- sum(is.na(df_test$ingresso_motivo))

message("\nRisultati:")
message("Matched: ", n_matched, " (atteso: 3)")
message("Unmatched: ", n_unmatched, " (atteso: 1)")
message("NA: ", n_na, " (atteso: 1)")

if (n_matched == 3 && n_unmatched == 1 && n_na == 1) {
    message("✓ Test PASSED")
} else {
    message("✗ Test FAILED")
}
```

### Risultato Atteso
- Matched: 3
- Unmatched: 1
- NA: 1

## Test 7: Verifica Messaggi Console durante Caricamento

### Obiettivo
Verificare che i messaggi di debug siano informativi e corretti.

### Procedura

1. Apri RStudio o R console
2. Avvia l'app: `runApp()`
3. Carica un file di test
4. Copia i messaggi dalla console R
5. Verifica che contengano:
   - `[DEBUG] Valori unici in ingresso_motivo (file): ...`
   - `[DEBUG] Valori unici in motivi_lookup (riferimento): ...`
   - `[OK]` o `[WARNING]` appropriati
   - `[DEBUG] Animali con prov_italia assegnato: ... / ...`

### Risultato Atteso
Messaggi chiari e informativi che aiutano a diagnosticare problemi.

## Risoluzione Problemi Comuni

### Problema: Errore "could not find function X"

**Causa**: Pacchetto non caricato o funzione non definita.

**Soluzione**:
```r
# Verifica pacchetti installati
installed.packages()[, "Package"]

# Installa pacchetti mancanti
install.packages("nome_pacchetto")
```

### Problema: Errore "object X not found"

**Causa**: Variabile globale non caricata.

**Soluzione**:
```r
# Ricarica global.R
source("global.R")
```

### Problema: Warning "closing unused connection"

**Causa**: Connessioni file non chiuse (file .gz).

**Soluzione**: Ignorare, è gestito automaticamente dal codice.

### Problema: Tutti i motivi risultano non matchati

**Causa**: File `motivi_ingresso.csv` non caricato o formato errato.

**Soluzione**:
```r
# Verifica il file
STATIC_MOTIVI_INGRESSO <- read.csv(
    "data_static/decodifiche/motivi_ingresso.csv",
    stringsAsFactors = FALSE
)
print(STATIC_MOTIVI_INGRESSO)
```

## Checklist Test Finale

Prima di considerare il testing completo, verificare:

- [ ] Nessun errore di sintassi nel caricamento dei moduli
- [ ] Funzione `normalize_text()` passa tutti i test
- [ ] Dati statici caricati correttamente
- [ ] App Shiny si avvia senza errori
- [ ] Modulo di verifica visibile nell'interfaccia
- [ ] Caricamento file funziona
- [ ] Messaggi di debug appaiono in console
- [ ] Report di verifica genera output corretto
- [ ] Statistiche mostrano valori sensati
- [ ] Documentazione chiara e completa

## Note

- Questi test richiedono un ambiente R funzionante
- Alcuni test potrebbero richiedere file di esempio reali
- I test manuali richiedono ispezione visiva dell'output
- Per test automatizzati più completi, considerare l'uso di `testthat` package

---

**Versione**: 1.0  
**Data**: 2025-11-19  
**Tipo**: Guida per testing manuale e automatizzato
