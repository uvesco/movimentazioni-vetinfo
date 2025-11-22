# Pipeline di Controlli sulle Movimentazioni

## Panoramica

Questo modulo implementa una pipeline completa di controlli sulle movimentazioni di animali, con incrocio tra dati animali, malattie e codifiche territoriali.

## Funzionalità Implementate

### 1. Gestione Italia/Estero

La funzione `classifica_origine()` distingue animali provenienti da Italia vs Estero basandosi su:
- Campo `ingresso_motivo` incrociato con la tabella `motivi_ingresso.csv`
- Colonna `prov_italia` nella tabella dei motivi (TRUE = Italia, FALSE = Estero)
- Restituisce un campo `origine` con valori "italia" o "estero"

**File:** `R/utils_pipeline.R`

### 2. Conversione Booleana Malattie

Tutti i dataset delle malattie sono convertiti in campi boolean TRUE/FALSE senza NA:
- TRUE = zona indenne (disease-free)
- FALSE = zona non indenne
- NA vengono convertiti a TRUE (se non specificato, si assume indenne)

**Modifiche in:** `R/mod_import_malattie.R`
- Conversione per tipo "province_indenni"
- Conversione per tipo "blocchi"
- Gestione NA post-merge

### 3. Merge Malattie sulla Provenienza

Le malattie sono collegate alla provenienza degli animali tramite:
- Codice ISTAT del comune (`PRO_COM_T`)
- Estrazione del comune da `orig_stabilimento_cod` tramite tabella `df_stab`
- Tutte le colonne malattie hanno prefisso `prov_`

**Funzione:** `merge_malattie_con_prefisso()` in `R/utils_pipeline.R`

### 4. Merge Malattie sulla Nascita

Le malattie sono collegate alla provincia di nascita tramite:
- Codice UTS della provincia estratto dal marchio auricolare
- Pattern: IT + 3 cifre (es. IT001... → provincia 001)
- Tutte le colonne malattie hanno prefisso `nascita_`

**Funzione:** `estrai_provincia_nascita()` in `R/utils_pipeline.R`

### 5. Validazione Comune di Provenienza

Per animali italiani con comune di provenienza non valido:
- Vettore `casi_provenienza_non_trovati`: IDs degli animali
- DataFrame `df_provenienza_non_trovati`: dati completi
- Download Excel disponibile
- Visualizzazione in tab "Controllo Manuale"

**Implementato in:** `R/mod_pipeline_controlli.R` e `R/app_server.R`

### 6. Validazione Provincia di Nascita

Per animali italiani con provincia di nascita non valida:
- Vettore `casi_nascita_non_trovati`: IDs degli animali
- DataFrame `df_nascita_non_trovati`: dati completi
- Download Excel disponibile
- Visualizzazione in tab "Controllo Manuale"

**Implementato in:** `R/mod_pipeline_controlli.R` e `R/app_server.R`

### 7. Tab "Provenienze"

Tab dedicato che mostra animali provenienti da comuni non indenni:
- Una tabella per ogni malattia
- Titolo con nome malattia
- Tabella filtrata con animali da zone non indenni
- Pulsante download Excel per ogni malattia
- Messaggio se nessun caso: "Nessuna movimentazione proveniente da zone non indenni per le malattie considerate"

**UI:** `R/app_ui.R`
**Logic:** `R/app_server.R` - output `ui_provenienze`

### 8. Tab "Nascite"

Tab dedicato che mostra animali nati in province non indenni:
- Una tabella per ogni malattia
- Titolo con nome malattia
- Tabella filtrata con animali nati in zone non indenni
- Pulsante download Excel per ogni malattia
- Messaggio se nessun caso: "Nessuna movimentazione di animali nati in zone non indenni per le malattie considerate"

**UI:** `R/app_ui.R`
**Logic:** `R/app_server.R` - output `ui_nascite`

## Struttura File

### Nuovi File

1. **R/utils_pipeline.R**
   - `classifica_origine()`: Classificazione Italia/Estero
   - `estrai_provincia_nascita()`: Estrazione provincia da marchio auricolare
   - `estrai_comune_provenienza()`: Estrazione comune da codice stabilimento
   - `merge_malattie_con_prefisso()`: Merge malattie con prefisso
   - `crea_dataframe_validazione()`: Creazione dataframe validazione
   - `filtra_animali_non_indenni()`: Filtro animali da zone non indenni

2. **R/mod_pipeline_controlli.R**
   - Modulo Shiny principale che orchestra tutta la pipeline
   - Gestisce tutti i merge e le validazioni
   - Espone valori reattivi per l'interfaccia

3. **tests/manual_test_pipeline.R**
   - Script di test manuale per validare le funzioni utility

### File Modificati

1. **R/app_ui.R**
   - Aggiunti 3 nuovi tab: "Controllo Manuale", "Provenienze", "Nascite"

2. **R/app_server.R**
   - Integrazione modulo pipeline
   - Output renderer per tutte le nuove tabelle
   - Download handler per tutti i file Excel

3. **R/mod_import_malattie.R**
   - Conversione booleana con gestione NA per "province_indenni"
   - Conversione booleana con gestione NA per "blocchi"

4. **global.R**
   - Source dei nuovi file utilities

5. **app.R**
   - Aggiunta libreria `openxlsx`

## Flusso Dati

```
Upload File Movimentazioni
         ↓
Standardizzazione (mod_upload_movimentazioni)
         ↓
Import Malattie (mod_import_malattie)
         ↓
Pipeline Controlli (mod_pipeline_controlli)
    ↓         ↓         ↓
Classifica  Estrai    Estrai
Origine     Provincia Comune
    ↓         ↓         ↓
Merge Malattie (prov_ e nascita_)
         ↓
Validazioni e Filtri
         ↓
Output Tabs: Controllo Manuale, Provenienze, Nascite
```

## Testing

Eseguire lo script di test manuale:
```r
source("tests/manual_test_pipeline.R")
```

Per test completi con l'app Shiny:
1. Avviare l'applicazione: `shiny::runApp()`
2. Caricare un file di movimentazioni (bovini o ovicaprini)
3. Verificare i 3 nuovi tab
4. Testare i download Excel

## Note Tecniche

### Marchio Auricolare
- Format italiano: IT + 12 cifre
- Prime 3 cifre dopo IT = codice provincia
- Mapping diretto a COD_UTS nella maggior parte dei casi

### Codice Stabilimento
- Format: XXX + Sigla Provincia (es. 001TO)
- Mappato a PRO_COM_T tramite tabella `df_stab`

### Malattie
- Logica: TRUE = indenne, FALSE = non indenne
- NA post-import vengono convertiti a TRUE
- Due tipi di file supportati: "province_indenni" e "blocchi"

## Dipendenze

- shiny
- dplyr
- DT
- openxlsx
- readxl
- bsicons

## Autore

Implementato per uvesco/movimentazioni-vetinfo
