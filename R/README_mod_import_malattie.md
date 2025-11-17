# Modulo mod_import_malattie

## Descrizione
Il modulo `mod_import_malattie` è responsabile dell'importazione e gestione dei dati relativi alle malattie animali per provincia e comune. Questo modulo carica i file Excel presenti nella cartella `data_static/malattie` e prepara una struttura dati pronta per il merge con le importazioni delle movimentazioni animali.

## Funzionamento

### Input
- **id**: Identificativo del modulo Shiny
- **gruppo**: Gruppo di specie (reattivo) - es. "bovini", "ovicaprini"

### Processo
1. **Caricamento dati geografici**: Carica le tabelle di riferimento da `data_static/geo/`:
   - `df_prefissi_stab.csv` - Prefissi codici stabilimento
   - `df_stati_iso3166.csv` - Stati esteri
   - `df_regioni.csv` - Regioni italiane
   - `df_province.csv` - Province italiane
   - `df_comuni.csv` - Comuni italiani

2. **Caricamento file malattie**: Cerca tutti i file `.xlsx` in `data_static/malattie/`

3. **Lettura metadati**: Per ogni file malattia, legge il foglio "metadati" contenente:
   - `campo`: Nome del campo da creare (es. "BRC_TBC", "LSD")
   - `malattia`: Nome della malattia
   - `specie`: Gruppo di specie interessato (es. "bovini")
   - `riferimento`: Riferimento normativo
   - `data_inizio`: Data inizio validità
   - `data_fine`: Data fine validità

4. **Filtraggio**: Mantiene solo le malattie valide alla data odierna

5. **Elaborazione dati**: Supporta due tipi di file malattia:
   - **province_indenni**: File con fogli "province" e "metadati"
     - Marca le province indenni dalla malattia
   - **blocchi**: File con fogli "regioni", "province", "comuni" e "metadati"
     - Gestisce blocchi a livello gerarchico (regione → provincia → comune)
     - Logica: se una regione/provincia/comune è bloccata, tutti i livelli sottostanti lo sono

### Output
Il modulo restituisce una **lista reattiva** con la seguente struttura:

```r
malattie <- list(
  metadati = <dataframe con tutti i metadati>,
  bovini = list(
    province = <dataframe province con campi malattia>,
    comuni = <dataframe comuni con campi malattia>
  ),
  ovicaprini = list(
    province = <dataframe province con campi malattia>,
    comuni = <dataframe comuni con campi malattia>
  ),
  # ... altri gruppi di specie
)
```

#### Struttura dettagliata:

**`malattie$metadati`**:
```
campo | malattia | specie | riferimento | data_inizio | data_fine | file
------|----------|--------|-------------|-------------|-----------|------
...   | ...      | ...    | ...         | ...         | ...       | ...
```

**`malattie$<specie>$province`**:
```
COD_UTS | COD_REG | <campo_malattia_1> | <campo_malattia_2> | ...
--------|---------|-------------------|-------------------|----
001     | 01      | TRUE              | FALSE             | ...
002     | 01      | TRUE              | TRUE              | ...
...
```

**`malattie$<specie>$comuni`**:
```
PRO_COM_T | COD_UTS | COD_REG | <campo_malattia_1> | <campo_malattia_2> | ...
----------|---------|---------|-------------------|-------------------|----
001001    | 001     | 01      | TRUE              | FALSE             | ...
001002    | 001     | 01      | TRUE              | TRUE              | ...
...
```

## Utilizzo nell'applicazione
Il modulo viene chiamato in `app_server.R`:

```r
st_import <- mod_import_malattie("df_standard", gruppo)
```

Dove:
- `"df_standard"`: ID del modulo
- `gruppo`: Valore reattivo contenente il gruppo di specie determinato dal file caricato

La lista risultante può essere usata per:
1. Fare merge con i dati delle movimentazioni
2. Determinare se un'importazione proviene da zona indenne o non indenne
3. Applicare le regole di gestione del rischio sanitario

## Correzioni apportate
1. **Correzione variabile non definita**: Sostituito `metadati$malattia` e `metadati$specie` con `df_meta_malattie$malattia[i]` e `df_meta_malattie$specie[i]` (righe 263-265, 309-311)
2. **Correzione variabile non definita**: Sostituito `df_province_malattie` con `malattie[[df_meta_malattie$specie[i]]][["province"]]` (riga 304)

## Note tecniche
- I valori booleani nei campi malattia indicano se la zona è **indenne** (TRUE) o **non indenne** (FALSE)
- Per i file di tipo "blocchi", la logica è invertita internamente: blocco=TRUE diventa indenne=FALSE
- La validazione controlla che non ci siano valori NA nei campi calcolati
- La validazione controlla che non ci siano duplicati nelle combinazioni malattia-specie
