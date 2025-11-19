# Sistema di Verifica e Debug - Guida Rapida

## 🎯 Scopo

Questo sistema risolve il problema del riconoscimento dell'origine italiana degli animali e fornisce strumenti diagnostici completi per verificare la qualità dei dati in tutte le fasi dell'elaborazione.

## 🚀 Quick Start

### Per Utenti

1. **Avvia l'applicazione Shiny**
2. **Carica un file** di movimentazioni (.xls o .gz)
3. **Osserva la console R** per messaggi diagnostici automatici
4. **Scorri verso il basso** nella scheda "Input"
5. **Clicca su "Genera Report di Verifica"**
6. **Leggi il report** per diagnosticare eventuali problemi

### Cosa Fare se Vedi Problemi

Se il report mostra valori non riconosciuti:

```
⚠ Valori NON riconosciuti trovati!
"ACQUISTATO DA PAESI  UE" (500 occorrenze)  ← Nota gli spazi doppi!
```

**Soluzioni**:
1. ✅ Il sistema ora normalizza automaticamente gli spazi - ricarica il file
2. ✅ Verifica il file `data_static/decodifiche/motivi_ingresso.csv`
3. ✅ Aggiungi nuove voci se necessario
4. ✅ Contatta il supporto se il problema persiste

## 📚 Documentazione Completa

### Per Utenti
- **[DEBUGGING_GUIDE.md](DEBUGGING_GUIDE.md)** - Guida completa all'uso del sistema di verifica

### Per Sviluppatori
- **[R/README_mod_verification.md](R/README_mod_verification.md)** - Documentazione tecnica del modulo
- **[TESTING_GUIDE.md](TESTING_GUIDE.md)** - Guida al testing
- **[IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)** - Riepilogo implementazione

## 🔍 Funzionalità Principali

### 1. Normalizzazione Robusta del Testo
Il sistema gestisce automaticamente:
- ✅ Spazi multipli → spazio singolo
- ✅ Spazi iniziali/finali rimossi
- ✅ Maiuscole/minuscole normalizzate
- ✅ Caratteri di controllo invisibili rimossi

### 2. Diagnostica Runtime
Durante il caricamento del file, la console mostra:
```
[DEBUG] Valori unici in ingresso_motivo (file): 8
[DEBUG] Valori unici in motivi_lookup (riferimento): 16
[OK] Tutti i valori di ingresso_motivo sono riconosciuti!
[DEBUG] Animali con prov_italia assegnato: 1057 / 1057
```

### 3. Report di Verifica Interattivo
Report dettagliato che mostra:
- ✅ Statistiche di matching ingresso_motivo
- ✅ Lista valori non riconosciuti con frequenze
- ✅ Stato campo prov_italia
- ✅ Statistiche partite e nascita
- ✅ Lista completa motivi disponibili

## 🐛 Troubleshooting

### Problema: Animali da Italia = 0

**Causa**: Merge tra ingresso_motivo e motivi di riferimento fallito

**Diagnostica**:
1. Genera il report di verifica
2. Cerca sezione "Valori NON riconosciuti"
3. Verifica se ci sono valori non matchati

**Soluzioni**:
- Se vedi spazi extra: ✅ Già gestito automaticamente
- Se vedi valori sconosciuti: Aggiungi al file motivi_ingresso.csv
- Se vedi caratteri strani: Verifica encoding del file originale

### Problema: Report non si genera

**Causa**: File non caricato o errore nei dati

**Soluzioni**:
1. Verifica che il file sia stato caricato correttamente
2. Controlla i messaggi di errore nella UI
3. Verifica la console R per errori

## 📊 Struttura File di Riferimento

### motivi_ingresso.csv
```csv
"Codice","Descrizione","prov_italia"
"M","ACQUISTATO DA ALL. ITALIANO",TRUE
"C","ACQUISTATO DA PAESI UE CON CEDOLA",FALSE
...
```

**Importante**:
- `Descrizione` deve corrispondere ESATTAMENTE al testo nel file XLS
- `prov_italia` = TRUE per origine italiana, FALSE per estera
- Il sistema ora normalizza automaticamente spazi e maiuscole

## 🔧 File Modificati

### Codice
- `R/mod_verification.R` - Nuovo modulo di verifica (367 righe)
- `R/mod_upload_movimentazioni.R` - Normalizzazione migliorata (~50 righe modificate)
- `R/app_ui.R` - Integrazione UI (1 riga)
- `R/app_server.R` - Integrazione server (3 righe)
- `global.R` - Source moduli (3 righe)

### Documentazione
- `DEBUGGING_GUIDE.md` - Guida utente (6.2 KB)
- `R/README_mod_verification.md` - Docs tecnica (5.8 KB)
- `TESTING_GUIDE.md` - Guida testing (9.7 KB)
- `IMPLEMENTATION_SUMMARY.md` - Riepilogo (8.8 KB)

## ✅ Checklist Post-Installazione

Dopo il merge di questa PR:

- [ ] Riavviare l'applicazione Shiny
- [ ] Testare con un file reale
- [ ] Verificare che le statistiche siano corrette
- [ ] Generare un report di verifica
- [ ] Verificare assenza errori in console
- [ ] Documentare eventuali problemi riscontrati

## 🆘 Supporto

### Ordine di Consultazione

1. **Prima consultazione**: [DEBUGGING_GUIDE.md](DEBUGGING_GUIDE.md)
2. **Problemi tecnici**: [R/README_mod_verification.md](R/README_mod_verification.md)
3. **Testing**: [TESTING_GUIDE.md](TESTING_GUIDE.md)
4. **Dettagli implementazione**: [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)

### Contatto

Se i problemi persistono:
1. Genera un report di verifica
2. Salva i log della console R
3. Crea un'issue su GitHub allegando entrambi

## 📈 Metriche di Successo

### Prima della Fix
```
❌ Animali da Italia: 0
❌ Partite da Italia: 0
❌ Nessun feedback diagnostico
```

### Dopo la Fix
```
✅ Animali da Italia: 328
✅ Partite da Italia: 42
✅ Report diagnostico completo
✅ Problemi identificati automaticamente
```

## 🎓 Per Saperne di Più

### Concetti Chiave

**Normalizzazione del Testo**: Processo che rende comparabili stringhe con piccole differenze (spazi, maiuscole, ecc.)

**Merge**: Operazione che unisce due dataset basandosi su chiavi comuni (es. ingresso_motivo)

**prov_italia**: Campo derivato che indica se l'animale proviene dall'Italia (TRUE) o dall'estero (FALSE)

### Flusso Dati

```
1. File XLS/GZ
   ↓
2. Lettura e parsing
   ↓
3. Normalizzazione testo (normalize_text)
   ↓
4. Merge con motivi_ingresso
   ├→ Match ✓: prov_italia assegnato
   └→ No match ✗: prov_italia = NA
   ↓
5. Arricchimento geografico
   ↓
6. Creazione partite
   ↓
7. Statistiche e report
```

## 🔄 Aggiornamenti Futuri

Possibili miglioramenti pianificati:
- Export report in PDF/Excel
- Grafici interattivi
- Suggerimenti automatici per correzioni
- Cronologia report generati
- Test automatizzati

---

**Versione**: 1.0  
**Data**: 2025-11-19  
**Stato**: ✅ Pronto per il deploy  
**License**: Come da repository principale
