# Pipeline Implementation - Verification Checklist

## Pre-Testing Checklist

### Code Structure
- [x] All required files created
- [x] All functions properly defined
- [x] Proper error handling in place
- [x] Documentation complete

### Dependencies
- [x] openxlsx added to app.R
- [x] All utility files sourced in global.R
- [x] Module properly structured with moduleServer

### UI Components
- [x] Three new tabs added to app_ui.R
- [x] Dynamic UI elements properly defined
- [x] Download buttons for all tables

### Server Logic
- [x] Pipeline module integrated in app_server.R
- [x] Output renderers for all new tabs
- [x] Download handlers implemented
- [x] Dynamic table generation for diseases

## Testing Checklist

### Unit Tests (via manual_test_pipeline.R)
- [ ] Test 1: classifica_origine - Italia/Estero classification
- [ ] Test 2: estrai_provincia_nascita - Ear tag parsing
- [ ] Test 3: estrai_comune_provenienza - Stabilimento to comune mapping
- [ ] Test 4: merge_malattie_con_prefisso - Disease data merging
- [ ] Test 5: filtra_animali_non_indenni - Filtering logic
- [ ] Test 6: Verify disease files exist

### Integration Tests (with Shiny app)

#### File Upload
- [ ] Upload bovini file (.xls format)
- [ ] Upload bovini file (.gz format)
- [ ] Upload ovicaprini file (.xls format)
- [ ] Upload ovicaprini file (.gz format)
- [ ] Verify file recognition and group detection

#### Tab: Input
- [ ] Verify file type message
- [ ] Check animal count display
- [ ] Verify disease list display
- [ ] Confirm correct group shown

#### Tab: Controllo Manuale
- [ ] Check "Animali di provenienza nazionale con codice stabilimento di origine non mappabile" table
  - [ ] Table displays correctly
  - [ ] Data is filtered (only Italian animals with NA comune)
  - [ ] Download button works
  - [ ] Excel file contains expected data
- [ ] Check "Animali nati in Italia con provincia nel marchio auricolare non mappabile" table
  - [ ] Table displays correctly
  - [ ] Data is filtered (only Italian animals with NA provincia)
  - [ ] Download button works
  - [ ] Excel file contains expected data

#### Tab: Provenienze
- [ ] Verify message when no cases: "Nessuna movimentazione proveniente da zone non indenni..."
- [ ] When cases exist:
  - [ ] One section per disease
  - [ ] Disease name displayed as title
  - [ ] Table shows animals from non-disease-free comuni
  - [ ] Disease columns have `prov_` prefix
  - [ ] Download button per disease works
  - [ ] Excel files contain correct data

#### Tab: Nascite
- [ ] Verify message when no cases: "Nessuna movimentazione di animali nati in zone non indenni..."
- [ ] When cases exist:
  - [ ] One section per disease
  - [ ] Disease name displayed as title
  - [ ] Table shows animals born in non-disease-free provinces
  - [ ] Disease columns have `nascita_` prefix
  - [ ] Download button per disease works
  - [ ] Excel files contain correct data

### Data Validation Tests

#### Origin Classification
- [ ] Animals with motivo "M", "N", "F", etc. → "italia"
- [ ] Animals with motivo "C", "E", "S", "T" → "estero"
- [ ] Check edge cases (NA, unknown codes)

#### Province Extraction from Ear Tags
- [ ] IT001... → 001 (or appropriate provincia code)
- [ ] IT201... → 201 (or appropriate provincia code)
- [ ] Non-IT tags → NA
- [ ] Malformed tags → NA
- [ ] NA inputs → NA

#### Comune Extraction from Stabilimento
- [ ] Valid codes (e.g., 001TO) → correct PRO_COM_T
- [ ] Invalid codes → NA
- [ ] NA inputs → NA

#### Disease Data Boolean Conversion
- [ ] All disease fields are boolean (TRUE/FALSE)
- [ ] No NA values in disease columns
- [ ] TRUE = indenne (disease-free)
- [ ] FALSE = non indenne

#### Merge Operations
- [ ] `prov_` prefix applied to provenance disease columns
- [ ] `nascita_` prefix applied to birth disease columns
- [ ] Geographic columns (COD_UTS, PRO_COM_T, COD_REG) preserved
- [ ] No duplicate columns in output
- [ ] All animals preserved (left join)

## Performance Tests

- [ ] Test with small file (< 100 rows)
- [ ] Test with medium file (100-1000 rows)
- [ ] Test with large file (> 1000 rows)
- [ ] Check UI responsiveness
- [ ] Verify memory usage is reasonable

## Error Handling Tests

- [ ] Upload invalid file format
- [ ] Upload empty file
- [ ] Upload file with missing columns
- [ ] Upload file with corrupted data
- [ ] Test with missing disease files
- [ ] Test with incomplete geographic data

## Browser Compatibility (if applicable)

- [ ] Chrome
- [ ] Firefox
- [ ] Safari
- [ ] Edge

## Documentation Review

- [ ] IMPLEMENTATION_README.md is accurate
- [ ] SUMMARY_IT.md is accurate
- [ ] Code comments are clear
- [ ] Function documentation is complete

## Post-Testing Actions

- [ ] Update checklist with test results
- [ ] Document any issues found
- [ ] Create tickets for future enhancements
- [ ] Mark PR ready for review

## Known Limitations

1. **Ear Tag Parsing**: Assumes standard IT + 12 digits format. May need adjustment for special cases.
2. **Stabilimento Codes**: Relies on df_stab table completeness. Missing codes will result in NA.
3. **Disease Data**: Assumes boolean TRUE/FALSE convention. Non-boolean data may cause issues.
4. **Performance**: Large files (>10000 rows) not yet tested.

## Future Enhancements

1. Add logging for debugging
2. Implement data validation warnings
3. Add export of complete dataset with all merges
4. Consider caching for performance
5. Add more detailed statistics in Controllo Manuale tab
6. Implement ear tag validation against known patterns
7. Add province/comune lookup tools

---
*Created: 2025-11-20*
*Last Updated: 2025-11-20*
