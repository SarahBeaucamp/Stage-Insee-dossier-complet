r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == 'REV_T1') %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)