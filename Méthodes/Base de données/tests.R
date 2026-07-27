r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == 'FAM_T3') %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

r_2023 <- dossier_complet_2023 %>%
  filter(GEO == "44109", ID_TAB == 'FAM_T3') %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r_2023)