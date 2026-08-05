# On met cette fois-ci que le GEO_OBJECT_LABEL c'est la France

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "France" & 
      TAB_MEASURE_LABEL %in% c(
        "Population – Moins de 15 ans",
        "Population – De 15 à 24 ans ",
        "Population – De 25 à 39 ans",
        "Population – De 40 à 54 ans",
        "Population – De 55 à 64 ans",
        "Population – De 65 à 79 ans",
        "Population – 80 ans ou plus"
      )
  ) %>%
  # Étape de nettoyage par code commune unique
  #distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  #group_by(TAB_MEASURE_LABEL) %>% 
  #summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)

# Résultats trés élevés, encore plus élevés que pour un filtrage sur les départements 
# ou sur les communes 