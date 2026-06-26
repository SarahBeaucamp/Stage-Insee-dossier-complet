pop_totale_villes <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      TAB_MEASURE_LABEL == "Population" &
      ID_TAB == "POP_T1"
  ) %>%
  group_by(GEO_LABEL) %>%
  summarise(population_totale_ville = sum(OBS_VALUE, na.rm = TRUE))

# ÉTAPE 2 : On additionne les tranches d'âge par ville 
structure_villes_propres <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
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
  # On regroupe les résultats par ville et par tranche d'âge
  group_by(TAB_MEASURE_LABEL) %>%
  summarise(total = sum(OBS_VALUE, na.rm = TRUE)) %>%


# Résultat
View(structure_villes_propres)


# 1 On sélectionne par communes afin d'éviter les doublons avec les dép et régions
nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      ID_TAB == "POP_T1" &
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
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(total_brut = sum(OBS_VALUE, na.rm = TRUE)) %>% # Nombre de personnes par âges en France
  collect() %>%
  #View(structure_reelle)
  # On calcule le pourcentage de chaque tranche d'âge
  mutate(part_pourcentage = (total_brut / sum(total_brut))) %>%
  
  # 2. ON détermine le nombre grâce à la population en 2022
  mutate(population_exacte = round(part_pourcentage * 68000000)) %>%
  select(TAB_MEASURE_LABEL, population_exacte)

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)

