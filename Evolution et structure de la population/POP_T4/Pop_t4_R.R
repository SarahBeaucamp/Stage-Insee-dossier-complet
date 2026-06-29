# Population par sexe et âge regroupés en 2023
# Mêmes tranches d'âges que pour la section tranches d'âge. 
# But : à partir d'une région géographique choisie on a la répartition de la population par sexe et par tranche d'âge 
# Là je vais juste faire le niveau commune puis ensuite une adaptation en shiny 

library(DBI)
library(duckdb)
library(dplyr)

# 1. Créer ou ouvrir une base DuckDB locale (elle sera stockée dans un fichier 'base.duckdb')
con <- dbConnect(duckdb(), dbdir = "base.duckdb")

# 2. Charger les extensions nécessaires pour lire le S3 dans DuckDB
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "INSTALL aws;")
dbExecute(con, "LOAD aws;")

# 3. Injecter automatiquement les identifiants SSP Cloud dans DuckDB
# Le SSP Cloud expose les clés via des variables d'environnement, on les transmet à DuckDB
dbExecute(con, "CREATE OR REPLACE SECRET (
TYPE S3,
PROVIDER CREDENTIAL_CHAIN,
ENDPOINT 'minio.lab.sspcloud.fr',
URL_STYLE 'path'
);")

# 4. Définir l'adresse de votre fichier sur le S3 du SSP Cloud
chemin_s3 <- "s3://sarahbeaucamp/dossier_complet.parquet"

# Créer une table virtuelle
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

Ordre_Ages <- c(
  "Moins de 15 ans",
  "15-24 ans",
  "25-39 ans",
  "40-54 ans",
  "55-64 ans",
  "65-79 ans",
  "80 ans et plus"
)

structure_villes_propres <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    TAB_MEASURE_LABEL %in% c(
      "Population – Moins de 15 ans x Homme",
      "Population – De 15 à 24 ans x Homme",
      "Population – De 25 à 39 ans x Homme",
      "Population – De 40 à 54 ans x Homme",
      "Population – De 55 à 64 ans x Homme",
      "Population – De 65 à 79 ans x Homme",
      "Population – 80 ans ou plus x Homme",
      "Population – Moins de 15 ans x Femme",
      "Population – De 15 à 24 ans x Femme",
      "Population – De 25 à 39 ans x Femme",
      "Population – De 40 à 54 ans x Femme",
      "Population – De 55 à 64 ans x Femme",
      "Population – De 65 à 79 ans x Femme",
      "Population – 80 ans ou plus x Femme"
    )
  ) %>%
  mutate(
    SEXE = case_when(
      str_detect(TAB_MEASURE_LABEL, "Homme") ~ "Homme",
      str_detect(TAB_MEASURE_LABEL, "Femme") ~ "Femme",
      TRUE ~ "Total" 
    ),
    TRANCHE_AGE = case_when(
      str_detect(TAB_MEASURE_LABEL, "Moins de 15 ans") ~ "Moins de 15 ans",
      str_detect(TAB_MEASURE_LABEL, "15 à 24 ans") ~ "15-24 ans",
      str_detect(TAB_MEASURE_LABEL, "25 à 39 ans") ~ "25-39 ans",
      str_detect(TAB_MEASURE_LABEL, "40 à 54 ans") ~ "40-54 ans",
      str_detect(TAB_MEASURE_LABEL, "55 à 64 ans") ~ "55-64 ans",
      str_detect(TAB_MEASURE_LABEL, "65 à 79 ans") ~ "65-79 ans",
      str_detect(TAB_MEASURE_LABEL, "80 ans") ~ "80 ans et plus",
      TRUE ~ "Autre"
    )
  ) %>%
  
  group_by(GEO_LABEL, SEXE, TRANCHE_AGE) %>%
  summarise(total_ville = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
  
  group_by(GEO_LABEL, SEXE) %>%
  mutate(
    part_locale = total_ville / sum(total_ville, na.rm = TRUE),
    population_exacte_ville = round(part_locale * sum(total_ville, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  
  select(GEO_LABEL, SEXE, TRANCHE_AGE, total_ville, part_locale, population_exacte_ville) %>%
  
  collect() %>% 
  
  mutate(TRANCHE_AGE = factor(TRANCHE_AGE, levels = Ordre_Ages)) %>% 
  arrange(GEO_LABEL, SEXE, TRANCHE_AGE)

View(structure_villes_propres)

# Graphique 

library(ggplot2)
library(scales)

ggplot(structure_villes_propres, aes(x = TRANCHE_AGE, y = part_locale, fill = SEXE)) +
  geom_col(position = "dodge", alpha = 0.9) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  scale_fill_manual(values = c("Homme" = "lightblue", "Femme" = "pink")) +
  
  labs(
    title = paste("Population par sexe et âge regroupé  -", unique(structure_villes_propres$GEO_LABEL)),
    subtitle = "Année 2022",
    x = "Tranches d'âge",
    y = "Proportion d'hommes et de femmes",
    fill = "Sexe"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 18, hjust = 1, vjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )