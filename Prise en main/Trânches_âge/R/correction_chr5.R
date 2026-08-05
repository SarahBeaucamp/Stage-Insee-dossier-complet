library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Créer ou ouvrir une base DuckDB locale (elle sera stockée dans un fichier 'base.duckdb')
con <- dbConnect(duckdb(), dbdir = "base.duckdb")

# 2. Charger les extensions nécessaires pour lire le S3 dans DuckDB
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "INSTALL aws;")
dbExecute(con, "LOAD aws;")

# 3. Injecter automatiquement les identifiants SSP Cloud dans DuckDB
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

# Afficher les 10 premières lignes 
début <- dossier_complet %>%
  head(10) %>%
  collect()

# Résumé de la base de données
glimpse(dossier_complet)

dossier_complet %>%
  summarise(
    minimum = min(GEO_REF, na.rm = TRUE),
    maximum = max(GEO_REF, na.rm = TRUE),
    nb_valeurs_uniques = n_distinct(GEO_REF)
  ) %>%
  collect()

# Plutôt
dossier_complet %>%
  count(GEO_REF, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO_OBJECT, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO_OBJECT_LABEL, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO_LABEL, sort = TRUE) %>%
  head(30) %>%
  collect()

dossier_complet %>%
  count(GEO_REF, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(TIME_PERIOD, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(CONF_STATUS, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(OBS_STATUS, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(UNIT_MULT, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(UNIT_MEASURE, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(TAB_MEASURE_LABEL, sort = TRUE) %>%
  collect()

# Lister toutes les modalités de la colonne TAB_MEASURE_LABEL et les stocker dans un csv
toutes_les_modalites <- dossier_complet %>% 
  count(TAB_MEASURE_LABEL, sort = TRUE) %>% 
  collect()

write.csv(toutes_les_modalites,
          file = file.choose(new = TRUE),
          row.names = FALSE)

# Définition de l'ordre chronologique des tranches d'âge
Ordre_Ages <- c(
  "Population – Moins de 15 ans",
  "Population – De 15 à 24 ans ",
  "Population – De 25 à 39 ans",
  "Population – De 40 à 54 ans",
  "Population – De 55 à 64 ans",
  "Population – De 65 à 79 ans",
  "Population – 80 ans ou plus"
)


# 1 : Calcul du nombre de personnes par tranche d'âge en France

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      TAB_MEASURE_LABEL %in% Ordre_Ages &
      nchar(GEO) == 5 # Restreint strictement aux codes communes à 5 caractères
  ) %>%
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect() %>%
  mutate(TAB_MEASURE_LABEL = factor(TAB_MEASURE_LABEL, levels = Ordre_Ages)) %>%
  arrange(TAB_MEASURE_LABEL)


print(nombre_total)

#Toujours le même problème


# 2 : Calcul de la répartition des tranches d'âge par ville

structure_villes_propres <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      TAB_MEASURE_LABEL %in% Ordre_Ages &
      nchar(GEO) == 5
  ) %>%
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE, .keep_all = TRUE) %>%
  group_by(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(population_brute_ville = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
  collect() %>% # Rapatriement local avant conversion en facteur pour éviter l'erreur de parser
  mutate(TAB_MEASURE_LABEL = factor(TAB_MEASURE_LABEL, levels = Ordre_Ages)) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL)

# Résultat villes
View(structure_villes_propres)

#A l'air un peu plus correct

# Région et département non fait ici en raison de l'erreur sur le total 