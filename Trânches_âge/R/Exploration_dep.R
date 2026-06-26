library(DBI)
library(duckdb)

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

library(dplyr)
# Créer une table virtuelle
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))


# Maintenant nous allons tenter de calculer le nombre de Français par tranches
# d'âge mais cette fois-ci nous allons prendre en compte les départements et non
# les communes afin de voir si le résultat change

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
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
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)

# Conclusion ce n'est pas mieux 

#Essayons maintenant avec un filtre sur le code GEO à 3 chiffres nchar(GEO) == 5

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
      nchar(GEO) == 3 &
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
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)

# Maintenant avec cette restriction on a des résultats beaucoup trop faibles tels que 
# 322 958 personnes de 25 à 39 ans en France 

#Ca vient peut être de trois qui est tros restrictif essayons 2 ou 3
nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
      nchar(GEO) <= 3 &
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
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)

# Là on retrouve les même chiffres que lors du premier traitement selon les départements
# Si on essaie uniquement avec 2 

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
      nchar(GEO) == 2 &
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
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)

# Résultats toujours trop élevés et ces résultats ressemblent beaucoup à ceux obtenus 
# en filtrant sur communes distinctes 