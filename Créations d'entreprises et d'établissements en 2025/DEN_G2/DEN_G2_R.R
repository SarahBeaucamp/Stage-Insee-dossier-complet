# Nombres d'électeurs en 2020, 2021, 2022, 2024, 2026
# Electeurs inscrits sur la liste par élections

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

r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == "DEN_G2") %>%
  # distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Electeurs par ans et par type d'élection
crea <- dossier_complet %>%
  filter(
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "DEN_G2",
  ) %>%
  
  select(TIME_PERIOD, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  
  collect()

View(elec)

# Graphique 
library(ggplot2)

plot(crea$TIME_PERIOD, crea$OBS_VALUE, type = "b", col = "blue", lwd = 2, 
          main = "Évolution des créations d'établissements", xlab = "Année de création", ylab = "Nombre d'établissements crés")



