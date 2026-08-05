library(DBI)
library(duckdb)
# 1. Créer ou ouvrir une base DuckDB locale (elle sera stockée dans un fichier 'base.duckdb')
con <- dbConnect(duckdb(), dbdir = "base.duckdb")
# 2. Charger les extensions nécessaires pour lire le S3 dans DuckDB
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "INSTALL aws;")
dbExecute(con, "LOAD aws;")
# 3. Injecter automatiquement vos identifiants SSP Cloud dans DuckDB
# Le SSP Cloud expose vos clés via des variables d'environnement, on les transmet à DuckDB
dbExecute(con, "CREATE OR REPLACE SECRET (
TYPE S3,
PROVIDER CREDENTIAL_CHAIN,
ENDPOINT 'minio.lab.sspcloud.fr',
URL_STYLE 'path'
);")
# 4. Définir l'adresse de votre fichier sur le S3 du SSP Cloud
# L'endpoint par défaut du SSP Cloud est généralement géré automatiquement par la chaîne,
# mais vous pouvez spécifier l'adresse complète du fichier comme ceci :
chemin_s3 <- "s3://sarahbeaucamp/dossier_complet.parquet"

library(dplyr)
#Créer une table virtuelle
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

#Afficher les 10 premières lignes 
début <- dossier_complet %>%
  head(10) %>%
  collect()
