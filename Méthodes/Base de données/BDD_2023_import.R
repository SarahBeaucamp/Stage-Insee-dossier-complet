# ==============================================================================
# PRÉPARATION DE LA BASE DE DONNÉES ET GESTION EXPERTE DES VALEURS MANQUANTES
# ==============================================================================

# Installation des packages manquants si nécessaire
# install.packages(c("missForest", "VIM", "Metrics"))

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)
library(tidyr)
install.packages("missForest")
library(missForest) # Pour l'Option A
install.packages("Metrics")
library(Metrics)    # Pour le calcul de la RMSE

# ------------------------------------------------------------------------------
# ÉTAPE 1 : CONNEXION ET EXTRACTION DES DONNÉES
# ------------------------------------------------------------------------------

con <- dbConnect(duckdb(), dbdir = "base.duckdb")
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "INSTALL aws;")
dbExecute(con, "LOAD aws;")

dbExecute(con, "CREATE OR REPLACE SECRET (
TYPE S3,
PROVIDER CREDENTIAL_CHAIN,
ENDPOINT 'minio.lab.sspcloud.fr',
URL_STYLE 'path'
);")

chemin_s3 <- "s3://sarahbeaucamp/dossier_complet_2023.parquet"
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

base_filtree <- dossier_complet %>%
  filter(
    GEO_OBJECT_LABEL == "Commune",
    str_detect(ID_TAB, "^FAM|^FOR|^EMP|^LOG") | 
      ID_TAB %in% c("POP_T8", "POP_T9", "REV_T1", "SAL_G1", "SAL_G3", 
                    "SAL_T1", "SAL_G4", "TOU_T1", "TOU_T2", "TOU_T3", 
                    "EQUIP_T1", "EQUIP_T2", "EQUIP_T3", "DEN_T1", "POP_T3", "POP_T0")
  ) %>%
  select(GEO, GEO_LABEL, ID_TAB, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  collect()

print("Étape 1 terminée : Données extraites.")
