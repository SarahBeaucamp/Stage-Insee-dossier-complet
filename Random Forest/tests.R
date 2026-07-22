# Fichier de test pour les noms des variables parce que ça peut être long

# Préparation de la base de données avant de pouvoir procéder à la random forest

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Créer ou ouvrir une base DuckDB locale
con <- dbConnect(duckdb(), dbdir = "base.duckdb")

# 2. Charger les extensions nécessaires
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

# Etape 1 : Selection des variables d'intérêt
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

# Petite vérification du volume récupéré
print(paste("Nombre de lignes récupérées :", nrow(base_filtree)))
print(paste("Nombre de variables distinctes :", length(unique(base_filtree$TAB_MEASURE_LABEL))))

# Etape 2 : Pivot sur les colonnes de la base de données
library(tidyr)

base_large <- base_filtree %>%
  # 1. On force un seul nom de ville par code INSEE
  group_by(GEO) %>%
  mutate(GEO_LABEL = first(GEO_LABEL)) %>%
  ungroup() %>%
  
  # 2. Le pivot sécurisé avec les libellés en clair
  pivot_wider(
    id_cols = c(GEO, GEO_LABEL), 
    names_from = TAB_MEASURE_LABEL, 
    values_from = OBS_VALUE,
    values_fn = max              
  )

# On remplace les espaces, accents et caractères spéciaux par des points
names(base_large) <- make.names(names(base_large), unique = TRUE)

r <- base_filtree %>%
  filter(GEO == "44109", ID_TAB == "POP_T0") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  # On simule la transformation pour voir le nom exact de la colonne
  mutate(NOM_DANS_BASE_LARGE = make.names(TAB_MEASURE_LABEL))

View(r)

# Résumé statistique de 4 variables de création d'entreprises
base_sans_na %>%
  # On isole toutes les colonnes de créations d'unités légales
  select(starts_with("Nombre.de.nouvelles.unités.légales.enregistrées")) %>%
  # On ne garde que les 4 premières pour que ce soit lisible
  select(1:4) %>%
  summary()