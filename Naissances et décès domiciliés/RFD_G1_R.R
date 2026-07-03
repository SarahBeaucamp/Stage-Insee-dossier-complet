# Nombre de naissances / décès

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
  filter(GEO == "44109", ID_TAB == "RFD_G1") %>%
 # distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Electeurs par ans et par type d'élection
solde <- dossier_complet %>%
  filter(
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "RFD_G1",
    TAB_MEASURE_LABEL %in% c(
      "Nombre de naissances vivantes",
      "Nombre de décès")
  ) %>%
  
  group_by(TAB_MEASURE_LABEL, TIME_PERIOD) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(solde)

# Graphique 
library(ggplot2)

# Définition des couleurs basées sur votre image
couleurs <- c(
  "Nombre de naissances vivantes" = "#003366",               
  "Nombre de décès" = "#ff4d4d")  

ggplot(solde, aes(x = as.numeric(TIME_PERIOD), y = total_groupe, color = TAB_MEASURE_LABEL)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = couleurs) +
  theme_minimal() +
  labs(
    title = "Naissances et décès domiciliés",
    x = NULL, 
    y = "en nombre",
    color = NULL
  ) +
  theme(legend.position = "top") +
  
  scale_x_continuous(breaks = seq(2012, 2025, 1))
