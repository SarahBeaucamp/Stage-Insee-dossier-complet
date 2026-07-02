# Nombre hôtels étoilés par villes

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
  filter(GEO == "44109", ID_TAB == "TOU_T1") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

Ordre_h <- c(
  "1 étoile",
  "2 étoiles",
  "3 étoiles",
  "4 étoiles",
  "5 étoiles",
  "Non classé"
)

cap_h <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2026",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "TOU_T1",
    TAB_MEASURE_LABEL %in% c(
      "Nombre de places – Hôtels et hébergement similaire x 1 étoile",
      "Nombre de places – Hôtels et hébergement similaire x 2 étoiles",
      "Nombre de places – Hôtels et hébergement similaire x 3 étoiles",
      "Nombre de places – Hôtels et hébergement similaire x 4 étoiles",
      "Nombre de places – Hôtels et hébergement similaire x 5 étoiles",
      "Nombre de places – Hôtels et hébergement similaire x Non classé"
    )
  ) %>%
  
  mutate (
    étoiles = case_when(
      TAB_MEASURE_LABEL == "Nombre de places – Hôtels et hébergement similaire x 1 étoile" ~ "1 étoile",
      TAB_MEASURE_LABEL == "Nombre de places – Hôtels et hébergement similaire x 2 étoiles" ~ "2 étoiles",
      TAB_MEASURE_LABEL == "Nombre de places – Hôtels et hébergement similaire x 3 étoiles" ~ "3 étoiles",
      TAB_MEASURE_LABEL == "Nombre de places – Hôtels et hébergement similaire x 4 étoiles" ~ "4 étoiles",
      TAB_MEASURE_LABEL == "Nombre de places – Hôtels et hébergement similaire x 5 étoiles" ~ "5 étoiles",
      TAB_MEASURE_LABEL == "Nombre de places – Hôtels et hébergement similaire x Non classé" ~ "Non classé"
    )
  ) %>%
  
  group_by(étoiles) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(étoiles = factor(étoiles, levels = Ordre_h))

View(cap_h)

# Histogramme 

library(ggplot2)
library(stringr)

ggplot(cap_h, aes(x = étoiles, y = total_groupe)) +
  geom_col(fill = "steelblue") + 
  
  #coord_flip() + 
  
  scale_x_discrete(
    limits = levels(cap_h$étoiles),
    labels = function(x) str_wrap(x, width = 10)
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Capacité des hôtels au 1er janvier 2026", 
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )