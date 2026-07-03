# Nombre d'unités légales économiquement actives en 2023

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
  filter(GEO == "44109", ID_TAB == "RES_G1") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
eco_act <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2024",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "RES_G1",
    TAB_MEASURE_LABEL %in% c(
      "Établissements – Construction",
      "Établissements – Industrie manufacturière, industries extractives et autres",
      "Établissements – Administration publique, enseignement, santé humaine et action sociale",
      "Établissements – Services principalement marchands",
      "Établissements – Agriculture, sylviculture et pêche"
    )
  ) %>%
  
  mutate (
    secteur_act = case_when(
      TAB_MEASURE_LABEL == "Établissements – Construction" ~ "Industrie manufacturière, industries extractives et autres",
      TAB_MEASURE_LABEL == "Établissements – Industrie manufacturière, industries extractives et autres" ~ "Industrie manufacturière, industries extractives et autres",
      TAB_MEASURE_LABEL == "Établissements – Administration publique, enseignement, santé humaine et action sociale" ~ "Administration publique, enseignement, santé humaine et action sociale",
      TAB_MEASURE_LABEL == "Établissements – Services principalement marchands" ~ "Services principalement marchands",
      TAB_MEASURE_LABEL == "Établissements – Agriculture, sylviculture et pêche" ~ "Agriculture, sylviculture et pêche"
    )
  ) %>%
  
  group_by(secteur_act) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(eco_act)

# Diagramme en camembert

eco_act <- eco_act %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(eco_act, aes(x = "", y = total_groupe, fill = secteur_act)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Secteur d'activité") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = "Répartition des établissements par secteur d'activité agrégé fin 2024",
    subtitle = "Source : Recensement Insee 2022",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5),
    legend.position = "right"
  )

