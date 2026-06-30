# Nombre d'enfants âgés de moins de 25 ans 

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

Ordre_pièces <- c(
  "1 pièce",
  "2 pièces",
  "3 pièces",
  "4 pièces",
  "5 pièces ou plus"
)

r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == "LOG_T3") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

taille_log <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "LOG_T3",
    TAB_MEASURE_LABEL %in% c(
      "Logements – Résidences principales x 1 pièce",
      "Logements – Résidences principales x 2 pièces",
      "Logements – Résidences principales x 3 pièces",
      "Logements – Résidences principales x 4 pièces",
      "Logements – Résidences principales x 5 pièces ou plus"
    )
  ) %>%
  
  mutate (
    pièces = case_when(
      TAB_MEASURE_LABEL == "Logements – Résidences principales x 1 pièce" ~ "1 pièce",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x 2 pièces" ~ "2 pièces",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x 3 pièces" ~ "3 pièces",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x 4 pièces" ~ "4 pièces",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x 5 pièces ou plus" ~ "5 pièces ou plus"
    )
  ) %>%
  
  group_by(pièces) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(pièces = factor(pièces, levels = Ordre_pièces))

View(taille_log)

# Diagramme


taille_log <- taille_log %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(taille_log, aes(x = "", y = total_groupe, fill = pièces)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", name = "Nombre de pièces") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = "Résidences principales selon le nombre de pièces",
    subtitle = "Source : Recensement Insee",
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


