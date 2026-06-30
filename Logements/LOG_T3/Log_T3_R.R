# Type de maison

# Population de 15 ans ou plus selon la csp actuelle ou antérieure

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
  filter(GEO == "44109", ID_TAB == "LOG_T2") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

Ordre_log <- c(
  "Appartements",
  "Maisons",
  "Autres"
)

type_log <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "LOG_T2",
    TAB_MEASURE_LABEL %in% c(
      "Logements – Maison",
      "Logements – Appartement",
      "Logements – Autres logements de métropole"
    )
  ) %>%
  
  mutate (
    logement = case_when(
      TAB_MEASURE_LABEL == "Logements – Maison" ~ "Maisons",
      TAB_MEASURE_LABEL == "Logements – Appartement" ~ "Appartements",
      TAB_MEASURE_LABEL == "Logements – Autres logements de métropole" ~ "Autres"
    )
  ) %>%
  
  group_by(logement) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(logement = factor(logement, levels = Ordre_log))

View(type_log)

# Diagramme


type_log <- type_log %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(type_log, aes(x = "", y = total_groupe, fill = logement)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Types de logements") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = " Types de logements",
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

