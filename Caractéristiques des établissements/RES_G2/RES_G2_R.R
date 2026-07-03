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
  filter(GEO == "44109", ID_TAB == "RES_G2") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
sal_act <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2024",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "RES_G2",
    TAB_MEASURE_LABEL %in% c(
      "Établissements – 200 à 499 salariés",
      "Établissements – 50 à 99 salariés",
      "Établissements – 20 à 49 salariés",
      "Établissements – 5 à 9 salariés",
      "Établissements – 500 salariés et plus",
      "Établissements – 10 à 19 salariés",
      "Établissements – 100 à 199 salariés",
      "Établissements – 1 à 4 salariés",
      "Établissements – 0 salarié"
    )
  ) %>%
  
  mutate (
    age = case_when(
      TAB_MEASURE_LABEL %in% c("Établissements – 1 à 4 salariés", "Établissements – 5 à 9 salariés") ~ "1 à 9 salariés", 
      TAB_MEASURE_LABEL == "Établissements – 0 salarié" ~ "0 salarié",
      TAB_MEASURE_LABEL == "Établissements – 10 à 19 salariés" ~ "10 à 19 salariés",
      TAB_MEASURE_LABEL == "Établissements – 20 à 49 salariés" ~ "20 à 49 salariés",
      TAB_MEASURE_LABEL %in% c("Établissements – 100 à 199 salariés", "Établissements – 500 salariés et plus", "Établissements – 50 à 99 salariés", "Établissements – 200 à 499 salariés") ~ "50 salariés et plus"
    )
  ) %>%
  
  group_by(age) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(sal_act)

# Diagramme en camembert

sal_act <- sal_act %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(sal_act, aes(x = "", y = total_groupe, fill = age)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Secteur d'activité") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = "Répartition des établissements par tranche d'effectifs fin 2024",
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

