# Nombre de pièces résidences princiales

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

Ordre_nb <- c(
  "Aucun enfant",
  "1 enfant",
  "2 enfants",
  "3 enfants",
  "4 enfants ou plus"
)

r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == "FAM_T4") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

nb_enfants <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "FAM_T4",
    TAB_MEASURE_LABEL %in% c(
      "Nombre de famille – 3 enfants de moins de 24 ans",
      "Nombre de famille – 1 enfant de moins de 24 ans",
      "Nombre de famille – 2 enfants de moins de 24 ans",
      "Nombre de famille – 4 enfants ou plus de moins de 24 ans",
      "Nombre de famille – Aucun enfant de moins de 24 ans"
    )
  ) %>%
  
  mutate (
    enfants = case_when(
      TAB_MEASURE_LABEL == "Nombre de famille – Aucun enfant de moins de 24 ans" ~ "Aucun enfant",
      TAB_MEASURE_LABEL == "Nombre de famille – 1 enfant de moins de 24 ans" ~ "1 enfant",
      TAB_MEASURE_LABEL == "Nombre de famille – 2 enfants de moins de 24 ans" ~ "2 enfants",
      TAB_MEASURE_LABEL == "Nombre de famille – 3 enfants de moins de 24 ans" ~ "3 enfants",
      TAB_MEASURE_LABEL == "Nombre de famille – 4 enfants ou plus de moins de 24 ans" ~ "4 enfants ou plus"
    )
  ) %>%
  
  group_by(enfants) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(enfants = factor(enfants, levels = Ordre_nb))

View(nb_enfants)

# Diagramme


nb_enfants <- nb_enfants %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(nb_enfants, aes(x = "", y = total_groupe, fill = enfants)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", name = "Nombre d'enfants de moins de 25 ans") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = " Familles selon le nombre d'enfants âgés de moins de 25 ans",
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


