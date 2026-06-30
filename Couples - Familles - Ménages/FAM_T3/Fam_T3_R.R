# Composition des familles

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
  filter(GEO == "44109", ID_TAB == "FAM_T3") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
compo_fam <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "FAM_T3",
    TAB_MEASURE_LABEL %in% c(
      "Nombre de famille – Famille avec un père seul",
      "Nombre de famille – Famille comprenant un couple sans enfant résident",
      "Nombre de famille – Famille avec une mère seule",
      "Nombre de famille – Famille comprenant un couple avec enfant résident"
    )
  ) %>%
  
  mutate (
    Situation_f = case_when(
      TAB_MEASURE_LABEL == "Nombre de famille – Famille comprenant un couple avec enfant résident" ~ "Couple avec enfant(s)",
      TAB_MEASURE_LABEL == "Nombre de famille – Famille comprenant un couple sans enfant résident" ~ "Couple sans enfant(s)",
      TAB_MEASURE_LABEL == "Nombre de famille – Famille avec une mère seule" ~ "Femmes seules avec enfant(s)",
      TAB_MEASURE_LABEL == "Nombre de famille – Famille avec un père seul" ~ "Hommes seuls avec enfant(s)"
    )
  ) %>%
  
  group_by(Situation_f) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(compo_fam)

# Diagramme


compo_fam <- compo_fam %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(compo_fam, aes(x = "", y = total_groupe, fill = Situation_f)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Situation familiale") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = "Composition des familles",
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

