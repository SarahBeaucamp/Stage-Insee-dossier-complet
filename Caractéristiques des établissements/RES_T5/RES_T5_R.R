# Particuliers employeurs en 2024

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
  filter(GEO == "44109", ID_TAB == "RES_T5") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
employeur <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2024",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "RES_T5",
    TAB_MEASURE_LABEL %in% c(
      "Établissements – Accueil de jeunes enfants",
      "Établissements – Aide à domicile ; Accueil de jeunes enfants ; Activités des ménages en tant qu'employeurs de personnel domestique",
      "Établissements – Aide à domicile et Activités des ménages en tant qu'employeurs de personnel domestique"
    )
  ) %>%
  
  mutate (
    type = case_when(
      TAB_MEASURE_LABEL == "Établissements – Accueil de jeunes enfants" ~ "Accueil de jeunes enfants",
      TAB_MEASURE_LABEL == "Établissements – Aide à domicile ; Accueil de jeunes enfants ; Activités des ménages en tant qu'employeurs de personnel domestique" ~ "Accueil de jeunes enfants et aide domestique",
      TAB_MEASURE_LABEL == "Établissements – Aide à domicile et Activités des ménages en tant qu'employeurs de personnel domestique" ~ "Aide à domicile et Activités des ménages"
    )
  ) %>%
  
  group_by(type) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(employeur)

# Histogramme

library(ggplot2)
library(stringr)

ggplot(employeur, aes(x = type, y = total_groupe)) +
  geom_col(fill = "steelblue") + 
  
  #coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(equip$équipements)),
    labels = function(x) str_wrap(x, width = 10) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Particuliers employeurs en 2024", 
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )