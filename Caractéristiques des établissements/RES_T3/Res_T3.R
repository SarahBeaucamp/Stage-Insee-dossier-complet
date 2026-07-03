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
  filter(GEO == "44109", ID_TAB == "RES_T3") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
etab_spheres <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2024",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "RES_T3",
    TAB_MEASURE_LABEL %in% c(
      "Établissements – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère productive",
      "Établissements – Fonction Publique x Établissements appartenant à la sphère présentielle",
      "Établissements – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère présentielle",
      "Établissements – Fonction Publique x Établissements appartenant à la sphère productive"
    )
  ) %>%
  
  mutate (
    type_etab = case_when(
      TAB_MEASURE_LABEL == "Établissements – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère productive" ~ "Sphère productive",
      TAB_MEASURE_LABEL == "Établissements – Fonction Publique x Établissements appartenant à la sphère présentielle" ~ "Sphère présentielle, domaine public",
      TAB_MEASURE_LABEL == "Établissements – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère présentielle" ~ "Sphère présentielle",
      TAB_MEASURE_LABEL == "Établissements – Fonction Publique x Établissements appartenant à la sphère productive" ~ "Sphère productive, domaine public"
    )
  ) %>%
  
  group_by(type_etab) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(etab_spheres)

# Histogramme

library(ggplot2)
library(stringr)

ggplot(etab_spheres, aes(x = type_etab, y = total_groupe)) +
  geom_col(fill = "steelblue") + 
  
  #coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(equip$équipements)),
    labels = function(x) str_wrap(x, width = 10) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Établissements selon les sphères de l'économie fin 2024", 
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )



# Établissements selon les sphères de l'économie fin 2024

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
  filter(GEO == "44109", ID_TAB == "RES_T3") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
eff_spheres <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2024",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "RES_T3",
    TAB_MEASURE_LABEL %in% c(
      "Effectifs présents la dernière semaine de décembre – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère productive",
      "Effectifs présents la dernière semaine de décembre – Fonction Publique x Établissements appartenant à la sphère présentielle",
      "Effectifs présents la dernière semaine de décembre – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère présentielle",
      "Effectifs présents la dernière semaine de décembre – Fonction Publique x Établissements appartenant à la sphère productive"
    )
  ) %>%
  
  mutate (
    type_eff = case_when(
      TAB_MEASURE_LABEL == "Effectifs présents la dernière semaine de décembre – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère productive" ~ "Sphère productive",
      TAB_MEASURE_LABEL == "Effectifs présents la dernière semaine de décembre – Fonction Publique x Établissements appartenant à la sphère présentielle" ~ "Sphère présentielle, domaine public",
      TAB_MEASURE_LABEL == "Effectifs présents la dernière semaine de décembre – Ensemble hors particuliers employeurs x Établissements appartenant à la sphère présentielle" ~ "Sphère présentielle",
      TAB_MEASURE_LABEL == "Effectifs présents la dernière semaine de décembre – Fonction Publique x Établissements appartenant à la sphère productive" ~ "Sphère productive, domaine public"
    )
  ) %>%
  
  group_by(type_eff) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(eff_spheres)

# Histogramme

library(ggplot2)
library(stringr)

ggplot(eff_spheres, aes(x = type_eff, y = total_groupe)) +
  geom_col(fill = "steelblue") + 
  
  #coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(equip$équipements)),
    labels = function(x) str_wrap(x, width = 10) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Établissements selon les sphères de l'économie fin 2024", 
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )