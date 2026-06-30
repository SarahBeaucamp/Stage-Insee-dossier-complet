# Nombre équipements scolaire

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
  filter(GEO == "44109", ID_TAB == "EQUIP_T3") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

equip_sco <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2024",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "EQUIP_T3",
    TAB_MEASURE_LABEL %in% c(
      "Nombre d'équipements – Bibliothèque",
      "Nombre d'équipements – École maternelle",
      "Nombre d'équipements – Lycée d’enseignement général et/ou technologique",
      "Nombre d'équipements – Sgt section d’enseignement général et technologique",
      "Nombre d'équipements – École primaire",
      "Nombre d'équipements – École élémentaire",
      "Nombre d'équipements – Lycée d’enseignement professionnel",
      "Nombre d'équipements – Lycée d’enseignement technique et/ou professionnel agricole",
      "Nombre d'équipements – Collège",
      "Nombre d'équipements – Sep section d’enseignement professionnel"
    )
  ) %>%
  
  mutate (
    sco = case_when(
      TAB_MEASURE_LABEL == "Nombre d'équipements – Bibliothèque" ~ "Bibliothèque",
      TAB_MEASURE_LABEL == "Nombre d'équipements – Collège" ~ "Collège",
      TAB_MEASURE_LABEL %in% c("Nombre d'équipements – École maternelle", "Nombre d'équipements – École primaire", "Nombre d'équipements – École élémentaire") ~ "Ecole",
      TAB_MEASURE_LABEL %in% c("Nombre d'équipements – Lycée d’enseignement général et/ou technologique", "Nombre d'équipements – Sgt section d’enseignement général et technologique") ~ "Lycée d’enseignement général et/ou technologique",
      TAB_MEASURE_LABEL %in% c("Nombre d'équipements – Lycée d’enseignement professionnel", "Nombre d'équipements – Lycée d’enseignement technique et/ou professionnel agricole", "Nombre d'équipements – Sep section d’enseignement professionnel") ~ "Lycée d’enseignement professionnel ou agricole"
    )
  ) %>%
  
  group_by(sco) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(equip_sco)

# Histogramme 

library(ggplot2)
library(stringr)

ggplot(equip_sco, aes(x = sco, y = total_groupe)) +
  geom_col(fill = "steelblue") + 
  
  #coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(equip_sco$sco)),
    labels = function(x) str_wrap(x, width = 10) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Nombre d'établissements scolaires et de bibliothèques en 2024", #2022 n'existe pas pour ces données
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )