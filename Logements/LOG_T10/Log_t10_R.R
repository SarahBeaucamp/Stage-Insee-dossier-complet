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

Ordre_date <- c(
  "Avant 1919",
  "De 1919 à 1945",
  "De 1946 à 1970",
  "De 1971 à 1990",
  "De 1991 à 2005",
  "De 2006 à 2020"
)

r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == "LOG_T5") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

date_rési <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "LOG_T5",
    TAB_MEASURE_LABEL %in% c(
      "Logements – Résidences principales x Avant 1919",
      "Logements – Résidences principales x De 1919 à 1945",
      "Logements – Résidences principales x De 1946 à 1970",
      "Logements – Résidences principales x De 1971 à 1990",
      "Logements – Résidences principales x De 1991 à 2005",
      "Logements – Résidences principales x De 2006 à AAAA"
    )
  ) %>%
  
  mutate (
    date = case_when(
      TAB_MEASURE_LABEL == "Logements – Résidences principales x Avant 1919" ~ "Avant 1919",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x De 1919 à 1945" ~ "De 1919 à 1945",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x De 1946 à 1970" ~ "De 1946 à 1970",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x De 1971 à 1990" ~ "De 1971 à 1990",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x De 1991 à 2005" ~ "De 1991 à 2005",
      TAB_MEASURE_LABEL == "Logements – Résidences principales x De 2006 à AAAA" ~ "De 2006 à 2020"
    )
  ) %>%
  
  group_by(date) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(date = factor(date, levels = Ordre_date))

View(date_rési)

# Histogramme 

library(ggplot2)
library(stringr)

ggplot(date_rési, aes(x = date, y = part_groupe)) +
  geom_col(fill = "steelblue") + 
  
  coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(date_rési$date)),
    labels = function(x) str_wrap(x, width = 28) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Résidences principales en 2022 selon la période d'achèvement",
    x = "",
    y = "%"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 10),
    plot.title.position = "plot",
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )