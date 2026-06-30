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

Ordre_occup <- c(
  "Sous-occupation très accentuée",
  "Sous-occupation accentuée",
  "Sous-occupation modérée",
  "Occupation dans la norme",
  "Suroccupation modérée",
  "Suroccupation accentuée"
)

r <- dossier_complet %>%
  filter(GEO == "44109", ID_TAB == "LOG_T4bis") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

taille_occup <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "LOG_T4bis",
    TAB_MEASURE_LABEL %in% c(
      "Logements – Occupation dans la norme",
      "Logements – Suroccupation modérée",
      "Logements – Sous-occupation très accentuée",
      "Logements – Suroccupation accentuée",
      "Logements – Sous-occupation modérée",
      "Logements – Sous-occupation accentuée"
    )
  ) %>%
  
  mutate (
    occupation = case_when(
      TAB_MEASURE_LABEL == "Logements – Occupation dans la norme" ~ "Occupation dans la norme",
      TAB_MEASURE_LABEL == "Logements – Suroccupation modérée" ~ "Suroccupation modérée",
      TAB_MEASURE_LABEL == "Logements – Sous-occupation très accentuée" ~ "Sous-occupation très accentuée",
      TAB_MEASURE_LABEL == "Logements – Suroccupation accentuée" ~ "Suroccupation accentuée",
      TAB_MEASURE_LABEL == "Logements – Sous-occupation modérée" ~ "Sous-occupation modérée",
      TAB_MEASURE_LABEL == "Logements – Sous-occupation accentuée" ~ "Sous-occupation accentuée"
    )
  ) %>%
  
  group_by(occupation) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(occupation = factor(occupation, levels = Ordre_occup))

View(taille_occup)

# Diagramme


taille_occup <- taille_occup %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

# Histogramme 

library(ggplot2)
library(stringr)

ggplot(taille_occup, aes(x = occupation, y = part_groupe)) +
  geom_col(fill = "steelblue") + 
  
  coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(taille_occup$occupation)),
    labels = function(x) str_wrap(x, width = 28) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Indice de peuplement des résidences principales",
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