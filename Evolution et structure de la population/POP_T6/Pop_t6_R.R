# Lieu de résidence 1 an auparavant
# Objectif : Diagramme en camenbert où on peut sélectionner l'année et ensuite avoir le lieu de résidence 1 an auparavant 

# Modalité.csv, les modalités sont : 
# "Population – 1 an ou plus x Dans le même logement"
# "Population – 1 an ou plus x Autre logement dans la même commune"

#"Population – 1 an ou plus x Autre commune du département"
#"Population – 1 an ou plus x Hors de la région en métropole"
#"Population – 1 an ou plus x Autre département de la région"
#"Population – 1 an ou plus x Hors métropole ou DOM"
#"Population – 1 an ou plus x Hors de la région dans un DOM"

# On va devoir créer une nouvelle variable 

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

déménagements <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "POP_T4",
    TAB_MEASURE_LABEL %in% c(
      "Population – 1 an ou plus x Dans le même logement",
      "Population – 1 an ou plus x Autre logement dans la même commune",
      "Population – 1 an ou plus x Autre commune du département",
      "Population – 1 an ou plus x Hors de la région en métropole",
      "Population – 1 an ou plus x Autre département de la région",
      "Population – 1 an ou plus x Hors métropole ou DOM",
      "Population – 1 an ou plus x Hors de la région dans un DOM"
      
    )
  ) %>%
  
  mutate (
    Lieu_de_résidence = case_when(
      TAB_MEASURE_LABEL == "Population – 1 an ou plus x Dans le même logement" ~ "Dans le même logement",
      TAB_MEASURE_LABEL == "Population – 1 an ou plus x Autre logement dans la même commune" ~ "Dans un autre logement de la même commune",
      TAB_MEASURE_LABEL %in% c("Population – 1 an ou plus x Autre commune du département",
                               "Population – 1 an ou plus x Hors de la région en métropole",
                               "Population – 1 an ou plus x Autre département de la région",
                               "Population – 1 an ou plus x Hors métropole ou DOM",
                               "Population – 1 an ou plus x Hors de la région dans un DOM") ~ "Dans une autre commune"
    )
  ) %>%
  
  group_by(Lieu_de_résidence) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(déménagements)

r <- dossier_complet %>%
  filter(GEO == "44109", GEO_OBJECT_LABEL == "Commune", ID_TAB == "POP_T4") %>%
  #distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Diagramme en camembert

library(ggplot2)
library(scales) 

déménagements <- déménagements %>%
  mutate(
    part_pourcentage = total_groupe / sum(total_groupe)
  )

ggplot(déménagements, aes(x = "", y = total_groupe, fill = Lieu_de_résidence)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Lieu de résidence") +
  
  geom_text(aes(label = scales::percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.5) +
  labs(
    title = "Lieu de résidence 1 an auparavant",
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
  
