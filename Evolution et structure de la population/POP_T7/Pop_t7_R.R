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
  filter(GEO == "44109", ID_TAB == "POP_T5") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

Ordre_CSP <- c(
  "Agriculteurs exploitants",
  "Artisans, commerçants, chefs d'entreprise",
  "Cadres et professions intellectuelles supérieures",
  "Professions intermédiaires",
  "Employés",
  "Ouvriers",
  "Retraités",
  "Autres personnes sans activité professionnelle"
)

csp <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "POP_T5",
    TAB_MEASURE_LABEL %in% c(
      "Population – 15 ans ou plus x Autres inactifs",
      "Population – 15 ans ou plus x Employés",
      "Population – 15 ans ou plus x Retraités",
      "Population – 15 ans ou plus x Etudiants ou élèves",
      "Population – 15 ans ou plus x Ouvriers",
      "Population – 15 ans ou plus x Cadres et professions intellectuelles supérieures",
      "Population – 15 ans ou plus x Artisans, commerçants et chefs d’entreprise",
      "Population – 15 ans ou plus x Professions intermédiaires",
      "Population – 15 ans ou plus x Agriculteurs"
    )
  ) %>%
  
  mutate (
    catégories = case_when(
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Employés" ~ "Employés",
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Retraités" ~ "Retraités",
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Ouvriers" ~ "Ouvriers",
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Cadres et professions intellectuelles supérieures" ~ "Cadres et professions intellectuelles supérieures",
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Artisans, commerçants et chefs d’entreprise" ~ "Artisans, commerçants, chefs d'entreprise",
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Professions intermédiaires" ~ "Professions intermédiaires",
      TAB_MEASURE_LABEL == "Population – 15 ans ou plus x Agriculteurs" ~ "Agriculteurs exploitants",
      TAB_MEASURE_LABEL %in% c("Population – 15 ans ou plus x Autres inactifs", 
                               "Population – 15 ans ou plus x Etudiants ou élèves") ~ "Autres personnes sans activité professionnelle"
    )
  ) %>%
  
  group_by(catégories) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  ) %>%
  
  mutate(catégories = factor(catégories, levels = Ordre_CSP))

View(csp)

# Histogramme 

library(ggplot2)
library(stringr)

ggplot(csp, aes(x = catégories, y = part_groupe)) +
  geom_col(fill = "steelblue") + 
  
  coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(csp$catégories)),
    labels = function(x) str_wrap(x, width = 28) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Population de 15 ans ou plus selon le groupe socioprofessionnel",
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