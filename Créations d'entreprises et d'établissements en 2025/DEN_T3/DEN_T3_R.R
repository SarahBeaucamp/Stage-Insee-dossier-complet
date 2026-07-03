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
  filter(GEO == "44109", ID_TAB == "DEN_T3") %>%
  distinct(TAB_MEASURE_LABEL) %>%
  collect()

View(r)

# Composition des familles
eco_act <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2023",
    GEO_OBJECT_LABEL == "Commune",
    GEO == "44109",
    ID_TAB == "DEN_T3",
    TAB_MEASURE_LABEL %in% c(
      "Nombre d'unités légales – Industrie manufacturière, industries extractives et autres",
      "Nombre d'unités légales – Activités immobilières",
      "Nombre d'unités légales – Activités spécialisées, scientifiques et techniques et activités de services administratifs et de soutien",
      "Nombre d'unités légales – Construction",
      "Nombre d'unités légales – Information et communication",
      "Nombre d'unités légales – Administration publique, enseignement, santé humaine et action sociale",
      "Nombre d'unités légales – Commerce de gros et de détail, transports, hébergement et restauration",
      "Nombre d'unités légales – Activités financières et d'assurance",
      "Nombre d'unités légales – Autres activités de services"
    )
  ) %>%
  
  mutate (
    secteur_act = case_when(
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Industrie manufacturière, industries extractives et autres" ~ "Industrie manufacturière, industries extractives et autres",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Activités immobilières" ~ "Activités immobilières",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Activités spécialisées, scientifiques et techniques et activités de services administratifs et de soutien" ~ "Activités spécialisées, scientifiques et techniques et activités de services administratifs et de soutien",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Construction" ~ "Construction",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Information et communication" ~ "Information et communication",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Administration publique, enseignement, santé humaine et action sociale" ~ "Administration publique, enseignement, santé humaine et action sociale",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Commerce de gros et de détail, transports, hébergement et restauration" ~ "Commerce de gros et de détail, transports, hébergement et restauration",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Activités financières et d'assurance" ~ "Activités financières et d'assurance",
      TAB_MEASURE_LABEL == "Nombre d'unités légales – Autres activités de services" ~ "Autres activités de services"
    )
  ) %>%
  
  group_by(secteur_act) %>%
  summarise(total_groupe = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  collect() %>%
  
  mutate(
    total = sum(total_groupe),
    part_groupe = (total_groupe / total) * 100
  )

View(eco_act)

# Histogramme

library(ggplot2)
library(stringr)

ggplot(eco_act, aes(x = secteur_act, y = total_groupe)) +
  geom_col(fill = "steelblue") + 
  
  #coord_flip() + 
  
  scale_x_discrete(
    limits = rev(levels(equip$équipements)),
    labels = function(x) str_wrap(x, width = 10) 
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Nombre d'unités légales économiquement actives en 2023", 
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(lineheight = 0.9) 
  )