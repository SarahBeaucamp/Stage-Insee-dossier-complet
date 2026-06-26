library(DBI)
library(duckdb)

# 1. Créer ou ouvrir une base DuckDB locale (elle sera stockée dans un fichier 'base.duckdb')
con <- dbConnect(duckdb(), dbdir = "base.duckdb")

# 2. Charger les extensions nécessaires pour lire le S3 dans DuckDB
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "INSTALL aws;")
dbExecute(con, "LOAD aws;")

# 3. Injecter automatiquement les identifiants SSP Cloud dans DuckDB
dbExecute(con, "CREATE OR REPLACE SECRET (
TYPE S3,
PROVIDER CREDENTIAL_CHAIN,
ENDPOINT 'minio.lab.sspcloud.fr',
URL_STYLE 'path'
);")

# 4. Définir l'adresse de votre fichier sur le S3 du SSP Cloud
chemin_s3 <- "s3://sarahbeaucamp/dossier_complet.parquet"

library(dplyr)
# Créer une table virtuelle
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

# Afficher les 10 premières lignes 
début <- dossier_complet %>%
  head(10) %>%
  collect()

#Résumé de la base de données
glimpse(dossier_complet)

dossier_complet %>%
  summarise(
    minimum = min(GEO_REF, na.rm = TRUE),
    maximum = max(GEO_REF, na.rm = TRUE),
    nb_valeurs_uniques = n_distinct(GEO_REF)
  ) %>%
  collect()

#Comme dans le précédent code pour avoir les variables
dossier_complet %>%
  count(GEO_REF, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO_OBJECT, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO_OBJECT_LABEL, sort = TRUE) %>%
  collect()


dossier_complet %>%
  count(GEO, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(GEO_LABEL, sort = TRUE) %>%
  head(30) %>%
  collect()

dossier_complet %>%
  count(GEO_REF, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(TIME_PERIOD, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(CONF_STATUS, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(OBS_STATUS, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(UNIT_MULT, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(UNIT_MEASURE, sort = TRUE) %>%
  collect()

dossier_complet %>%
  count(TAB_MEASURE_LABEL, sort = TRUE) %>%
  collect()

#Lister toutes les modalités de la colonne TAB_MEASURE_LABEL et les stocker dans un csv
toutes_les_modalites <- dossier_complet %>% 
  count(TAB_MEASURE_LABEL, sort = TRUE) %>% 
  collect()

write.csv(toutes_les_modalites,
          file = file.choose(new = TRUE),
          row.names = FALSE)

# Maintenant nous allons faire quelques traitements

# Calcul du nombre de personnes appartenant à chaque tranche d'âge en France

library(ggplot2)

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      TAB_MEASURE_LABEL %in% c(
        "Population – Moins de 15 ans",
        "Population – De 15 à 24 ans ",
        "Population – De 25 à 39 ans",
        "Population – De 40 à 54 ans",
        "Population – De 55 à 64 ans",
        "Population – De 65 à 79 ans",
        "Population – 80 ans ou plus"
      )
  ) %>%
  # Étape de nettoyage par code commune unique
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)


# Calcul de la répartition des différentes tranches d'âge par ville

structure_villes_propres <- dossier_complet %>%
  filter(
      GEO_OBJECT_LABEL == "Commune" & 
      ID_TAB == "POP_T1"
  ) %>%
  collect()
  # Unicité stricte par couple Code Commune / Tranche
  distinct(GEO, TAB_MEASURE_LABEL, .keep_all = TRUE) %>%
  group_by(GEO, GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(population_brute_ville = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

# Résultat
View(structure_villes_propres)

structure_villes_propres <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      ID_TAB == "POP_T1" &
      TAB_MEASURE_LABEL %in% c(
        "Population – Moins de 15 ans",
        "Population – De 15 à 24 ans ",
        "Population – De 25 à 39 ans",
        "Population – De 40 à 54 ans",
        "Population – De 55 à 64 ans",
        "Population – De 65 à 79 ans",
        "Population – 80 ans ou plus"
      )
  ) %>%
  # Unicité stricte par couple Code Commune / Tranche
  distinct(GEO, TAB_MEASURE_LABEL, .keep_all = TRUE) %>%
  group_by(GEO, GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(population_brute_ville = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

# Résultat
View(structure_villes_propres)

# Calcul de la répartition des différentes tranches d'âge par département

structure_departements <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
      TAB_MEASURE_LABEL %in% c(
        "Population – Moins de 15 ans",
        "Population – De 15 à 24 ans ",
        "Population – De 25 à 39 ans",
        "Population – De 40 à 54 ans",
        "Population – De 55 à 64 ans",
        "Population – De 65 à 79 ans",
        "Population – 80 ans ou plus"
      )
  ) %>%
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE, .keep_all = TRUE) %>%
  group_by(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(population_brute_dept = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

View(structure_departements)


# Calcul de la répartition des différentes tranches d'âge par région

structure_regions <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Région" & 
      TAB_MEASURE_LABEL %in% c(
        "Population – Moins de 15 ans",
        "Population – De 15 à 24 ans ",
        "Population – De 25 à 39 ans",
        "Population – De 40 à 54 ans",
        "Population – De 55 à 64 ans",
        "Population – De 65 à 79 ans",
        "Population – 80 ans ou plus"
      )
  ) %>%
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE, .keep_all = TRUE) %>%
  group_by(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(population_brute_reg = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

View(structure_regions)

# Diagramme en cammenbert pour la répartition de la population en tranches d'âge 

library(ggplot2)
library(scales) 

# 1. On récupère les données nationales
structure_reelle <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      TAB_MEASURE_LABEL %in% c(
        "Population – Moins de 15 ans",
        "Population – De 15 à 24 ans ",
        "Population – De 25 à 39 ans",
        "Population – De 40 à 54 ans",
        "Population – De 55 à 64 ans",
        "Population – De 65 à 79 ans",
        "Population – 80 ans ou plus"
      )
  ) %>%
  distinct(GEO, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  group_by(TAB_MEASURE_LABEL) %>%
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>%
  collect() %>%
  mutate(part_pourcentage = (population_brute / sum(population_brute)))

# 2 Ordre chronologique des âges pour la légende et le graphique
Ordre_Ages <- c(
  "Population – Moins de 15 ans",
  "Population – De 15 à 24 ans ",
  "Population – De 25 à 39 ans",
  "Population – De 40 à 54 ans",
  "Population – De 55 à 64 ans",
  "Population – De 65 à 79 ans",
  "Population – 80 ans ou plus"
)

structure_reelle <- structure_reelle %>%
  mutate(TAB_MEASURE_LABEL = factor(TAB_MEASURE_LABEL, levels = Ordre_Ages))

# 3 Création du diagramme en camembert
ggplot(structure_reelle, aes(x = "", y = population_brute, fill = TAB_MEASURE_LABEL)) +
  geom_col(width = 1, color = "white") + 
  coord_polar("y", start = 0, direction = -1) +         
  scale_fill_brewer(palette = "Blues", name = "Tranches d'âge") +
  
  # Ajout du pourcentage sur les parts
  geom_text(aes(label = percent(part_pourcentage, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold",
            size = 2.1) +
  labs(
    title = "Répartition de la population française par tranche d'âge",
    subtitle = "Source : Recensement Insee 2022 (Calcul direct)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5),
    legend.position = "right"
  )