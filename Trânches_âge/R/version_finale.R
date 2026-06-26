library(DBI)
library(duckdb)

# Dans ce fichier, jeu sur les proportion qui donne les bonnes proportions à la fin mais des chiffres bien trop importants au début.

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

library(dplyr)
# Créer une table virtuelle
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

# Afficher les 10 premières lignes 
début <- dossier_complet %>%
  head(10) %>%
  collect()

glimpse(dossier_complet)

# Il y a 16 colones 
# GEO_REF : chr : Année
# GEO_OBJECT : chr : L'aire d'attraction des villes 2020 (AAV2020) est un 
#                   ensemble de communes définissant l'influence d'un pôle de 
#                   population et d'emploi sur les communes environnantes.
# GEO_OBJECT_LABEL: chr :aire d'attraction des villes 2020
# GEO : chr : code departement ( ex : 001)
# GEP_LABEL : chr : Vile ( ex : Paris)
# TIME_PERIOD : chr : Année ici 2026
# ID_TAB : chr : (ex : TOU_T1)
# ID_TAB_LABEL : chr : (ex : Nombre et capacité des hôtels au premier janvier)
# DS : chr : (ex : DS_TOUR_CAP)
# TAB MEASURE : chr : (ex : UNIT LOC ACTIVITY I551 UNIT LOC RANKING 3)
# TAB_MEASURE_LABEL : chr : ( ex : Etablissement, hôtels et hébergements 
#                             similaire x3 étoiles)
# OBS_VALUE : dbl : des entiers là dans les 10 pemiers le plus petit est 90 et 
#                   le plus grand est 170 920 
# OBS_STATUS : chr : (ex : A)
# UNIT_MULT : int : Na sur les 10 premiers
# UNIT_MEASURE : chr : Na sur les 10 premiers
# CONF_STATUS : chr : Na sur les 10 premiers 

dossier_complet %>%
  count(GEO_REF, sort = TRUE) %>%
  collect()
# 2025

dossier_complet %>%
  count(GEO_OBJECT, sort = TRUE) %>%
  collect()
# Il y en a 13 différents. Ce sont des abréviations des différents types d'aires urbaines

dossier_complet %>%
  count(GEO_OBJECT_LABEL, sort = TRUE) %>%
  collect()
#Les noms des aires urbaines en toutes lettres ( département, commune ...) 

dossier_complet %>%
  count(GEO, sort = TRUE) %>%
  collect()
#Les codes communes/départements

dossier_complet %>%
  count(GEO_LABEL, sort = TRUE) %>%
  head(30) %>%
  collect()
#Noms d'aires urbaines en toutes lettres 

dossier_complet %>%
  count(TIME_PERIOD, sort = TRUE) %>%
  collect()
# 22 années différentes 

dossier_complet %>%
  count(ID_TAB, sort = TRUE) %>%
  collect()
# Il y en a 82

dossier_complet %>%
  count(ID_TAB_LABEL, sort = TRUE) %>%
  collect()
# Noms de ces différentes tables

dossier_complet %>%
  count(DS, sort = TRUE) %>%
  collect()
# Noms des différents dataset

dossier_complet %>%
  count(TAB_MEASURE, sort = TRUE) %>%
  collect()
# ID de toutes les variables qu'on mesure 

dossier_complet %>%
  count(TAB_MEASURE_LABEL, sort = TRUE) %>%
  collect()
# Nom de toutes les variables qu'on mesure 

dossier_complet %>%
  count(OBS_VALUE, sort = TRUE) %>%
  collect()
# int, valeurs observées

dossier_complet %>%
  count(OBS_STATUS, sort = TRUE) %>%
  collect()
# 5 valeurs possibles : NA, A, O, W, K

dossier_complet %>%
  count(UNIT_MULT, sort = TRUE) %>%
  collect()
# Soit NA soit 0

dossier_complet %>%
  count(UNIT_MEASURE, sort = TRUE) %>%
  collect()
# 3 valeurs : NA, PT, EUR_YR

dossier_complet %>%
  count(CONF_STATUS, sort = TRUE) %>%
  collect()
# 3 valeurs possibles : NA, F ou C


# REPARTITION PAR CLASSE D'AGE DE LA POPULATION FRANCAISE

nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "France" & 
      GEO_LABEL == "France" &
      ID_TAB == "POP_T0" &
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
  select(GEO_LABEL, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  collect()
# Tableau du nombre de personnes par tranches d'âge
View(nombre_total)

# DIAGRAMME EN CAMEMBERT POUR LA REPARTITION DE L'AGE 

library(ggplot2)
library(scales) 

# 1. On récupère les données nationales
nombre_total <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "France" & 
      GEO_LABEL == "France" &
      ID_TAB == "POP_T0" &
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
  select(GEO_LABEL, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  collect() %>%
  mutate(part_pourcentage = (OBS_VALUE / sum(OBS_VALUE))) %>%
  mutate(population_exacte = round(part_pourcentage * OBS_VALUE))

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

nombre_total <- nombre_total %>%
  mutate(TAB_MEASURE_LABEL = factor(TAB_MEASURE_LABEL, levels = Ordre_Ages))

# 3 Création du diagramme en camembert
ggplot(nombre_total, aes(x = "", y = population_exacte, fill = TAB_MEASURE_LABEL)) +
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
    subtitle = "Source : Recensement Insee 2022 (Données redressées sur base de 67,9M)",
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


# REPARTITION DES DIFFERENTES TRANCHES D'AGE PAR VILLE

# ÉTAPE 1 : On calcule la population totale de chaque ville
pop_totale_villes <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Commune" & 
      TAB_MEASURE_LABEL == "Population" &
      ID_TAB == "POP_T1"
  ) %>%
  group_by(GEO_LABEL) %>%
  summarise(population_totale_ville = sum(OBS_VALUE, na.rm = TRUE))

# ÉTAPE 2 : On additionne les tranches d'âge par ville 
structure_villes_propres <- dossier_complet %>%
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
  # On regroupe les résultats par ville et par tranche d'âge
  group_by(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(total_ville_age = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  # On joint avec la population totale de chaque ville
  left_join(pop_totale_villes, by = "GEO_LABEL") %>%
  
  # On applique notre méthode de redressement 
  group_by(GEO_LABEL) %>%
  mutate(part_locale = total_ville_age / sum(total_ville_age)) %>%
  mutate(population_exacte_ville = round(part_locale * population_totale_ville)) %>%
  
  # Données finales
  select(GEO_LABEL, TAB_MEASURE_LABEL, population_exacte_ville) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

# Résultat
View(structure_villes_propres)


# REPARTITION DES DIFFERENTES TRANCHES D'AGE PAR DEPARTEMENT

# 1 : Population totale par département
pop_totale_dept <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
      TAB_MEASURE_LABEL == "Population" &
      ID_TAB == "POP_T1"
  ) %>%
  group_by(GEO_LABEL) %>%
  summarise(population_totale_dept = sum(OBS_VALUE, na.rm = TRUE))

# 2 : Calcul des tranches d'âge par département
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
  # On somme pour agglomérer toutes les lignes d'un même département
  group_by(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(total_brut_dept = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  # On associe la population globale du département
  left_join(pop_totale_dept, by = "GEO_LABEL") %>%
  
  # Calcul de la proportion
  group_by(GEO_LABEL) %>%
  mutate(part_departementale = total_brut_dept / sum(total_brut_dept)) %>%
  mutate(population_exacte_dept = round(part_departementale * population_totale_dept)) %>%
  
  # Sélection finale
  select(GEO_LABEL, TAB_MEASURE_LABEL, population_exacte_dept) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

View(structure_departements)


# REPARTITION DES DIFFERENTES TRANCHES D'AGE PAR REGION

# 1 : Population totale par région
pop_totale_region <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Région" & 
      TAB_MEASURE_LABEL == "Population" &
      ID_TAB == "POP_T1"
  ) %>%
  group_by(GEO_LABEL) %>%
  summarise(population_totale_reg = sum(OBS_VALUE, na.rm = TRUE))

# 2 : Calcul des tranches d'âge par région
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
  # On somme pour agglomérer toutes les lignes d'une même région
  group_by(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  summarise(total_brut_reg = sum(OBS_VALUE, na.rm = TRUE)) %>%
  
  # On associe la population globale de la région
  left_join(pop_totale_region, by = "GEO_LABEL") %>%
  
  # Calcul de la proportion
  group_by(GEO_LABEL) %>%
  mutate(part_regionale = total_brut_reg / sum(total_brut_reg)) %>%
  mutate(population_exacte_region = round(part_regionale * population_totale_reg)) %>%
  
  # Résultat final
  select(GEO_LABEL, TAB_MEASURE_LABEL, population_exacte_region) %>%
  arrange(GEO_LABEL, TAB_MEASURE_LABEL) %>%
  collect()

View(structure_regions)
