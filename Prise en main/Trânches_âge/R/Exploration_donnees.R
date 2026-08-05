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

#Résumé de la base de données
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

dossier_complet %>%
  count(GEO_OBJECT, sort = TRUE) %>%
  collect()

# Il y en a 13 différents. Ce sont des abréviations des différents types d'aires urbaines

dossier_complet %>%
  count(GEO_OBJECT_LABEL, sort = TRUE) %>%
  collect()
#Les noms des aires urbaines en toutes lettres, c'est ce qu'on nous propose de sélectionner sur le site internet

dossier_complet %>%
  count(GEO, sort = TRUE) %>%
  collect()
#id de communes, # Lorque l'on regarde quelles sont les communes qui se répètent le plus c'est
#un peu surprennant car ce ne sont pas nécessairement des communes parmis les plus grandes / attractives de France
# Par exemple la commune Saint-Etienne-du-Grès (13103) de 2489 habitants a n = 5376 entrées c'est ainsi la 6ème commune de France avec le plus d'entrées dans cette base de données 

commune_6 <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO == "13103" & 
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
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(population_brute = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  collect()
# Tableau du nombre de personnes par tranches d'âge
print(commune_6)
# Population bien plus grande dans notre résultat que dans la réalité. Globalement ici, on trouve que cette 
# commune a. On retrouve ici 459 953 habitants contre 2490 en réalité. 



dossier_complet %>%
  count(GEO_LABEL, sort = TRUE) %>%
  head(30) %>%
  collect()

#Noms d'aires urbaines en toutes lettres 

dossier_complet %>%
  count(GEO_REF, sort = TRUE) %>%
  collect()


dossier_complet %>%
  count(TIME_PERIOD, sort = TRUE) %>%
  collect()
# 22 années différentes 

dossier_complet %>%
  count(CONF_STATUS, sort = TRUE) %>%
  collect()
# 3 valeur spossibles : NA, F ou C

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

# 1 On sélectionne par communes afin d'éviter les doublons avec les dép et régions
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
  group_by(TAB_MEASURE_LABEL) %>% 
  summarise(total_brut = sum(OBS_VALUE, na.rm = TRUE)) %>% # Nombre de personnes par âges en France
  collect() %>%
#View(structure_reelle)
  # On calcule le pourcentage de chaque tranche d'âge
  mutate(part_pourcentage = (total_brut / sum(total_brut))) %>%
  
  # 2. ON détermine le nombre grâce à la population en 2022
  mutate(population_exacte = round(part_pourcentage * 68000000)) %>%
  select(TAB_MEASURE_LABEL, population_exacte)

# Tableau du nombre de personnes par tranches d'âge
print(nombre_total)


# Calcul de la répartition des différentes tranches d'âge par ville

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

# Calcul de la répartition des différentes tranches d'âge par département

# 1 : Population totale par département
pop_totale_dept <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Département" & 
      TAB_MEASURE_LABEL == "Population"
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


# Calcul de la répartition des différentes tranches d'âge par région

# 1 : Population totale par région
pop_totale_region <- dossier_complet %>%
  filter(
    TIME_PERIOD == "2022" & 
      GEO_OBJECT_LABEL == "Région" & 
      TAB_MEASURE_LABEL == "Population"
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
  group_by(TAB_MEASURE_LABEL) %>%
  summarise(total_brut = sum(OBS_VALUE, na.rm = TRUE)) %>%
  collect() %>%
  mutate(part_pourcentage = (total_brut / sum(total_brut))) %>%
  mutate(population_exacte = round(part_pourcentage * 68000000))

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
ggplot(structure_reelle, aes(x = "", y = population_exacte, fill = TAB_MEASURE_LABEL)) +
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

