# Préparation de la base de données avant de pouvoir procéder à la random forest

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Créer ou ouvrir une base DuckDB locale
con <- dbConnect(duckdb(), dbdir = "base.duckdb")

# 2. Charger les extensions nécessaires
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

# Créer une table virtuelle
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

# Etape 1 : Selection des variables d'intérêt
base_filtree <- dossier_complet %>%
  filter(
    GEO_OBJECT_LABEL == "Commune",
    str_detect(ID_TAB, "^FAM|^FOR|^EMP|^LOG") | 
      ID_TAB %in% c("POP_T8", "POP_T9", "REV_T1", "SAL_G1", "SAL_G3", 
                    "SAL_T1", "SAL_G4", "TOU_T1", "TOU_T2", "TOU_T3", 
                    "EQUIP_T1", "EQUIP_T2", "EQUIP_T3", "DEN_T1", "POP_T3", "POP_T0")
  ) %>%
  select(GEO, GEO_LABEL, ID_TAB, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  collect()

# Petite vérification du volume récupéré
print(paste("Nombre de lignes récupérées :", nrow(base_filtree)))
print(paste("Nombre de variables distinctes :", length(unique(base_filtree$TAB_MEASURE_LABEL))))

# Etape 2 : Pivot sur les colonnes de la base de données
library(tidyr)

base_large <- base_filtree %>%
  # 1. On force un seul nom de ville par code INSEE
  group_by(GEO) %>%
  mutate(GEO_LABEL = first(GEO_LABEL)) %>%
  ungroup() %>%
  
  # 2. Le pivot sécurisé avec les libellés en clair
  pivot_wider(
    id_cols = c(GEO, GEO_LABEL), 
    names_from = TAB_MEASURE_LABEL, 
    values_from = OBS_VALUE,
    values_fn = max              
  )

# On remplace les espaces, accents et caractères spéciaux par des points
names(base_large) <- make.names(names(base_large), unique = TRUE)

# Vérification des dimensions
print(paste("Nombre de communes :", nrow(base_large)))
print(paste("Nombre de colonnes :", ncol(base_large)))

# On stocke tous les noms de colonnes dans un vecteur pour chercher dedans
noms_colonnes <- names(base_large)
print(noms_colonnes)


# Etape 3 : Création des taux 

library(dplyr)

base_prete_rf <- base_large %>%
  mutate(
    # --- 1. CRÉATION DU NOUVEAU Y : L'ÉCART GLOBAL HOMMES-FEMMES ---
    
    # Calcul du taux des femmes = (Femmes en emploi + Femmes au chômage) / Population Femmes
    POP_FEMME = (.data[["Population...De.15.à.24.ans..x.Femme"]] + .data[["Population...De.25.à.39.ans.x.Femme"]] + .data[["Population...De.40.à.54.ans.x.Femme"]] + .data[["Population...De.55.à.64.ans.x.Femme"]]),
    
    TAUX_F = (.data[["Population...De.15.à.64.ans.x.Actif.occupé.x.Femme"]] + .data[["Population...De.15.à.64.ans.x.Chômeur.x.Femme"]]) / POP_FEMME,
    
    # Calcul du taux des hommes = (Hommes en emploi + Hommes au chômage) / Population Hommes
    POP_HOMME = (.data[["Population...De.15.à.24.ans..x.Homme"]] + .data[["Population...De.25.à.39.ans.x.Homme"]] + .data[["Population...De.40.à.54.ans.x.Homme"]] + .data[["Population...De.55.à.64.ans.x.Homme"]]),
    
    TAUX_H = (.data[["Population...De.15.à.64.ans.x.Actif.occupé.x.Homme"]] + .data[["Population...De.15.à.64.ans.x.Chômeur.x.Homme"]]) / POP_HOMME,
    
    # Ta véritable variable cible à prédire
    Y_GAP_ACT_GLOBAL = TAUX_H - TAUX_F,
    
    # --- 2. MACRO-DÉNOMINATEUR : LOGEMENTS ---
    across(
      .cols = matches("logement|r.sidence", ignore.case = TRUE) & 
        !matches("Logements", ignore.case = TRUE),
      .fns = ~ .x / .data[["Logements"]]
    ),
    
    # --- 3. MACRO-DÉNOMINATEUR : FAMILLES & MÉNAGES ---
    across(
      .cols = matches("famille|m.nage", ignore.case = TRUE) & 
        !matches("Logements|revenu", ignore.case = TRUE),
      .fns = ~ .x / .data[["Logements"]]
    ),
    
    # --- 4. MACRO-DÉNOMINATEUR : EMPLOI & FORMATION ---
    POP_ACT = (.data[["Population...De.15.à.64.ans.x.Actif.occupé"]] + .data[["Population...De.15.à.64.ans.x.Chômeur"]]),
    
    across(
      .cols = matches("emploi|ch.mage|actif|dipl.me|formation", ignore.case = TRUE) & 
        !matches("salaire|revenu|POP_ACT", ignore.case = TRUE),
      .fns = ~ .x / POP_ACT
    ),
    
    # --- 5. MACRO-DÉNOMINATEUR : POPULATION TOTALE ---
    across(
      .cols = matches(".quipement|tourisme|population", ignore.case = TRUE) & 
        !matches("salaire|revenu|densit.|Population", ignore.case = TRUE),
      .fns = ~ .x / .data[["Population"]]
    )
  ) %>%
  
  # --- SÉCURITÉ MATHÉMATIQUE ---
  mutate(across(everything(), ~ ifelse(is.infinite(.) | is.nan(.), 0, .))) %>%
  
  # --- PURGE DES VARIABLES INTERMÉDIAIRES ---
  # On supprime les taux isolés pour que le modèle ne triche pas et se concentre sur l'écart
  select(-TAUX_F, -TAUX_H, -POP_HOMME, -POP_FEMME, -POP_ACT)

# Vérification finale de ta cible
summary(base_prete_rf$Y_GAP_ACT_GLOBAL)

base_finale <- base_prete_rf %>%
  # On supprime toutes les lignes où notre cible est NA
  filter(!is.na(Y_GAP_ACT_GLOBAL)) %>%
  
  # On retire aussi les variables géographiques pour la modélisation
  select(-GEO, -GEO_LABEL)

# Vérification finale (le compteur de NAs sur ton Y devrait disparaître)
summary(base_finale$Y_GAP_ACT_GLOBAL)

head(base_finale)

# Détection de variables mal converties 

colonnes_suspectes <- base_finale %>%
  # On ne regarde que les colonnes numériques
  select(where(is.numeric)) %>%
  # On isole celles dont la valeur maximale dépasse 1.5 (on laisse une petite marge d'erreur)
  select(where(~ max(., na.rm = TRUE) > 1.5)) %>%
  names()

# On retire de cette liste les exceptions qu'on a VOLONTAIREMENT gardées en absolu
colonnes_suspectes <- colonnes_suspectes[!grepl("revenu|salaire|densit.", colonnes_suspectes, ignore.case = TRUE)]

# Affichage du résultat
print(paste("Nombre de variables suspectes détectées :", length(colonnes_suspectes)))
print(colonnes_suspectes)