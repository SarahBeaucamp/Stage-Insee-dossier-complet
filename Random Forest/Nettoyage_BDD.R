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
library(tidyr)

base_prete_rf <- base_large %>%
  mutate(
    # --- 1. CRÉATION DU Y ET AJUSTEMENTS D'ÉCHELLE ---
    Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie = .data[["Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie"]] / 100,
    
    POP_FEMME = (.data[["Population...De.15.à.24.ans..x.Femme"]] + .data[["Population...De.25.à.39.ans.x.Femme"]] + .data[["Population...De.40.à.54.ans.x.Femme"]] + .data[["Population...De.55.à.64.ans.x.Femme"]]),
    TAUX_F = (.data[["Population...De.15.à.64.ans.x.Actif.occupé.x.Femme"]] + .data[["Population...De.15.à.64.ans.x.Chômeur.x.Femme"]]) / POP_FEMME,
    
    POP_HOMME = (.data[["Population...De.15.à.24.ans..x.Homme"]] + .data[["Population...De.25.à.39.ans.x.Homme"]] + .data[["Population...De.40.à.54.ans.x.Homme"]] + .data[["Population...De.55.à.64.ans.x.Homme"]]),
    TAUX_H = (.data[["Population...De.15.à.64.ans.x.Actif.occupé.x.Homme"]] + .data[["Population...De.15.à.64.ans.x.Chômeur.x.Homme"]]) / POP_HOMME,
    
    Y_GAP_ACT_GLOBAL = TAUX_H - TAUX_F,
    
    # --- 2. LOGEMENTS ---
    across(
      .cols = starts_with("Logements...") | starts_with("Nombre.de.pièces..."),
      .fns = ~ .x / .data[["Logements"]]
    ),
    
    # --- 3. MÉNAGES ---
    across(
      .cols = starts_with("Population.des.ménages...") & !matches("^Population.des.ménages$"),
      .fns = ~ .x / .data[["Population.des.ménages"]]
    ),
    
    # --- 4. EMPLOIS ---
    across(
      .cols = starts_with("Nombre.d.emplois...") & !matches("^Nombre.d.emplois$"),
      .fns = ~ .x / .data[["Nombre.d.emplois"]]
    ),
    
    # --- 5. POPULATION ACTIVE ---
    across(
      .cols = starts_with("Population...Actif.") & !matches("^Population...Actif$"),
      .fns = ~ .x / .data[["Population...Actif"]]
    ),
    
    # --- 6. POPULATION TOTALE (Correction du "s" à Nombre.de.place) ---
    across(
      .cols = (starts_with("Population...") | starts_with("Établissements...") | starts_with("Nombre.de.place") | starts_with("Nombre.d.équipements...") | starts_with("Nombre.de.nouvelles") | starts_with("Nombre.de.personnes.seules...") | starts_with("Nombre.de.famille")) & 
        !matches("^Population$|^Population.des.ménages$|^Population...Actif$|^Population...Actif."),
      .fns = ~ .x / .data[["Population"]]
    )
  ) %>%
  
  # --- SÉCURITÉ MATHÉMATIQUE ---
  mutate(across(everything(), ~ ifelse(is.infinite(.) | is.nan(.), 0, .))) %>%
  
  # --- LA GRANDE PURGE ---
  select(
    -TAUX_F, -TAUX_H, -POP_FEMME, -POP_HOMME, 
    -Population, -Logements, -Population.des.ménages, -Population...Actif, -Nombre.d.emplois
  )

# --- LE FILTRE FINAL ---
base_finale <- base_prete_rf %>%
  filter(!is.na(Y_GAP_ACT_GLOBAL)) %>%
  select(-GEO, -GEO_LABEL)

# --- DIAGNOSTIC DES VALEURS MANQUANTES (NAs) ---
compte_na <- colSums(is.na(base_finale))
valeurs_manquantes <- compte_na[compte_na > 0]
print("--- BILAN DES NAs RESTANTS ---")
print(valeurs_manquantes)

# --- DÉTECTION DE VARIABLES MAL CONVERTIES ---
colonnes_suspectes <- base_finale %>%
  select(where(is.numeric)) %>%
  select(where(~ max(., na.rm = TRUE) > 1)) %>%
  names()

colonnes_suspectes <- colonnes_suspectes[!grepl("revenu|salaire|densit.", colonnes_suspectes, ignore.case = TRUE)]

print(paste("Nombre de variables suspectes détectées :", length(colonnes_suspectes)))
print(colonnes_suspectes)

# --- LE BOUCHAGE DES TROUS (Remplacement des NAs par 0) ---
base_sans_na <- base_finale %>%
  mutate(across(everything(), ~ replace_na(., 0)))

total_nas_restants <- sum(is.na(base_sans_na))
print(paste("Nombre total de NAs restants dans la base :", total_nas_restants))

head(base_sans_na)

# Résumé statistique de 4 variables de création d'entreprises
base_sans_na %>%
  # On isole toutes les colonnes de créations d'unités légales
  select(starts_with("Nombre.de.nouvelles.unités.légales.enregistrées")) %>%
  # On ne garde que les 4 premières pour que ce soit lisible
  select(1:4) %>%
  summary()

# Affichage du résumé statistique exclusif des 27 variables
base_sans_na %>%
  select(all_of(colonnes_suspectes)) %>%
  summary()


# ____________DEBUT RANDOM FOREST ________________

# Echantillonnage
install.packages("rsample")
library(rsample)

# --- 1. SÉPARATION DE L'ÉCHANTILLON TEST (20%) ---
set.seed(42)
split_principal <- initial_split(base_sans_na, prop = 0.80)
base_train_val  <- training(split_principal) # 80% pour train + val
base_test       <- testing(split_principal)  # 20% pour le test final

# --- 2. SÉPARATION DU RESTE EN APPRENTISSAGE ET VALIDATION ---
# Sur les 80% restants, on met 75% pour train (soit 60% du total) et 25% pour val (soit 20% du total)
set.seed(42)
split_interne <- initial_split(base_train_val, prop = 0.75)
base_train    <- training(split_interne) # 60% du total
base_val      <- testing(split_interne)  # 20% du total

# Vérification des tailles pour s'assurer que tout est correct
print(paste("Taille Train :", nrow(base_train)))
print(paste("Taille Validation :", nrow(base_val)))
print(paste("Taille Test :", nrow(base_test)))


install.packages("ranger")
library(ranger)

# --- 1. ENTRAÎNEMENT DU MODÈLE DE BASE SUR `base_train` ---
print("Entraînement de la forêt aléatoire de base...")

modele_base <- ranger(
  formula = Y_GAP_ACT_GLOBAL ~ ., 
  data = base_train,
  num.trees = 500,               # 500 arbres par défaut
  importance = 'impurity'        # Pour pouvoir analyser l'importance des variables plus tard
)

# --- 2. ÉVALUATION SUR L'ÉCHANTILLON DE VALIDATION (`base_val`) ---
# On prédit les valeurs pour l'échantillon de validation
predictions_val <- predict(modele_base, data = base_val)

# On calcule l'erreur quadratique moyenne (MSE) sur la validation
# (En lien avec la perte quadratique de ton cours de régression)
mse_val <- mean((base_val$Y_GAP_ACT_GLOBAL - predictions_val$predictions)^2)
rmse_val <- sqrt(mse_val)

print(paste("MSE sur l'échantillon de validation :", round(mse_val, 5)))
print(paste("RMSE sur l'échantillon de validation :", round(rmse_val, 5)))

# Affichage du résumé du modèle
print(modele_base)