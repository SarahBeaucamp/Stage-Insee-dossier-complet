# ==============================================================================
# PRÉPARATION DE LA BASE DE DONNÉES ET OPTIMISATION DE LA FORÊT ALÉATOIRE
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(rsample)
library(ranger) # Ajout pour la forêt aléatoire

# ------------------------------------------------------------------------------
# ÉTAPE 1 : CONNEXION ET EXTRACTION DES DONNÉES
# ------------------------------------------------------------------------------

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

# Selection des variables d'intérêt
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


# ------------------------------------------------------------------------------
# ÉTAPE 2 : PIVOT ET NETTOYAGE DES NOMS
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# ÉTAPE 3 : CRÉATION DU Y, DES TAUX ET GRANDE PURGE
# ------------------------------------------------------------------------------

# On conserve la variable "Population" ici pour pouvoir filtrer juste après
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
    across(.cols = starts_with("Logements...") | starts_with("Nombre.de.pièces..."), .fns = ~ .x / .data[["Logements"]]),
    
    # --- 3. MÉNAGES ---
    across(.cols = starts_with("Population.des.ménages...") & !matches("^Population.des.ménages$"), .fns = ~ .x / .data[["Population.des.ménages"]]),
    
    # --- 4. EMPLOIS ---
    across(.cols = starts_with("Nombre.d.emplois...") & !matches("^Nombre.d.emplois$"), .fns = ~ .x / .data[["Nombre.d.emplois"]]),
    
    # --- 5. POPULATION ACTIVE ---
    across(.cols = starts_with("Population...Actif.") & !matches("^Population...Actif$"), .fns = ~ .x / .data[["Population...Actif"]]),
    
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
    -Logements, -Population.des.ménages, -Population...Actif, -Nombre.d.emplois,
    
    # 1. Le piège de la soustraction (Statuts liés à l'activité)
    -contains("Actif", ignore.case = TRUE),
    -contains("Chômeur", ignore.case = TRUE),
    -contains("inactif", ignore.case = TRUE),
    -contains("foyer", ignore.case = TRUE),
    -contains("Retraité", ignore.case = TRUE),
    -contains("Élève", ignore.case = TRUE),
    
    # 2. Le piège des dénominateurs
    -matches("^Population.*Femme", ignore.case = TRUE),
    -matches("^Population.*Homme", ignore.case = TRUE)
  )


# ------------------------------------------------------------------------------
# ÉTAPE 4 : APPLICATION DU SEUIL (430) ET GESTION DES NAs
# ------------------------------------------------------------------------------

# --- LE FILTRE FINAL ET SEUIL À 430 ---
base_finale <- base_prete_rf %>%
  filter(Population >= 430) %>%              # Filtrage ciblé sur ton seuil de 430
  filter(!is.na(Y_GAP_ACT_GLOBAL)) %>%
  select(-GEO, -GEO_LABEL, -Population)      # On supprime la Population pour le modèle

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

# --- LE BOUCHAGE DES TROUS (Remplacement des NAs par 0) ---
base_sans_na <- base_finale %>%
  mutate(across(everything(), ~ replace_na(., 0)))

total_nas_restants <- sum(is.na(base_sans_na))
print(paste("Nombre total de NAs restants dans la base :", total_nas_restants))


# ------------------------------------------------------------------------------
# ÉTAPE 5 : ÉCHANTILLONNAGE (TRAIN / VAL / TEST)
# ------------------------------------------------------------------------------

# --- 1. SÉPARATION DE L'ÉCHANTILLON TEST (20%) ---
set.seed(42)
split_principal <- initial_split(base_sans_na, prop = 0.80)
base_train_val  <- training(split_principal) # 80% pour train + val
base_test       <- testing(split_principal)  # 20% pour le test final (Caché pour la fin)

# --- 2. SÉPARATION DU RESTE EN APPRENTISSAGE ET VALIDATION ---
set.seed(42)
split_interne <- initial_split(base_train_val, prop = 0.75)
base_train    <- training(split_interne) # ~60% du total
base_val      <- testing(split_interne)  # ~20% du total

print(paste("Taille Train :", nrow(base_train)))
print(paste("Taille Validation :", nrow(base_val)))
print(paste("Taille Test final :", nrow(base_test)))


# ------------------------------------------------------------------------------
# ÉTAPE 6 : OPTIMISATION DES HYPERPARAMÈTRES (GRID SEARCH AVEC SUIVI)
# ------------------------------------------------------------------------------

# 1. Création de la grille des combinaisons à tester
grille <- expand.grid(
  mtry = c(130, 135, 140, 145, 150),
  min.node.size = c(2, 3, 4, 5)
)

resultats_grille <- data.frame()
total_modeles <- nrow(grille)

print(paste("--- DÉBUT DU GRID SEARCH :", total_modeles, "MODÈLES À ENTRAÎNER ---"))
print("Estimation du temps : ~ 1h15 à 1h30")

# 2. La boucle d'optimisation
for(i in 1:total_modeles) {
  
  param_mtry <- grille$mtry[i]
  param_node <- grille$min.node.size[i]
  
  # Affichage de l'avancement avant le début du calcul
  print(paste("-> [", i, "/", total_modeles, "] En cours : mtry =", param_mtry, "| min.node.size =", param_node, "..."))
  
  # Entraînement avec la combinaison [i]
  modele_grid <- ranger(
    formula = Y_GAP_ACT_GLOBAL ~ ., 
    data = base_train,
    num.trees = 500,
    mtry = param_mtry,
    min.node.size = param_node,
    seed = 42 
  )
  
  # Prédiction sur la base de validation
  preds <- predict(modele_grid, data = base_val)$predictions
  
  # Calcul des erreurs
  rmse_val <- sqrt(mean((base_val$Y_GAP_ACT_GLOBAL - preds)^2))
  moy_train <- mean(base_train$Y_GAP_ACT_GLOBAL, na.rm = TRUE)
  rmse_naif <- sqrt(mean((base_val$Y_GAP_ACT_GLOBAL - moy_train)^2))
  
  # Calcul du R2
  r2_val <- (1 - (rmse_val^2 / rmse_naif^2)) * 100
  
  # Sauvegarde
  resultats_grille <- rbind(resultats_grille, data.frame(
    Modele_ID = i,
    mtry = param_mtry,
    min_node_size = param_node,
    RMSE_Val = rmse_val,
    R2_Estime_Pct = round(r2_val, 2)
  ))
  
  # Affichage du résultat de cette étape
  print(paste("   Terminé ! R2 obtenu :", round(r2_val, 2), "%"))
}

# 3. Tri des résultats pour afficher les meilleurs en haut
resultats_grille <- resultats_grille %>% arrange(desc(R2_Estime_Pct))

print("--- FIN DE L'OPTIMISATION : TOP 10 DES MEILLEURS PARAMÈTRES ---")
print(head(resultats_grille, 10))