# ==============================================================================
# PRÉPARATION DE LA BASE DE DONNÉES 2023 ET PROJECTION (DATA DRIFT)
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)
library(tidyr)
install.packages("missForest")
library(missForest) 
install.packages("Metrics")
library(Metrics)    

# ------------------------------------------------------------------------------
# ÉTAPE 1 : CONNEXION ET EXTRACTION DES DONNÉES 2023
# ------------------------------------------------------------------------------

con <- dbConnect(duckdb(), dbdir = "base.duckdb")
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "INSTALL aws;")
dbExecute(con, "LOAD aws;")

dbExecute(con, "CREATE OR REPLACE SECRET (
TYPE S3,
PROVIDER CREDENTIAL_CHAIN,
ENDPOINT 'minio.lab.sspcloud.fr',
URL_STYLE 'path'
);")

chemin_s3_2023 <- "s3://sarahbeaucamp/dossier_complet_2023.parquet"
# CORRECTION : Utilisation de chemin_s3_2023
dossier_complet_2023 <- tbl(con, paste0("read_parquet('", chemin_s3_2023, "')"))

base_filtree_2023 <- dossier_complet_2023 %>%
  filter(
    GEO_OBJECT_LABEL == "Commune",
    str_detect(ID_TAB, "^EMP|^EQUIP|^TOU") | 
      ID_TAB %in% c("POP_T7", "POP_T8", "REV_T1", "SAL_G1", "SAL_G3", 
                    "SAL_T1", "SAL_G4", "DEN_T1", "DEN_T3", "DEN_T4", "RES_T3", 
                    "RES_T5", "POP_T4", "POP_T1", "LOG_T10", "FOR_T1", "FOR_T2", "FOR_T3",
                    "FAM_T1", "FAM_T2", "FAM_T3", "FAM_T4", "FAM_T5", "FAM_G1", "FAM_G2", "FAM_G4", "POP_T9")
  ) %>%
  select(GEO, GEO_LABEL, ID_TAB, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  collect()

print("Étape 1 terminée : Données 2023 extraites.")

# ------------------------------------------------------------------------------
# ÉTAPE 2 (2023) : PIVOT ET ALIGNEMENT STRICT SUR LA NOMENCLATURE 2022
# ------------------------------------------------------------------------------

base_large_2023 <- base_filtree_2023 %>%
  group_by(GEO) %>%
  mutate(GEO_LABEL = first(GEO_LABEL)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(GEO, GEO_LABEL), 
    names_from = TAB_MEASURE_LABEL, 
    values_from = OBS_VALUE,
    values_fn = max            
  ) %>%
  # ==============================================================================
# 0. ALIGNEMENT MANUEL SUR LES NOMS BRUTS (POUR LES 75 VARIABLES MODIFIÉES)
# ==============================================================================
rename(
  
  # --- POPULATION, ÂGE ET CSP ---
  "Population - De 15 à 24 ans x Agriculteurs" = "Population – De 15 à 24 ans * Agriculteurs",
  "Population - De 25 à 54 ans x Agriculteurs" = "Population – De 25 à 54 ans * Agriculteurs",
  "Population - 55 ans ou plus x Agriculteurs" = "Population – 55 ans ou plus * Agriculteurs",
  
  "Population - De 15 à 24 ans x Artisans, commerçants et chefs d'entreprise" = "Population – De 15 à 24 ans * Artisans, commerçants et chefs d’entreprise",
  "Population - De 25 à 54 ans x Artisans, commerçants et chefs d'entreprise" = "Population – De 25 à 54 ans * Artisans, commerçants et chefs d’entreprise",
  "Population - 55 ans ou plus x Artisans, commerçants et chefs d'entreprise" = "Population – 55 ans ou plus * Artisans, commerçants et chefs d’entreprise",
  
  "Population - De 15 à 24 ans x Cadres et professions intellectuelles supérieures" = "Population – De 15 à 24 ans * Cadres et professions intellectuelles supérieures",
  "Population - De 25 à 54 ans x Cadres et professions intellectuelles supérieures" = "Population – De 25 à 54 ans * Cadres et professions intellectuelles supérieures",
  "Population - 55 ans ou plus x Cadres et professions intellectuelles supérieures" = "Population – 55 ans ou plus * Cadres et professions intellectuelles supérieures",
  
  "Population - De 15 à 24 ans x Professions intermédiaires" = "Population – De 15 à 24 ans * Professions intermédiaires",
  "Population - De 25 à 54 ans x Professions intermédiaires" = "Population – De 25 à 54 ans * Professions intermédiaires",
  "Population - 55 ans ou plus x Professions intermédiaires" = "Population – 55 ans ou plus * Professions intermédiaires",
  
  "Population - De 15 à 24 ans x Employés" = "Population – De 15 à 24 ans * Employés",
  "Population - De 25 à 54 ans x Employés" = "Population – De 25 à 54 ans * Employés",
  "Population - 55 ans ou plus x Employés" = "Population – 55 ans ou plus * Employés",
  
  "Population - De 15 à 24 ans x Ouvriers" = "Population – De 15 à 24 ans * Ouvriers",
  "Population - De 25 à 54 ans x Ouvriers" = "Population – De 25 à 54 ans * Ouvriers",
  "Population - 55 ans ou plus x Ouvriers" = "Population – 55 ans ou plus * Ouvriers",  
  # --- ÉTAT CIVIL ---
  "Population - 15 ans ou plus x Célibataire" = "Population – Célibataire",
  "Population - 15 ans ou plus x Marié" = "Population – Marié",
  "Population - 15 ans ou plus x Pacsé" = "Population – Pacsé",
  "Population - 15 ans ou plus x Divorcé" = "Population – Divorcé",
  "Population - 15 ans ou plus x Veuf" = "Population – Veuf",
  "Population - 15 ans ou plus x En concubinage, union libre" = "Population – En concubinage, union libre", 
  
  # --- DIPLÔMES ---
  "Population - Aucun diplôme" = "Population – Aucun diplôme ou certificat d'études primaires",
  "Population - Aucun diplôme ou BEPC, brevet des collèges, DNB" = "Population – BEPC, brevet élémentaire, brevet des collèges, DNB",
  "Population - CEP (certificat d'études primaires)" = "Population – CAP, BEP ou diplôme de niveau équivalent",
  "Population - Diplôme universitaire 2e ou 3e cycle" = "Population – Baccalauréat universitaire ou équivalent : Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4",
  
  # --- LOGEMENTS ET STRUCTURE FAMILIALE ---
  "Logements - Résidences principales" = "Logements – Résidences principales",
  "Logements - Homme seul" = "Population des ménages – Homme seul",
  "Logements - Femme seule" = "Population des ménages – Femme seule",
  "Logements - Ménage à une personne" = "Population des ménages – Ménage à une personne",
  "Logements - Ménage sans famille à plusieurs personnes" = "Population des ménages – Ménage sans famille à plusieurs personnes",
  "Logements - Ménage comprenant une famille" = "Population des ménages – Ménage comprenant une famille",
  "Logements - Famille principale monoparentale" = "Population des ménages – Famille principale monoparentale",
  "Logements - Famille principale couple sans enfants" = "Population des ménages – Famille principale couple sans enfants",
  "Logements - Famille principale couple avec enfants" = "Population des ménages – Famille principale couple avec enfants",
  
  # --- NOMBRE DE FAMILLES (ENFANTS) ---
  "Nombre de famille - Aucun enfant de moins de 24 ans" = "Nombre de famille – Aucun enfant de moins de 24 ans",
  "Nombre de famille - 1 enfant de moins de 24 ans" = "Nombre de famille – 1 enfant de moins de 24 ans",
  "Nombre de famille - 2 enfants de moins de 24 ans" = "Nombre de famille – 2 enfants de moins de 24 ans",
  "Nombre de famille - 3 enfants de moins de 24 ans" = "Nombre de famille – 3 enfants de moins de 24 ans",
  "Nombre de famille - 4 enfants ou plus de moins de 24 ans" = "Nombre de famille – 4 enfants ou plus de moins de 24 ans",
  
  # --- PERSONNES SEULES PAR ÂGE ---
  "Nombre de personnes seules - De 15 à 24 ans" = "Population des ménages – De 15 à 24 ans",
  "Nombre de personnes seules - De 25 à 39 ans" = "Population des ménages – De 25 à 39 ans",
  "Nombre de personnes seules - De 40 à 54 ans" = "Population des ménages – De 40 à 54 ans",
  "Nombre de personnes seules - De 55 à 64 ans" = "Population des ménages – De 55 à 64 ans",
  "Nombre de personnes seules - De 65 à 79 ans" = "Population des ménages – De 65 à 79 ans",
  "Nombre de personnes seules - 80 ans ou plus" = "Population des ménages – 80 ans ou plus",
  
  "Nombre de personnes seules - 1 personne x De 15 à 24 ans" = "Nombre de personnes seules – De 15 à 24 ans * 1 personne",
  "Nombre de personnes seules - 1 personne x De 25 à 39 ans" = "Nombre de personnes seules – De 25 à 39 ans * 1 personne",
  "Nombre de personnes seules - 1 personne x De 40 à 54 ans" = "Nombre de personnes seules – De 40 à 54 ans * 1 personne",
  "Nombre de personnes seules - 1 personne x De 55 à 64 ans" = "Nombre de personnes seules – De 55 à 64 ans * 1 personne",
  "Nombre de personnes seules - 1 personne x De 65 à 79 ans" = "Nombre de personnes seules – De 65 à 79 ans * 1 personne",
  "Nombre de personnes seules - 1 personne x 80 ans ou plus" = "Nombre de personnes seules – 80 ans ou plus * 1 personne",
  
  # --- EMPLOIS ET SECTEURS ---
  "Nombre d'emplois - Salariés x Femme" = "Nombre d’emplois – Femme * Salariés",
  "Nombre d'emplois - Non Salariés x Femme" = "Nombre d’emplois – Femme * Non Salariés",
  "Nombre d'emplois - Salariés x Temps partiel" = "Nombre d’emplois – Temps partiel * Salariés",
  "Nombre d'emplois - Non Salariés x Temps partiel" = "Nombre d’emplois – Temps partiel * Non Salariés",
  
  "Nombre d'emplois - Salariés x Femme x Agriculture, sylviculture et pêche" = "Nombre d’emplois – Femme * Salariés * Agriculture, sylviculture et pêche",
  "Nombre d'emplois - Non Salariés x Femme x Agriculture, sylviculture et pêche" = "Nombre d’emplois – Femme * Non Salariés * Agriculture, sylviculture et pêche",
  
  "Nombre d'emplois - Salariés x Femme x Construction" = "Nombre d’emplois – Femme * Salariés * Construction",
  "Nombre d'emplois - Non Salariés x Femme x Construction" = "Nombre d’emplois – Femme * Non Salariés * Construction",
  
  "Nombre d'emplois - Salariés x Femme x Industrie manufacturière, industries extractives et autres" = "Nombre d’emplois – Femme * Salariés * Industrie manufacturière, industries extractives et autres",
  "Nombre d'emplois - Non Salariés x Femme x Industrie manufacturière, industries extractives et autres" = "Nombre d’emplois – Femme * Non Salariés * Industrie manufacturière, industries extractives et autres",
  
  "Nombre d'emplois - Salariés x Femme x Services principalement marchands" = "Nombre d’emplois – Femme * Salariés * Services principalement marchands",
  "Nombre d'emplois - Non Salariés x Femme x Services principalement marchands" = "Nombre d’emplois – Femme * Non Salariés * Services principalement marchands",
  
  "Nombre d'emplois - Salariés x Femme x Administration publique, enseignement, santé humaine et action sociale" = "Nombre d’emplois – Femme * Salariés * Administration publique, enseignement, santé humaine et action sociale",
  "Nombre d'emplois - Non Salariés x Femme x Administration publique, enseignement, santé humaine et action sociale" = "Nombre d’emplois – Femme * Non Salariés * Administration publique, enseignement, santé humaine et action sociale"
)

# 1. On applique le formatage R classique sur 2023
names(base_large_2023) <- make.names(names(base_large_2023), unique = TRUE)

# ==============================================================================
# CORRECTION DE SCHÉMA : FORCER LES NOMS DE 2023 À COPIER CEUX DE 2022
# ==============================================================================

# 2. On récupère les noms officiels de ta base 2022 (qui doivent être dans ton environnement)
noms_2022_ref <- names(base_large) 
noms_2023_actuels <- names(base_large_2023)

# 3. Fonction pour extraire l'essence du texte (sans point, sans x de liaison, sans casse)
nettoyer_texte <- function(noms) {
  noms <- tolower(noms)
  # On retire les 'x' qui servent de croisements dans les noms R (ex: .x.)
  noms <- gsub("\\.x\\.", "", noms)
  # On retire absolument toute la ponctuation et les espaces
  noms <- gsub("[^a-z0-9éèàâêîôû]", "", noms)
  return(noms)
}

# 4. On crée les dictionnaires nettoyés en coulisses
noms_2022_propres <- nettoyer_texte(noms_2022_ref)
noms_2023_propres <- nettoyer_texte(noms_2023_actuels)

# 5. Remplacement conditionnel : on écrase 2023 par le nom strict de 2022 si l'essence matche
for (i in seq_along(noms_2023_actuels)) {
  # On cherche à quelle position dans 2022 correspond le nom nettoyé de 2023
  index_correspondant <- match(noms_2023_propres[i], noms_2022_propres)
  
  if (!is.na(index_correspondant)) {
    # Si correspondance trouvée, on remplace le nom 2023 par le nom officiel 2022
    names(base_large_2023)[i] <- noms_2022_ref[index_correspondant]
  }
}

print("Étape 2 terminée : Base pivotée, renommée manuellement, et nomenclatures 2023 rigoureusement alignées sur 2022.")
# ------------------------------------------------------------------------------
# ÉTAPE 3 : CRÉATION DU Y, DES TAUX ET GRANDE PURGE
# ------------------------------------------------------------------------------

base_prete_rf_2023 <- base_large_2023 %>%
  mutate(
    Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie = .data[["Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie"]] / 100,
    
    # 1. Somme des tranches d'âge pour la population totale (15-64 ans)
    POP_FEMME = (.data[["Population...Femme...De.15.à.24.ans"]] + 
                   .data[["Population...Femme...De.25.à.39.ans"]] + 
                   .data[["Population...Femme...De.40.à.54.ans"]] + 
                   .data[["Population...Femme...De.55.à.64.ans"]]),
    
    POP_HOMME = (.data[["Population...Homme...De.15.à.24.ans"]] + 
                   .data[["Population...Homme...De.25.à.39.ans"]] + 
                   .data[["Population...Homme...De.40.à.54.ans"]] + 
                   .data[["Population...Homme...De.55.à.64.ans"]]),
    
    # 2. Somme des actifs occupés
    TAUX_F = (.data[["Population...Femme...Actif.occupé...De.15.à.24.ans"]] + 
                .data[["Population...Femme...Actif.occupé...De.25.à.54.ans"]] + 
                .data[["Population...Femme...Actif.occupé...De.55.à.64.ans"]]) / POP_FEMME,
    
    TAUX_H = (.data[["Population...Homme...Actif.occupé...De.15.à.24.ans"]] + 
                .data[["Population...Homme...Actif.occupé...De.25.à.54.ans"]] + 
                .data[["Population...Homme...Actif.occupé...De.55.à.64.ans"]]) / POP_HOMME,
    
    Y_GAP_ACT_GLOBAL = TAUX_H - TAUX_F,
    
    # CORRECTION : Utilisation de la résidence principale comme base pour les logements
    across(.cols = starts_with("Logements...") | starts_with("Nombre.de.pièces..."), .fns = ~ .x / .data[["Logements...Résidences.principales"]]),
    
    across(.cols = starts_with("Population.des.ménages...") & !matches("^Population.des.ménages$"), .fns = ~ .x / .data[["Population.des.ménages"]]),
    across(.cols = starts_with("Nombre.d.emplois...") & !matches("^Nombre.d.emplois$"), .fns = ~ .x / .data[["Nombre.d.emplois"]]),
    across(.cols = starts_with("Population...Actif.") & !matches("^Population...Actif$"), .fns = ~ .x / .data[["Population...Actif"]]),
    across(
      .cols = (starts_with("Population...") | starts_with("Établissements...") | starts_with("Nombre.de.place") | starts_with("Nombre.d.équipements...") | starts_with("Nombre.de.nouvelles") | starts_with("Nombre.de.personnes.seules...") | starts_with("Nombre.de.famille")) & 
        !matches("^Population$|^Population.des.ménages$|^Population...Actif$|^Population...Actif."),
      .fns = ~ .x / .data[["Population"]]
    )
  ) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.) | is.nan(.), NA, .))) %>% 
  select(
    -TAUX_F, -TAUX_H, -POP_FEMME, -POP_HOMME,
    -Logements...Résidences.principales, -Population.des.ménages, -Population...Actif, -Nombre.d.emplois,
    -contains("Actif", ignore.case = TRUE),
    -contains("Chômeur", ignore.case = TRUE),
    -contains("inactif", ignore.case = TRUE),
    -contains("foyer", ignore.case = TRUE),
    -contains("Retraité", ignore.case = TRUE),
    -contains("Élève", ignore.case = TRUE),
    -matches("^Population.*Femme", ignore.case = TRUE),
    -matches("^Population.*Homme", ignore.case = TRUE)
    # -matches("^Population.*ans\\.?$", ignore.case = TRUE) # Commenté pour garder l'âge
  )

print("Étape 3 terminée : Purge et calculs effectués.")

# ------------------------------------------------------------------------------
# ÉTAPE 4 : FILTRE DE POPULATION ET TRAITEMENT HYBRIDE DES NAs
# ------------------------------------------------------------------------------

base_pre_filtre_2023 <- base_prete_rf_2023 %>%
  filter(Population >= 500) %>%             
  filter(!is.na(Y_GAP_ACT_GLOBAL)) %>%
  select(-Population)     

# CORRECTION : Remplacement de base_pre_filtre par base_pre_filtre_2023 dans le grepl
colonnes_secret_2023 <- names(base_pre_filtre_2023)[grepl("salaire|pauvret|revenu|niveau.de.vie", names(base_pre_filtre_2023), ignore.case = TRUE)]

print("Colonnes identifiées pour l'imputation mathématique :")
print(colonnes_secret_2023)

base_finale_2023 <- base_pre_filtre_2023 %>%
  mutate(across(-all_of(c(colonnes_secret_2023, "GEO", "GEO_LABEL")), ~ replace_na(., 0)))

compte_na_2023 <- colSums(is.na(base_finale_2023))
print("--- BILAN DES NAs APRÈS MISE À ZÉRO DES ÉQUIPEMENTS ---")
print(compte_na_2023[compte_na_2023 > 0])

referentiel_communes_2023 <- base_finale_2023 %>% select(GEO, GEO_LABEL)

# ==============================================================================
# ÉTAPE 4B : ALIGNEMENT DES COLONNES DU MODÈLE (RETRAIT DES VARIABLES DISPARUES)
# ==============================================================================

# 1. On identifie les colonnes communes entre l'entraînement (2022) et la prédiction (2023)
colonnes_communes <- intersect(names(base_sans_na), names(base_finale_2023))

# 2. On regarde ce qui va être retiré pour ton information
variables_supprimees_du_modele <- setdiff(names(base_sans_na), colonnes_communes)
print(paste("Nombre de variables retirées du modèle car disparues en 2023 :", length(variables_supprimees_du_modele)))
print(variables_supprimees_du_modele)

# 3. On filtre définitivement les deux bases pour qu'elles aient EXACTEMENT les mêmes colonnes
base_sans_na <- base_sans_na %>% select(all_of(colonnes_communes))
base_finale_2023 <- base_finale_2023 %>% select(all_of(colonnes_communes))

# (Ton Étape 5 de test de sécurité commence juste en dessous...)

# ==============================================================================
# ÉTAPE 6A : IMPUTATION DÉFINITIVE AVEC MISSFOREST
# ==============================================================================

print("--- 1. CONVERSION STRICTE DU FORMAT ---")
base_finale_propre_2023 <- base_finale_2023 %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.data.frame()

print("--- 2. APPLICATION DE MISSFOREST ---")
imputation_finale_2023 <- missForest(base_finale_propre_2023, ntree = 50, maxiter = 5)

# CORRECTION : Harmonisation du nom de la base finale
base_2023_sans_na <- imputation_finale_2023$ximp

print(paste("Nombre total de NAs restants :", sum(is.na(base_2023_sans_na))))

#===============================================================================
# ETAPE 7 : VÉRIFICATION DES VARIABLES MANQUANTES
#===============================================================================

# On suppose que base_sans_na (2022) est toujours dans l'environnement
variables_manquantes <- setdiff(names(base_sans_na), names(base_2023_sans_na))
print("Variables présentes en 2022 mais absentes en 2023 :")
print(variables_manquantes)

#===============================================================================
# ETAPE 8 : FORET ALEATOIRE ALGO CREATION
#===============================================================================

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

#_______________ RANDOM FOREST SIMPLE _____________________

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


#===============================================================================
# ETAPE 9 : PRÉDICTION SUR 2023 (INFERENCE)
#===============================================================================

print("--- 1. PRÉDICTION SUR LE JEU DE DONNÉES 2023 ---")

# On applique le modèle entraîné (modele_base) sur les nouvelles données 2023
predictions_2023 <- predict(modele_base, data = base_2023_sans_na)$predictions 

print("--- 2. ÉVALUATION DE LA ROBUSTESSE (DATA DRIFT) ---")

# Calcul du R2 sur les données de 2023
r2_2023 <- cor(predictions_2023, base_2023_sans_na$Y_GAP_ACT_GLOBAL)^2

# Calcul de la RMSE sur 2023
rmse_2023 <- rmse(base_2023_sans_na$Y_GAP_ACT_GLOBAL, predictions_2023)

print(paste("Performance R2 sur 2023 :", round(r2_2023, 4)))
print(paste("Erreur RMSE sur 2023 :", round(rmse_2023, 4)))

# ==============================================================================
# PARTIE 10 : SHAP VALUES 2023 POUR LE RANDOM FOREST (VILLE DE NANTES)
# ==============================================================================

install.packages("kernelshap")
install.packages("shapviz")
library(kernelshap)
library(shapviz)
library(ggplot2)

print("--- ANALYSE RANDOM FOREST LOCALE : NANTES (2023) ---")

code_insee_cible <- "44109" 
index_commune <- which(referentiel_communes_2023$GEO == code_insee_cible)

if(length(index_commune) > 0) {
  
  nom_commune <- referentiel_communes_2023$GEO_LABEL[index_commune]
  print(paste("--- COMMUNE TROUVÉE :", nom_commune, "---"))
  
  # A. Extraction de la ligne pour Nantes (uniquement Nantes !)
  X_2023_features <- base_2023_sans_na %>% select(-Y_GAP_ACT_GLOBAL)
  X_nantes_rf <- X_2023_features[index_commune, , drop = FALSE]
  
  # B. PRÉDICTION RANDOM FOREST POUR NANTES
  prediction_nantes_rf <- predict(modele_base, data = X_nantes_rf)$predictions
  print(paste("=> L'écart d'activité prédit par le Random Forest pour", nom_commune, "est de :", round(prediction_nantes_rf, 4)))
  
  # C. CALCUL SHAP ULTRA-ALLÉGÉ (20 lignes de fond pour éliminer tout risque de crash)
  set.seed(42)
  bg_rf <- base_train %>% select(-Y_GAP_ACT_GLOBAL) %>% sample_n(20)
  
  predict_ranger_simple <- function(modele, newdata) {
    predict(modele, data = as.data.frame(newdata))$predictions
  }
  
  shap_nantes_rf <- kernelshap(
    modele_base, 
    X = X_nantes_rf, 
    bg_X = bg_rf, 
    pred_fun = predict_ranger_simple
  )
  
  # D. AFFICHAGE DU GRAPHIQUE WATERFALL POUR NANTES
  sv_nantes_rf <- shapviz(shap_nantes_rf)
  
  graphique_cascade_rf <- sv_waterfall(sv_nantes_rf, max_display = 15) +
    ggtitle(paste("SHAP Values (Random Forest) - Décomposition pour", nom_commune)) +
    theme_minimal()
  
  print(graphique_cascade_rf)
  
} else {
  print("❌ La commune n'a pas été trouvée dans le référentiel.")
}

# ==============================================================================
# PARTIE 11 : PRÉDICTION SUR 2023 AVEC LE MODÈLE LASSO (INFERENCE)
# ==============================================================================

print("--- 1. NORMALISATION ET PRÉPARATION DE LA MATRICE 2023 ---")

# NORMALISATION : On applique la même règle de centrage/réduction sur 2023
base_normalisee_2023 <- base_2023_sans_na %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

# Séparation de la cible 2023
vecteur_Y_2023 <- base_normalisee_2023$Y_GAP_ACT_GLOBAL

# Transformation en matrice (doit avoir exactement les mêmes colonnes que matrice_X)
matrice_X_2023 <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee_2023)

print("--- 2. PRÉDICTION SUR LE JEU DE DONNÉES 2023 ---")

# On applique le modèle Lasso (qui a le meilleur lambda) sur la matrice 2023
predictions_2023_lasso <- predict(modele_lasso_cv, s = meilleur_lambda, newx = matrice_X_2023)

print("--- 3. ÉVALUATION DE LA ROBUSTESSE (DATA DRIFT) ---")

# Calcul du R2 sur les données de 2023
# (On utilise as.vector pour s'assurer que c'est un format compatible avec cor())
r2_2023_lasso <- cor(as.vector(predictions_2023_lasso), vecteur_Y_2023)^2

# Calcul de la RMSE sur 2023
rmse_2023_lasso <- rmse(vecteur_Y_2023, as.vector(predictions_2023_lasso))

print(paste("Performance R2 Lasso sur 2023 :", round(r2_2023_lasso, 4)))
print(paste("Erreur RMSE Lasso sur 2023 :", round(rmse_2023_lasso, 4)))

print("--- BILAN : COMPARAISON RANDOM FOREST vs LASSO ---")
print(paste("RMSE Random Forest :", round(rmse_2023, 4)))
print(paste("RMSE Lasso :", round(rmse_2023_lasso, 4)))

#===============================================================================
# PARTIE 12 : SHAP VALUES 2023 (COMMUNES)
#===============================================================================

library(kernelshap)
library(shapviz)
library(ggplot2)

print("--- ANALYSE LASSOLOCALE : VILLE DE NANTES (2023) ---")

code_insee_cible <- "44109" 
index_commune <- which(referentiel_communes_2023$GEO == code_insee_cible)

if(length(index_commune) > 0) {
  
  nom_commune <- referentiel_communes_2023$GEO_LABEL[index_commune]
  print(paste("--- COMMUNE TROUVÉE :", nom_commune, "---"))
  
  # A. Extraction de la ligne normalisée pour Nantes
  # On s'assure d'avoir exactement les mêmes colonnes que la matrice d'entraînement du Lasso
  colonnes_lasso <- colnames(matrice_X)
  
  # On ajuste les colonnes de Nantes sur celles du modèle Lasso (au cas où il y aurait un décalage)
  X_nantes_lasso <- matrice_X_2023[index_commune, , drop = FALSE]
  
  # B. PRÉDICTION LASSO POUR NANTES
  prediction_nantes_lasso <- predict(modele_lasso_cv, s = meilleur_lambda, newx = X_nantes_lasso)
  print(paste("=> L'écart d'activité prédit par le Lasso pour", nom_commune, "est de :", round(as.numeric(prediction_nantes_lasso), 4)))
  
  # C. CALCUL DES SHAP VALUES POUR LE MODÈLE LINÉAIRE (LASSO)
  # Pour un modèle linéaire, kernelshap marche à la perfection et sans aucun risque de saturation
  set.seed(42)
  bg_lasso <- matrice_X[sample(nrow(matrice_X), 50), ] # Petit échantillon de référence
  
  predict_lasso_simple <- function(modele, newdata) {
    predict(modele, s = meilleur_lambda, newx = as.matrix(newdata))
  }
  
  shap_nantes_lasso <- kernelshap(
    modele_lasso_cv, 
    X = X_nantes_lasso, 
    bg_X = bg_lasso, 
    pred_fun = predict_lasso_simple
  )
  
  # D. AFFICHAGE DU GRAPHIQUE WATERFALL POUR LE LASSO
  sv_nantes_lasso <- shapviz(shap_nantes_lasso)
  
  graphique_cascade_lasso <- sv_waterfall(sv_nantes_lasso, max_display = 15) +
    ggtitle(paste("SHAP Values (Lasso) - Décomposition de la prédiction pour", nom_commune)) +
    theme_minimal()
  
  print(graphique_cascade_lasso)
  
} else {
  print("❌ La commune n'a pas été trouvée dans le référentiel.")
}

# ==============================================================================
# ETAPE 14 : MODÉLISATION XGBOOST ET SHAP VALUES EXTRÊMEMENT RAPIDES (2023)
# ==============================================================================

install.packages("xgboost")
library(xgboost)
library(shapviz)
library(ggplot2)
library(dplyr)
library(Metrics)

print("--- 1. PRÉPARATION DES MATRICES POUR XGBOOST ---")

# XGBoost a besoin de matrices pures (identiques au Lasso)
X_train_xgb <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_train)
Y_train_xgb <- base_train$Y_GAP_ACT_GLOBAL

X_2023_xgb <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_2023_sans_na)
Y_2023_xgb <- base_2023_sans_na$Y_GAP_ACT_GLOBAL

print("--- 2. ENTRAÎNEMENT DU MODÈLE XGBOOST ---")

set.seed(42)
modele_xgb <- xgboost(
  data = X_train_xgb, 
  label = Y_train_xgb, 
  nrounds = 150,                # Nombre d'arbres
  max_depth = 6,                # Profondeur des arbres
  eta = 0.1,                    # Taux d'apprentissage
  objective = "reg:squarederror", # Pour de la régression (RMSE)
  verbose = 0                   # Pour ne pas polluer ta console
)

print("--- 3. PRÉDICTION ET ÉVALUATION SUR 2023 ---")

predictions_2023_xgb <- predict(modele_xgb, X_2023_xgb)

r2_2023_xgb <- cor(predictions_2023_xgb, Y_2023_xgb)^2
rmse_2023_xgb <- rmse(Y_2023_xgb, predictions_2023_xgb)

print(paste("Performance R2 XGBoost sur 2023 :", round(r2_2023_xgb, 4)))
print(paste("Erreur RMSE XGBoost sur 2023 :", round(rmse_2023_xgb, 4)))


library(kernelshap)
library(shapviz)
library(ggplot2)

# ==============================================================================
# 1. PARAMÉTRAGE DE LA COMMUNE CHOISIE (Exemple : Nantes = 44109)
# ==============================================================================
code_insee_cible <- "44109" 

# ==============================================================================
# 2. EXTRACTION, PRÉDICTION ET SHAP VALUES POUR CETTE COMMUNE
# ==============================================================================

index_commune <- which(referentiel_communes_2023$GEO == code_insee_cible)

if(length(index_commune) > 0) {
  
  nom_commune <- referentiel_communes_2023$GEO_LABEL[index_commune]
  print(paste("--- ANALYSE CIBLÉE POUR :", nom_commune, "(", code_insee_cible, ") ---"))
  
  # A. Extraction et alignement strict de la ligne de la commune
  colonnes_entrainement <- colnames(X_train_xgb)
  X_ciblé <- X_2023_xgb[index_commune, colonnes_entrainement, drop = FALSE]
  
  # B. PRÉDICTION SIMPLE
  prediction_commune <- predict(modele_xgb, newdata = as.matrix(X_ciblé))
  print(paste("=> L'écart d'activité prédit par XGBoost pour", nom_commune, "est de :", round(prediction_commune, 4)))
  
  # C. CALCUL DES SHAP VALUES (Uniquement pour cette commune)
  # On prend un petit échantillon de référence (50 lignes de X_train) : c'est très léger pour la RAM
  set.seed(42)
  bg_xgb <- X_train_xgb[sample(nrow(X_train_xgb), 50), ] 
  
  predict_xgb_simple <- function(modele, newdata) {
    predict(modele, newdata = as.matrix(newdata))
  }
  
  # Le calcul se fait uniquement sur la ligne "X_ciblé". C'est instantané et sans danger pour R.
  shap_commune_kernel <- kernelshap(
    modele_xgb, 
    X = X_ciblé, 
    bg_X = bg_xgb, 
    pred_fun = predict_xgb_simple
  )
  
  # D. AFFICHAGE DU GRAPHIQUE SHAP EN CASCADE (WATERFALL)
  sv_commune <- shapviz(shap_commune_kernel)
  graphique_cascade <- sv_waterfall(sv_commune, max_display = 15) +
    ggtitle(paste("SHAP Values (XGBoost) - Explication de la prédiction pour", nom_commune)) +
    theme_minimal()
  
  print(graphique_cascade)
  
} else {
  print("❌ La commune n'a pas été trouvée dans le référentiel. Vérifie le code INSEE.")
}