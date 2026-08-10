# ==============================================================================
# PRÉPARATION DE LA BASE DE DONNÉES 2023 ET PROJECTION (DATA DRIFT)
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)
library(tidyr)
library(missForest) 
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
# ÉTAPE 2 : PIVOT ET ALIGNEMENT STRICT SUR LA NOMENCLATURE 2022
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

# 1. On applique le formatage sur 2023
names(base_large_2023) <- make.names(names(base_large_2023), unique = TRUE)

# ==============================================================================
# CORRECTION DE SCHÉMA : FORCER LES NOMS DE 2023 À COPIER CEUX DE 2022
# ==============================================================================

# 2. On récupère les noms officiels de la base 2022
noms_2022_ref <- names(base_large) 
noms_2023_actuels <- names(base_large_2023)

# 3. Fonction pour extraire l'essence du texte
nettoyer_texte <- function(noms) {
  noms <- tolower(noms)
  # On retire les 'x' qui servent de croisements dans les noms R 
  noms <- gsub("\\.x\\.", "", noms)
  # On retire absolument toute la ponctuation et les espaces
  noms <- gsub("[^a-z0-9éèàâêîôû]", "", noms)
  return(noms)
}

# 4. On crée les dictionnaires nettoyés
noms_2022_propres <- nettoyer_texte(noms_2022_ref)
noms_2023_propres <- nettoyer_texte(noms_2023_actuels)

# 5. On écrase 2023 par le nom de 2022
for (i in seq_along(noms_2023_actuels)) {
  index_correspondant <- match(noms_2023_propres[i], noms_2022_propres)
  
  if (!is.na(index_correspondant)) {
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
    
    # 1. Somme des tranches d'âge pour la population totale 
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
    
    #Utilisation de la résidence principale comme base pour les logements
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
  select(-GEO, -GEO_LABEL, -Population)     

colonnes_secret_2023 <- names(base_pre_filtre_2023)[grepl("salaire|pauvret|revenu|niveau.de.vie", names(base_pre_filtre_2023), ignore.case = TRUE)]

print("Colonnes identifiées pour l'imputation mathématique :")
print(colonnes_secret_2023)

base_finale_2023 <- base_pre_filtre_2023 %>%
  mutate(across(-all_of(colonnes_secret_2023), ~ replace_na(., 0)))

compte_na_2023 <- colSums(is.na(base_finale_2023))
print("--- BILAN DES NAs APRÈS MISE À ZÉRO DES ÉQUIPEMENTS ---")
print(compte_na_2023[compte_na_2023 > 0])

# ==============================================================================
# ÉTAPE 4B : ALIGNEMENT DES COLONNES DU MODÈLE (RETRAIT DES VARIABLES DISPARUES)
# ==============================================================================

# 1. On identifie les colonnes communes entre 2022 et 2023
colonnes_communes <- intersect(names(base_sans_na), names(base_finale_2023))

# 2. On regarde ce qui va être retiré 
variables_supprimees_du_modele <- setdiff(names(base_sans_na), colonnes_communes)
print(paste("Nombre de variables retirées du modèle car disparues en 2023 :", length(variables_supprimees_du_modele)))
print(variables_supprimees_du_modele)

# 3. On filtre définitivement les deux bases pour qu'elles aient exactement les mêmes colonnes
base_sans_na <- base_sans_na %>% select(all_of(colonnes_communes))
base_finale_2023 <- base_finale_2023 %>% select(all_of(colonnes_communes))

# ==============================================================================
# ÉTAPE 5 : TEST DE SÉCURITÉ - VÉRIFICATION DES VARIABLES AVANT L'IMPUTATION
# ==============================================================================

print("--- LANCEMENT DU TEST DE CORRESPONDANCE DES COLONNES ---")

# 1. On récupère les colonnes de référence de 2022 
colonnes_2022 <- names(base_sans_na) 

# 2. On récupère les colonnes actuelles de 2023 
colonnes_2023 <- names(base_finale_2023) 

# 3. Calcul des différences
variables_manquantes_2023 <- setdiff(colonnes_2022, colonnes_2023)
variables_en_trop_2023 <- setdiff(colonnes_2023, colonnes_2022)

# 4. Affichage du bilan
cat("Variables attendues par le modèle (2022) :", length(colonnes_2022), "\n")
cat("Variables présentes dans la base (2023) :", length(colonnes_2023), "\n\n")

# 5. La condition d'arrêt
if (length(variables_manquantes_2023) == 0 && length(variables_en_trop_2023) == 0) {
  print("✅ SUCCÈS : Toutes les variables colonnes parfaitement ")
} else {
  print("❌ ERREUR : La structure des colonnes ne correspond pas. Arrêt immédiat.")
  
  if (length(variables_manquantes_2023) > 0) {
    cat("\n-> Variables présentes en 2022 mais absentes en 2023 (", length(variables_manquantes_2023), ") :\n")
    print(variables_manquantes_2023)
  }
  
  if (length(variables_en_trop_2023) > 0) {
    cat("\n-> Variables présentes en 2023 mais absentes en 2022 (", length(variables_en_trop_2023), ") :\n")
    print(variables_en_trop_2023)
  }
  
  # Le stop() coupe l'exécution du script, missForest ne sera pas lancé
  stop("Vérification échouée")
}
# ==============================================================================
# ÉTAPE 6A : IMPUTATION DÉFINITIVE AVEC MISSFOREST
# ==============================================================================

print("--- 1. Conversion du format ---")
base_finale_propre_2023 <- base_finale_2023 %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.data.frame()

print("--- 2. Application de MissForest ---")
imputation_finale_2023 <- missForest(base_finale_propre_2023, ntree = 50, maxiter = 5)

base_2023_sans_na <- imputation_finale_2023$ximp

print(paste("Nombre total de NAs restants :", sum(is.na(base_2023_sans_na))))

#===============================================================================
# ETAPE 7 : VÉRIFICATION DES VARIABLES MANQUANTES
#===============================================================================

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
  num.trees = 400,               # 400 arbres aprés validation seuil
  importance = 'impurity'        # Pour pouvoir analyser l'importance des variables plus tard
)

# --- 2. ÉVALUATION SUR L'ÉCHANTILLON DE VALIDATION (`base_val`) ---
# On prédit les valeurs pour l'échantillon de validation
predictions_val <- predict(modele_base, data = base_val)

# On calcule l'erreur quadratique moyenne (MSE) sur la validation
mse_val <- mean((base_val$Y_GAP_ACT_GLOBAL - predictions_val$predictions)^2)
rmse_val <- sqrt(mse_val)

print(paste("MSE sur l'échantillon de validation :", round(mse_val, 5)))
print(paste("RMSE sur l'échantillon de validation :", round(rmse_val, 5)))

# Affichage du résumé du modèle
print(modele_base)


#===============================================================================
# ETAPE 9 : PRÉDICTION SUR 2023 (INFERENCE)
#===============================================================================

print("--- 1. Prédiction sur le jeu de données 2023 ---")

# On applique le modèle entraîné sur les nouvelles données 2023
predictions_2023 <- predict(modele_base, data = base_2023_sans_na)$predictions 

print("--- 2. ÉVALUATION DE LA ROBUSTESSE (DATA DRIFT) ---")

# Calcul du R2 sur les données de 2023
r2_2023 <- cor(predictions_2023, base_2023_sans_na$Y_GAP_ACT_GLOBAL)^2

# Calcul de la RMSE sur 2023
rmse_2023 <- rmse(base_2023_sans_na$Y_GAP_ACT_GLOBAL, predictions_2023)

print(paste("Performance R2 sur 2023 :", round(r2_2023, 4)))
print(paste("Erreur RMSE sur 2023 :", round(rmse_2023, 4)))


# ==============================================================================
# PARTIE 11 : PRÉDICTION SUR 2023 AVEC LE MODÈLE LASSO (INFERENCE)
# ==============================================================================

print("--- 1. Normalisation et préparation de la matrice 2023 ---")

# Normalisation sur 2023
base_normalisee_2023 <- base_2023_sans_na %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

# Séparation de Y 2023
vecteur_Y_2023 <- base_normalisee_2023$Y_GAP_ACT_GLOBAL

# Transformation en matrice 
matrice_X_2023 <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee_2023)

print("--- 2. Prédiction sur le jeu de données 2023 ---")

# On applique le modèle Lasso sur la matrice 2023
predictions_2023_lasso <- predict(modele_lasso_cv, s = meilleur_lambda, newx = matrice_X_2023)

print("--- 3. Évaluation de la robustesse ---")

# Calcul du R2 de 2023
r2_2023_lasso <- cor(as.vector(predictions_2023_lasso), vecteur_Y_2023)^2

# Calcul de la RMSE de 2023
rmse_2023_lasso <- rmse(vecteur_Y_2023, as.vector(predictions_2023_lasso))

print(paste("Performance R2 Lasso sur 2023 :", round(r2_2023_lasso, 4)))
print(paste("Erreur RMSE Lasso sur 2023 :", round(rmse_2023_lasso, 4)))

print("--- BILAN : COMPARAISON RANDOM FOREST vs LASSO ---")
print(paste("RMSE Random Forest :", round(rmse_2023, 4)))
print(paste("RMSE Lasso :", round(rmse_2023_lasso, 4)))