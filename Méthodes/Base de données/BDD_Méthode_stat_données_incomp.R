# ==============================================================================
# PRÉPARATION DE LA BASE DE DONNÉES ET GESTION EXPERTE DES VALEURS MANQUANTES
# ==============================================================================

# Installation des packages manquants si nécessaire
# install.packages(c("missForest", "VIM", "Metrics"))

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)
library(tidyr)
install.packages("missForest")
library(missForest) # Pour l'Option A
install.packages("VIM")
library(VIM)        # Pour l'Option B
install.packages("Metrics")
library(Metrics)    # Pour le calcul de la RMSE

# ------------------------------------------------------------------------------
# ÉTAPE 1 : CONNEXION ET EXTRACTION DES DONNÉES
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

chemin_s3 <- "s3://sarahbeaucamp/dossier_complet.parquet"
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

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

print("Étape 1 terminée : Données extraites.")

# ------------------------------------------------------------------------------
# ÉTAPE 2 : PIVOT ET NETTOYAGE DES NOMS
# ------------------------------------------------------------------------------

base_large <- base_filtree %>%
  group_by(GEO) %>%
  mutate(GEO_LABEL = first(GEO_LABEL)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(GEO, GEO_LABEL), 
    names_from = TAB_MEASURE_LABEL, 
    values_from = OBS_VALUE,
    values_fn = max            
  )

names(base_large) <- make.names(names(base_large), unique = TRUE)

print("Étape 2 terminée : Base pivotée au format Wide.")

# ------------------------------------------------------------------------------
# ÉTAPE 3 : CRÉATION DU Y, DES TAUX ET GRANDE PURGE
# ------------------------------------------------------------------------------

base_prete_rf <- base_large %>%
  mutate(
    Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie = .data[["Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie"]] / 100,
    POP_FEMME = (.data[["Population...De.15.à.24.ans..x.Femme"]] + .data[["Population...De.25.à.39.ans.x.Femme"]] + .data[["Population...De.40.à.54.ans.x.Femme"]] + .data[["Population...De.55.à.64.ans.x.Femme"]]),
    TAUX_F = (.data[["Population...De.15.à.64.ans.x.Actif.occupé.x.Femme"]] + .data[["Population...De.15.à.64.ans.x.Chômeur.x.Femme"]]) / POP_FEMME,
    POP_HOMME = (.data[["Population...De.15.à.24.ans..x.Homme"]] + .data[["Population...De.25.à.39.ans.x.Homme"]] + .data[["Population...De.40.à.54.ans.x.Homme"]] + .data[["Population...De.55.à.64.ans.x.Homme"]]),
    TAUX_H = (.data[["Population...De.15.à.64.ans.x.Actif.occupé.x.Homme"]] + .data[["Population...De.15.à.64.ans.x.Chômeur.x.Homme"]]) / POP_HOMME,
    Y_GAP_ACT_GLOBAL = TAUX_H - TAUX_F,
    across(.cols = starts_with("Logements...") | starts_with("Nombre.de.pièces..."), .fns = ~ .x / .data[["Logements"]]),
    across(.cols = starts_with("Population.des.ménages...") & !matches("^Population.des.ménages$"), .fns = ~ .x / .data[["Population.des.ménages"]]),
    across(.cols = starts_with("Nombre.d.emplois...") & !matches("^Nombre.d.emplois$"), .fns = ~ .x / .data[["Nombre.d.emplois"]]),
    across(.cols = starts_with("Population...Actif.") & !matches("^Population...Actif$"), .fns = ~ .x / .data[["Population...Actif"]]),
    across(
      .cols = (starts_with("Population...") | starts_with("Établissements...") | starts_with("Nombre.de.place") | starts_with("Nombre.d.équipements...") | starts_with("Nombre.de.nouvelles") | starts_with("Nombre.de.personnes.seules...") | starts_with("Nombre.de.famille")) & 
        !matches("^Population$|^Population.des.ménages$|^Population...Actif$|^Population...Actif."),
      .fns = ~ .x / .data[["Population"]]
    )
  ) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.) | is.nan(.), NA, .))) %>% # On garde les NA pour le moment
  select(
    -TAUX_F, -TAUX_H, -POP_FEMME, -POP_HOMME,
    -Logements, -Population.des.ménages, -Population...Actif, -Nombre.d.emplois,
    -contains("Actif", ignore.case = TRUE),
    -contains("Chômeur", ignore.case = TRUE),
    -contains("inactif", ignore.case = TRUE),
    -contains("foyer", ignore.case = TRUE),
    -contains("Retraité", ignore.case = TRUE),
    -contains("Élève", ignore.case = TRUE),
    -matches("^Population.*Femme", ignore.case = TRUE),
    -matches("^Population.*Homme", ignore.case = TRUE)
  )

print("Étape 3 terminée : Purge et calculs effectués.")

# ------------------------------------------------------------------------------
# ÉTAPE 4 : FILTRE DE POPULATION ET TRAITEMENT HYBRIDE DES NAs
# ------------------------------------------------------------------------------

base_pre_filtre <- base_prete_rf %>%
  filter(Population >= 430) %>%              
  filter(!is.na(Y_GAP_ACT_GLOBAL)) %>%
  select(-GEO, -GEO_LABEL, -Population)      

# 1. Identifier les colonnes soumises au secret statistique (Revenus, Salaires, Pauvreté)
# Le grepl va chercher tous les mots clés liés à l'argent, peu importe la casse
colonnes_secret <- names(base_pre_filtre)[grepl("salaire|pauvret|revenu|niveau.de.vie", names(base_pre_filtre), ignore.case = TRUE)]

print("Colonnes identifiées pour l'imputation mathématique (Secret Statistique) :")
print(colonnes_secret)

# 2. Le traitement hybride : 0 pour les équipements, NA pour l'argent
base_finale <- base_pre_filtre %>%
  # On remplace par 0 TOUTES les colonnes SAUF celles identifiées dans colonnes_secret
  mutate(across(-all_of(colonnes_secret), ~ replace_na(., 0)))

compte_na <- colSums(is.na(base_finale))
print("--- BILAN DES NAs APRÈS MISE À ZÉRO DES ÉQUIPEMENTS ---")
print(compte_na[compte_na > 0])


# ------------------------------------------------------------------------------
# ÉTAPE 5 : LE CRASH-TEST D'IMPUTATION (BENCHMARK) SUR LES SALAIRES
# ------------------------------------------------------------------------------
print("--- DÉBUT DU BENCHMARK D'IMPUTATION ---")

# 1. Isoler une sous-base parfaite et FORCER LE FORMAT NUMÉRIQUE (Correction du bug)
base_parfaite <- base_finale %>% 
  drop_na() %>%
  mutate(across(everything(), as.numeric)) %>% # On force tout en chiffres
  as.data.frame()                              # On retire le format "tibble" pour missForest

# 2. Échantillon de 2000 communes pour la rapidité du test
set.seed(42)
base_test_imputation <- base_parfaite[sample(nrow(base_parfaite), min(2000, nrow(base_parfaite))), ]

# 3. Choix de la variable cible pour le test
colonne_cible <- names(sort(compte_na, decreasing = TRUE))[1]
print(paste("La variable testée sera :", colonne_cible))

vraies_valeurs <- base_test_imputation[[colonne_cible]]

# 4. Création des Faux NAs (15% de données détruites)
set.seed(42)
index_na <- sample(1:nrow(base_test_imputation), size = 0.15 * nrow(base_test_imputation))
base_truquee <- base_test_imputation
base_truquee[[colonne_cible]][index_na] <- NA

print(paste("Nombre de faux NAs générés pour le test :", length(index_na)))

# --- OPTION A : MISSFOREST ---
print("-> Lancement de missForest (Option A)... (Patiente quelques minutes)")
imputation_A <- missForest(base_truquee, ntree = 50, maxiter = 5)
base_A <- imputation_A$ximp
valeurs_predites_A <- base_A[[colonne_cible]][index_na]

# --- OPTION B : K-NN ---
print("-> Lancement de k-NN (Option B)...")
base_B <- kNN(base_truquee, variable = colonne_cible, k = 5, imp_var = FALSE)
valeurs_predites_B <- base_B[[colonne_cible]][index_na]

# --- RÉSULTATS DU BENCHMARK ---
vraies_valeurs_na <- vraies_valeurs[index_na]
rmse_A <- rmse(vraies_valeurs_na, valeurs_predites_A)
rmse_B <- rmse(vraies_valeurs_na, valeurs_predites_B)

print("--- RÉSULTATS DU MATCH D'IMPUTATION ---")
print(paste("RMSE Option A (missForest) :", round(rmse_A, 5)))
print(paste("RMSE Option B (k-NN)       :", round(rmse_B, 5)))

if(rmse_A < rmse_B) {
  print("LE GAGNANT EST : L'Option A (missForest) ! Tu peux appliquer cette méthode pour ton Lasso.")
} else {
  print("LE GAGNANT EST : L'Option B (k-NN) ! Tu peux appliquer cette méthode pour ton Lasso.")
}

# ==============================================================================
# ÉTAPE 6 : APPLICATION DÉFINITIVE SUR LA BASE COMPLÈTE (Choisir A ou B)
# ==============================================================================

# DÉCOMMENTE (Enlève le #) DEVANT LES LIGNES DU BLOC GAGNANT POUR L'EXÉCUTER

# ==============================================================================
# ÉTAPE 6A : IMPUTATION DÉFINITIVE AVEC MISSFOREST
# ==============================================================================

print("--- 1. CONVERSION STRICTE DU FORMAT ---")
base_finale_propre <- base_finale %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.data.frame()

print("--- 2. APPLICATION DE MISSFOREST SUR LA BASE COMPLÈTE ---")
# On lance l'imputation sur la base "propre"
imputation_finale <- missForest(base_finale_propre, ntree = 50, maxiter = 5)

# On récupère le tableau final nettoyé
base_sans_na <- imputation_finale$ximp

print(paste("Nombre total de NAs restants :", sum(is.na(base_sans_na))))

# ------------------------------------------------------------------------------
# 6.B SI K-NN A GAGNÉ (Exécution plus rapide)
# ------------------------------------------------------------------------------
# print("--- APPLICATION DE K-NN SUR LA BASE COMPLÈTE ---")
# base_sans_na_propre <- kNN(base_finale, k = 5, imp_var = FALSE)
# print(paste("NAs restants après k-NN :", sum(is.na(base_sans_na_propre))))