# ==============================================================================
# PRÉPARATION DE LA BASE DE DONNÉES ET GESTION EXPERTE DES VALEURS MANQUANTES
# ==============================================================================

# install.packages(c("missForest", "VIM", "Metrics"))

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

base_filtree_sans_pop <- dossier_complet %>%
  filter(
    GEO_OBJECT_LABEL == "Commune",
    str_detect(ID_TAB, "^EMP|^EQUIP|^TOU") | 
      ID_TAB %in% c("POP_T5", "POP_T6", "REV_T1", "SAL_G1", "SAL_G3", 
                    "SAL_T1", "SAL_G4", "DEN_T1", "DEN_T3", "DEN_T4", "RES_T3", 
                    "RES_T5", "POP_T3", "POP_T0", "LOG_T7", "FOR_T1", "FOR_T2",
                    "FAM_T1", "FAM_T2", "FAM_T3", "FAM_T4", "FAM_G1", "FAM_G2", "FAM_G4")
  ) %>%
  select(GEO, GEO_LABEL, ID_TAB, TAB_MEASURE_LABEL, OBS_VALUE) %>%
  collect()

print("Étape 1 terminée : Données extraites.")

# ------------------------------------------------------------------------------
# ÉTAPE 2 : PIVOT ET NETTOYAGE DES NOMS
# ------------------------------------------------------------------------------

base_large_sans_pop <- base_filtree_sans_pop %>%
  group_by(GEO) %>%
  mutate(GEO_LABEL = first(GEO_LABEL)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(GEO, GEO_LABEL), 
    names_from = TAB_MEASURE_LABEL, 
    values_from = OBS_VALUE,
    values_fn = max            
  )

names(base_large_sans_pop) <- make.names(names(base_large_sans_pop), unique = TRUE)

print("Étape 2 terminée : Base pivotée au format Wide.")

# ------------------------------------------------------------------------------
# ÉTAPE 3 : CRÉATION DU Y, DES TAUX ET GRANDE PURGE
# ------------------------------------------------------------------------------

base_prete_rf_sans_pop <- base_large_sans_pop %>%
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
    -matches("^Population.*Homme", ignore.case = TRUE),
    -matches("^Population.*ans\\.?$", ignore.case = TRUE)
  )

print("Étape 3 terminée : Purge et calculs effectués.")

# ------------------------------------------------------------------------------
# ÉTAPE 4 : FILTRE DE POPULATION ET TRAITEMENT HYBRIDE DES NAs ET SEUIL
# ------------------------------------------------------------------------------

base_pre_filtre_sans_pop <- base_prete_rf_sans_pop %>%
  filter(Population >= 500) %>%              
  filter(!is.na(Y_GAP_ACT_GLOBAL)) %>%
  select(-GEO, -GEO_LABEL, -Population)      

# 1. Identifier les colonnes soumises au secret statistique (Revenus, Salaires, Pauvreté)
# Le grepl va chercher tous les mots clés liés à l'argent, peu importe la casse
colonnes_secret <- names(base_pre_filtre_sans_pop)[grepl("salaire|pauvret|revenu|niveau.de.vie", names(base_pre_filtre), ignore.case = TRUE)]

print("Colonnes identifiées pour l'imputation mathématique (Secret Statistique) :")
print(colonnes_secret)

# 2. Le traitement hybride : 0 pour les équipements, NA pour l'argent
base_finale_sans_pop <- base_pre_filtre_sans_pop %>%
  # On remplace par 0 toutes les colonnes sauf celles identifiées dans colonnes_secret
  mutate(across(-all_of(colonnes_secret), ~ replace_na(., 0)))

compte_na_sans_pop <- colSums(is.na(base_finale_sans_pop))
print("--- BILAN DES NAs APRÈS MISE À ZÉRO DES ÉQUIPEMENTS ---")
print(compte_na_sans_pop[compte_na_sans_pop > 0])

# ==============================================================================
# ÉTAPE 6A : IMPUTATION DÉFINITIVE AVEC MISSFOREST
# ==============================================================================

print("--- 1. CONVERSION STRICTE DU FORMAT ---")
base_finale_propre_sans_pop <- base_finale_sans_pop %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.data.frame()

print("--- 2. APPLICATION DE MISSFOREST SUR LA BASE COMPLÈTE ---")
# On lance l'imputation sur la base "propre"
imputation_finale_sans_pop <- missForest(base_finale_propre_sans_pop, ntree = 50, maxiter = 5)

# On récupère le tableau final nettoyé
base_sans_na_sans_pop <- imputation_finale_sans_pop$ximp

print(paste("Nombre total de NAs restants :", sum(is.na(base_sans_na_sans_pop))))