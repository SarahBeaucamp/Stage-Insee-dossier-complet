# ==============================================================================
# CALCUL MASSIF DES SHAP VALUES ET GÉNÉRATION DU FICHIER FINAL 
# ==============================================================================
install.packages("treeshap")
library(treeshap)
library(arrow)
library(dplyr)

print("--- 1. PRÉPARATION DU MODÈLE POUR TREESHAP ---")
# On unifie le modèle ranger (nécessite les données d'entraînement pour comprendre la structure)
unified_rf <- ranger.unify(modele_base, base_train)

# Création du dossier temporaire pour sauvegarder les morceaux sans saturer la RAM
dir.create("chunks_rf_2023", showWarnings = FALSE)

print("--- 2. PARAMÉTRAGE DU TRAITEMENT PAR MICRO-LOTS ---")
# On fusionne avec le référentiel pour garder les noms des communes (GEO et GEO_LABEL)
donnees_completes <- bind_cols(referentiel_communes_2023, base_2023_sans_na)
noms_des_variables <- setdiff(names(base_2023_sans_na), "Y_GAP_ACT_GLOBAL")

taille_lot <- 5
nb_lignes <- nrow(donnees_completes)
nb_lots <- ceiling(nb_lignes / taille_lot)

print(paste("Début du traitement massif en", nb_lots, "lots..."))

# --- Fonction rapide pour extraire le Top 5 par commune ---
extraire_top_5 <- function(shaps_row, vals_row, noms_vars) {
  # Trouve les indices des 5 plus fortes contributions (en valeur absolue)
  idx <- order(abs(shaps_row), decreasing = TRUE)[1:5]
  
  list(
    VAR_1 = noms_vars[idx[1]], VAL_1 = vals_row[idx[1]], SHAP_1 = shaps_row[idx[1]],
    VAR_2 = noms_vars[idx[2]], VAL_2 = vals_row[idx[2]], SHAP_2 = shaps_row[idx[2]],
    VAR_3 = noms_vars[idx[3]], VAL_3 = vals_row[idx[3]], SHAP_3 = shaps_row[idx[3]],
    VAR_4 = noms_vars[idx[4]], VAL_4 = vals_row[idx[4]], SHAP_4 = shaps_row[idx[4]],
    VAR_5 = noms_vars[idx[5]], VAL_5 = vals_row[idx[5]], SHAP_5 = shaps_row[idx[5]]
  )
}

# ==============================================================================
# BOUCLE DE CALCUL ET D'ÉCRITURE
# ==============================================================================

for (i in 1:nb_lots) {
  
  debut <- ((i - 1) * taille_lot) + 1
  fin <- min(i * taille_lot, nb_lignes)
  
  print(paste("Traitement du lot", i, "/", nb_lots, "(communes", debut, "à", fin, ")"))
  
  # 1. Extraction du lot
  lot_data <- donnees_completes[debut:fin, ]
  lot_features <- lot_data %>% select(all_of(noms_des_variables))
  
  # 2. Prédictions RF
  predictions_rf <- predict(modele_base, data = lot_features)$predictions
  
  # 3. Calculs SHAP (Rapide avec treeshap sur de petits volumes)
  shap_results <- treeshap(unified_rf, lot_features, verbose = FALSE)
  matrice_shaps <- as.matrix(shap_results$shaps)
  matrice_valeurs <- as.matrix(lot_features)
  
  # 4. Extraction du Top 5 ligne par ligne
  liste_top5 <- lapply(1:nrow(lot_features), function(j) {
    extraire_top_5(
      shaps_row = matrice_shaps[j, ], 
      vals_row = matrice_valeurs[j, ], 
      noms_vars = noms_des_variables
    )
  })
  
  # Conversion de la liste en dataframe propre
  df_top5 <- bind_rows(liste_top5)
  
  # 5. Création du tableau final pour ce lot
  resultats_lot <- data.frame(
    GEO = lot_data$GEO,
    GEO_LABEL = lot_data$GEO_LABEL,
    Y_OBSERVE = lot_data$Y_GAP_ACT_GLOBAL,
    Y_PREDIT_RF = predictions_rf
  )
  
  # On colle les informations top 5
  resultats_lot <- bind_cols(resultats_lot, df_top5)
  
  # 6. Sauvegarde du lot en fichier Parquet 
  nom_fichier <- paste0("chunks_rf_2023/lot_", sprintf("%04d", i), ".parquet")
  write_parquet(resultats_lot, nom_fichier)
  
  # 7. PURGE DE LA RAM (Ultra important)
  rm(lot_data, lot_features, predictions_rf, shap_results, matrice_shaps, matrice_valeurs, liste_top5, df_top5, resultats_lot)
  gc(verbose = FALSE)
}

print("--- 3. REGROUPEMENT ET EXPORT FINAL ---")

# Lecture de tous les petits fichiers sans saturer la RAM
fichiers_lots <- list.files("chunks_rf_2023", full.names = TRUE, pattern = "*.parquet")
dataset_final <- open_dataset(fichiers_lots) %>% collect()

# Écriture du fichier final global
write_parquet(dataset_final, "predictions_rf_2023.parquet")

# Nettoyage du dossier temporaire
unlink("chunks_rf_2023", recursive = TRUE)

print("Le fichier 'predictions_rf_2023.parquet' a été généré et formaté pour le simulateur Quarto.")
