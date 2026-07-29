# ==============================================================================
# GÉNÉRATION DE LA BASE DASHBOARD (MÉTHODE SÉCURISÉE AVEC KERNELSHAP)
# ==============================================================================

library(dplyr)
library(kernelshap)
library(arrow) # Pour exporter en .parquet

print("--- 1. CALCUL DES SHAP VALUES GLOBALES (VIA KERNELSHAP) ---")

# A. On reprend ton échantillon de fond (Background) de 50 lignes
set.seed(42)
bg_xgb <- X_train_xgb[sample(nrow(X_train_xgb), 50), ] 

# B. Ta fonction de prédiction
predict_xgb_simple <- function(modele, newdata) {
  predict(modele, newdata = as.matrix(newdata))
}

# C. Lancement de KernelSHAP sur TOUTES les communes de 2023
print("Attention : Calcul en cours sur toutes les communes...")
print("Cela peut prendre quelques minutes. Laisse la barre de progression avancer tranquillement !")

shap_global_kernel <- kernelshap(
  modele_xgb, 
  X = X_2023_xgb,     # On lui donne toute la matrice 2023 d'un coup
  bg_X = bg_xgb, 
  pred_fun = predict_xgb_simple
)

# On extrait la matrice brute des résultats
matrice_shap <- shap_global_kernel$S
noms_variables <- colnames(matrice_shap)

print("--- 2. EXTRACTION DU TOP 5 SHAP POUR CHAQUE COMMUNE ---")

# Fonction qui trouve les 5 variables avec le plus d'impact pour une ligne donnée
extraire_top_5 <- function(ligne_shap) {
  # Tri des indices par valeur absolue décroissante
  indices_top <- order(abs(ligne_shap), decreasing = TRUE)[1:5]
  
  # On renvoie une ligne avec le Nom et la Valeur des 5 variables
  data.frame(
    VAR_1 = noms_variables[indices_top[1]], SHAP_1 = ligne_shap[indices_top[1]],
    VAR_2 = noms_variables[indices_top[2]], SHAP_2 = ligne_shap[indices_top[2]],
    VAR_3 = noms_variables[indices_top[3]], SHAP_3 = ligne_shap[indices_top[3]],
    VAR_4 = noms_variables[indices_top[4]], SHAP_4 = ligne_shap[indices_top[4]],
    VAR_5 = noms_variables[indices_top[5]], SHAP_5 = ligne_shap[indices_top[5]]
  )
}

# On applique cette fonction à toutes les communes (c'est très rapide)
liste_top5 <- lapply(1:nrow(matrice_shap), function(i) extraire_top_5(matrice_shap[i, ]))
tableau_top5 <- bind_rows(liste_top5)

print("--- 3. ASSEMBLAGE DE LA BASE D'EXPORT FINALE ---")

# On rassemble l'identifiant, le Réel, le Prédit et notre Top 5 SHAP
base_dashboard <- data.frame(
  GEO = referentiel_communes_2023$GEO,
  GEO_LABEL = referentiel_communes_2023$GEO_LABEL,
  Y_OBSERVE = Y_2023_xgb,
  Y_PREDIT_XGB = predictions_2023_xgb
)

# On colle le tableau des SHAP à côté
base_dashboard <- bind_cols(base_dashboard, tableau_top5)

print("--- 4. EXPORTATION AU FORMAT PARQUET ---")

nom_fichier_export <- "predictions_xgboost_2023.parquet"
write_parquet(base_dashboard, nom_fichier_export)

print(paste("✅ SUCCÈS ! Le fichier", nom_fichier_export, "a été créé dans ton répertoire courant."))