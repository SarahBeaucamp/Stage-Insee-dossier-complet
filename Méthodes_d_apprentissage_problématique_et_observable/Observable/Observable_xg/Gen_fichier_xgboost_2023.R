# ==============================================================================
# EXPORT DE LA BASE COMPLÈTE AVEC SHAP POUR LE DASHBOARD
# ==============================================================================

library(dplyr)
library(arrow)
library(xgboost)

print("--- 1. CONVERSION ET CALCUL SHAP GLOBAL ---")
# 1. On force la classe en "Booster" pour débloquer les SHAP values
modele_pur <- modele_xgb
class(modele_pur) <- "xgb.Booster"

# 2. On transforme TOUTE la base 2023 en DMatrix d'un coup
matrice_globale_dmatrix <- xgb.DMatrix(data = as.matrix(X_2023_xgb))

# 3. Prédictions classiques pour toutes les communes
predictions_2023 <- predict(modele_pur, newdata = matrice_globale_dmatrix)

# 4. Calcul SHAP natif sur toutes les communes
shap_brut_global <- predict(modele_pur, newdata = matrice_globale_dmatrix, predcontrib = TRUE)

# 5. On sépare les variables explicatives de la colonne "Biais" (la dernière colonne)
shap_vars_global <- shap_brut_global[, -ncol(shap_brut_global)]
noms_variables_xgb <- colnames(X_train_xgb)

print("--- 2. EXTRACTION DU TOP 5 POUR CHAQUE COMMUNE ---")
# Fonction rapide pour traiter une seule ligne
extraire_top_5 <- function(ligne_shap) {
  indices_top <- order(abs(ligne_shap), decreasing = TRUE)[1:5]
  data.frame(
    VAR_1 = noms_variables_xgb[indices_top[1]], SHAP_1 = ligne_shap[indices_top[1]],
    VAR_2 = noms_variables_xgb[indices_top[2]], SHAP_2 = ligne_shap[indices_top[2]],
    VAR_3 = noms_variables_xgb[indices_top[3]], SHAP_3 = ligne_shap[indices_top[3]],
    VAR_4 = noms_variables_xgb[indices_top[4]], SHAP_4 = ligne_shap[indices_top[4]],
    VAR_5 = noms_variables_xgb[indices_top[5]], SHAP_5 = ligne_shap[indices_top[5]]
  )
}

# On applique cette fonction à toutes les communes (ça prend environ 15 à 30 secondes)
liste_top5 <- lapply(1:nrow(shap_vars_global), function(i) extraire_top_5(shap_vars_global[i, ]))
tableau_top5 <- bind_rows(liste_top5)

print("--- 3. ASSEMBLAGE ET EXPORT PARQUET ---")
# On rassemble tout : Identifiants, Réel, Prédit
base_dashboard <- data.frame(
  GEO = referentiel_communes_2023$GEO,
  GEO_LABEL = referentiel_communes_2023$GEO_LABEL,
  Y_OBSERVE = base_2023_sans_na$Y_GAP_ACT_GLOBAL,
  Y_PREDIT_XGB = predictions_2023
)

# On y colle les 5 variables SHAP
base_dashboard <- bind_cols(base_dashboard, tableau_top5)

# On sauvegarde sur le pc
nom_fichier_export <- "predictions_xgboost_2023.parquet"
write_parquet(base_dashboard, nom_fichier_export)

print(paste("Le fichier", nom_fichier_export, "a été généré avec succès."))


# ==============================================================================
# EXPORT DE LA BASE COMPLÈTE AVEC SHAP ET VRAIES VALEURS POUR LE DASHBOARD
# ==============================================================================

library(dplyr)
library(arrow)
library(xgboost)

print("--- 1. CONVERSION ET CALCUL SHAP GLOBAL ---")
# 1. On force la classe en "Booster" pour débloquer les SHAP values
modele_pur <- modele_xgb
class(modele_pur) <- "xgb.Booster"

# 2. On transforme toute la base 2023 en DMatrix d'un coup
matrice_globale_dmatrix <- xgb.DMatrix(data = as.matrix(X_2023_xgb))

# 3. Prédictions classiques pour toutes les communes
predictions_2023 <- predict(modele_pur, newdata = matrice_globale_dmatrix)

# 4. Calcul SHAP natif sur toutes les communes
shap_brut_global <- predict(modele_pur, newdata = matrice_globale_dmatrix, predcontrib = TRUE)

# 5. On sépare les variables explicatives de la colonne "Biais" (la dernière colonne)
shap_vars_global <- shap_brut_global[, -ncol(shap_brut_global)]
noms_variables_xgb <- colnames(X_train_xgb)

print("--- 2. EXTRACTION DU TOP 5 ET DES VALEURS RÉELLES POUR CHAQUE COMMUNE ---")
matrice_valeurs_reelles <- as.matrix(X_2023_xgb)

# La fonction prend l'index "i" pour accéder aux SHAP et aux Valeurs Réelles
extraire_top_5 <- function(i) {
  ligne_shap <- shap_vars_global[i, ]
  ligne_valeurs <- matrice_valeurs_reelles[i, ]
  
  indices_top <- order(abs(ligne_shap), decreasing = TRUE)[1:5]
  
  data.frame(
    VAR_1 = noms_variables_xgb[indices_top[1]], 
    VAL_1 = ligne_valeurs[indices_top[1]], 
    SHAP_1 = ligne_shap[indices_top[1]],
    
    VAR_2 = noms_variables_xgb[indices_top[2]], 
    VAL_2 = ligne_valeurs[indices_top[2]], 
    SHAP_2 = ligne_shap[indices_top[2]],
    
    VAR_3 = noms_variables_xgb[indices_top[3]], 
    VAL_3 = ligne_valeurs[indices_top[3]], 
    SHAP_3 = ligne_shap[indices_top[3]],
    
    VAR_4 = noms_variables_xgb[indices_top[4]], 
    VAL_4 = ligne_valeurs[indices_top[4]], 
    SHAP_4 = ligne_shap[indices_top[4]],
    
    VAR_5 = noms_variables_xgb[indices_top[5]], 
    VAL_5 = ligne_valeurs[indices_top[5]], 
    SHAP_5 = ligne_shap[indices_top[5]]
  )
}

# On applique cette fonction à toutes les communes
liste_top5 <- lapply(1:nrow(shap_vars_global), extraire_top_5)
tableau_top5 <- bind_rows(liste_top5)

print("--- 3. ASSEMBLAGE ET EXPORT PARQUET ---")
# On rassemble tout : Identifiants, Réel, Prédit
base_dashboard <- data.frame(
  GEO = referentiel_communes_2023$GEO,
  GEO_LABEL = referentiel_communes_2023$GEO_LABEL,
  Y_OBSERVE = base_2023_sans_na$Y_GAP_ACT_GLOBAL,
  Y_PREDIT_XGB = predictions_2023
)

# On y colle les 5 variables SHAP ET leurs valeurs
base_dashboard <- bind_cols(base_dashboard, tableau_top5)

# On sauvegarde sur le pc
nom_fichier_export <- "predictions_xgboost.parquet"
write_parquet(base_dashboard, nom_fichier_export)


print(paste("Le fichier", nom_fichier_export, "a été généré avec succès."))
