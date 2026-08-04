# ==============================================================================
# EXPORT DE LA BASE LASSO POUR LE DASHBOARD (LE VRAI ÉQUIVALENT SHAP CENTRÉ)
# ==============================================================================

library(dplyr)
library(arrow)

print("--- 1. ENTRAÎNEMENT DU MODÈLE FINAL SUR DONNÉES RÉELLES ---")
# 1. On récupère les noms des variables sélectionnées par le Lasso (sans l'intercept)
variables_selectionnees_noms <- tableau_coefs %>%
  filter(Variable != "(Intercept)") %>%
  pull(Variable)

# 2. On reconstruit la formule
formule_post_lasso <- as.formula(paste("Y_GAP_ACT_GLOBAL ~", paste(variables_selectionnees_noms, collapse = " + ")))

# 3. On entraîne le modèle linéaire sur les VRAIES données de 2022 (non normalisées)
modele_dashboard <- lm(formule_post_lasso, data = base_sans_na)

# 4. On calcule les prédictions finales pour toutes les communes (sur 2023)
predictions_2023_lasso <- predict(modele_dashboard, newdata = base_2023_sans_na)


print("--- 2. CALCUL DES CONTRIBUTIONS LOCALES (LE VRAI ÉQUIVALENT SHAP) ---")
# On extrait les coefficients du modèle (en enlevant l'Intercept)
coefs_modele <- coef(modele_dashboard)[-1] 
noms_variables_lasso <- names(coefs_modele)

# On isole les vraies valeurs des communes en 2023
matrice_valeurs_reelles <- as.matrix(base_2023_sans_na[, noms_variables_lasso])

# LA MAGIE EST ICI : On calcule les moyennes nationales de 2022 (l'année d'apprentissage)
moyennes_nationales <- colMeans(as.matrix(base_sans_na[, noms_variables_lasso]))

# On centre les valeurs : Valeur de la commune - Moyenne nationale
matrice_valeurs_centrees <- sweep(matrice_valeurs_reelles, 2, moyennes_nationales, "-")

# L'impact devient l'écart à la moyenne multiplié par le coefficient !
matrice_impacts <- sweep(matrice_valeurs_centrees, 2, coefs_modele, "*")


print("--- 3. EXTRACTION DU TOP 5 POUR CHAQUE COMMUNE ---")
extraire_top_5_lasso <- function(i) {
  ligne_impact <- matrice_impacts[i, ]
  
  # Très important : on récupère les "VRAIES" valeurs pour l'affichage dans le dashboard
  ligne_valeurs <- matrice_valeurs_reelles[i, ]
  
  # On trouve les 5 plus gros impacts (en valeur absolue)
  indices_top <- order(abs(ligne_impact), decreasing = TRUE)[1:5]
  
  data.frame(
    VAR_1 = noms_variables_lasso[indices_top[1]], 
    VAL_1 = ligne_valeurs[indices_top[1]], 
    SHAP_1 = ligne_impact[indices_top[1]],
    
    VAR_2 = noms_variables_lasso[indices_top[2]], 
    VAL_2 = ligne_valeurs[indices_top[2]], 
    SHAP_2 = ligne_impact[indices_top[2]],
    
    VAR_3 = noms_variables_lasso[indices_top[3]], 
    VAL_3 = ligne_valeurs[indices_top[3]], 
    SHAP_3 = ligne_impact[indices_top[3]],
    
    VAR_4 = noms_variables_lasso[indices_top[4]], 
    VAL_4 = ligne_valeurs[indices_top[4]], 
    SHAP_4 = ligne_impact[indices_top[4]],
    
    VAR_5 = noms_variables_lasso[indices_top[5]], 
    VAL_5 = ligne_valeurs[indices_top[5]], 
    SHAP_5 = ligne_impact[indices_top[5]]
  )
}

# On applique la fonction à toutes les communes
liste_top5_lasso <- lapply(1:nrow(matrice_impacts), extraire_top_5_lasso)
tableau_top5_lasso <- bind_rows(liste_top5_lasso)


print("--- 4. ASSEMBLAGE ET EXPORT PARQUET ---")
# On rassemble tout : Identifiants, Réel, Prédit
base_dashboard_lasso <- data.frame(
  GEO = referentiel_communes_2023$GEO,
  GEO_LABEL = referentiel_communes_2023$GEO_LABEL,
  Y_OBSERVE = base_2023_sans_na$Y_GAP_ACT_GLOBAL,
  Y_PREDIT_LASSO = predictions_2023_lasso
)

# On y colle les 5 variables impacts ET leurs valeurs (VAL_)
base_dashboard_lasso <- bind_cols(base_dashboard_lasso, tableau_top5_lasso)

# On sauvegarde sur la machine
nom_fichier_export_lasso <- "predictions_lasso_2023.parquet"
write_parquet(base_dashboard_lasso, nom_fichier_export_lasso)

print(paste("✅ SUCCÈS ! Le fichier", nom_fichier_export_lasso, "a été généré avec succès."))

