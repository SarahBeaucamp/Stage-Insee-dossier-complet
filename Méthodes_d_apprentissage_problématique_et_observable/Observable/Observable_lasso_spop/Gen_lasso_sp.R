# ==============================================================================
# 1. ENTRAÎNEMENT DU LASSO (SUR BASE 2022 SANS POP) POUR OBTENIR LES COEFS
# ==============================================================================
library(glmnet)
library(dplyr)
library(arrow)

print("--- 1A. NORMALISATION DE LA BASE 2022 (SANS POP) ---")
base_normalisee_sans_pop <- base_sans_na_sans_pop %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

vecteur_Y_sp <- base_normalisee_sans_pop$Y_GAP_ACT_GLOBAL
matrice_X_sp <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee_sans_pop)

print("--- 1B. ENTRAÎNEMENT DU LASSO (VALIDATION CROISÉE) ---")
modele_lasso_cv_sp <- cv.glmnet(
  x = matrice_X_sp, 
  y = vecteur_Y_sp, 
  alpha = 1,            
  standardize = FALSE,   
  nfolds = 5            
)

# Extraction des coefficients
coefficients_lasso_sp <- coef(modele_lasso_cv_sp, s = "lambda.min")
tableau_coefs_sp <- data.frame(
  Variable = rownames(coefficients_lasso_sp),
  Coefficient = as.numeric(coefficients_lasso_sp)
) %>%
  filter(Coefficient != 0) %>% 
  arrange(desc(abs(Coefficient)))

print("Tableau des coefficients 'sans population' généré avec succès !")


# ==============================================================================
# 2. EXPORT DE LA BASE LASSO POUR LE DASHBOARD (LE VRAI ÉQUIVALENT SHAP CENTRÉ)
# ==============================================================================

print("--- 2A. ENTRAÎNEMENT DU MODÈLE FINAL SUR DONNÉES BRUTES 2022 ---")
# On récupère les noms des variables sélectionnées par le Lasso (sans l'intercept)
variables_selectionnees_noms_sp <- tableau_coefs_sp %>%
  filter(Variable != "(Intercept)") %>%
  pull(Variable)

formule_post_lasso_sp <- as.formula(paste("Y_GAP_ACT_GLOBAL ~", paste(variables_selectionnees_noms_sp, collapse = " + ")))

# On entraîne le modèle linéaire sur les données brutes 2022 (SANS POPULATION)
modele_dashboard_sp <- lm(formule_post_lasso_sp, data = base_sans_na_sans_pop)

# On calcule les prédictions finales sur les communes de 2023 (SANS POPULATION)
predictions_2023_lasso_sp <- predict(modele_dashboard_sp, newdata = base_2023_sans_na_sans_pop)


print("--- 2B. CALCUL DES CONTRIBUTIONS LOCALES (LE VRAI ÉQUIVALENT SHAP) ---")
# On extrait les coefficients du modèle (sans l'Intercept)
coefs_modele_sp <- coef(modele_dashboard_sp)[-1] 
noms_variables_lasso_sp <- names(coefs_modele_sp)

# On isole les vraies valeurs des communes 2023 uniquement pour les variables du Lasso
matrice_valeurs_reelles_sp <- as.matrix(base_2023_sans_na_sans_pop[, noms_variables_lasso_sp])

# On calcule les moyennes nationales de 2022 (l'année d'apprentissage)
moyennes_nationales_sp <- colMeans(as.matrix(base_sans_na_sans_pop[, noms_variables_lasso_sp]))

# On centre les valeurs : Valeur de la commune - Moyenne nationale
matrice_valeurs_centrees_sp <- sweep(matrice_valeurs_reelles_sp, 2, moyennes_nationales_sp, "-")

# L'impact devient l'écart à la moyenne multiplié par le coefficient !
matrice_impacts_sp <- sweep(matrice_valeurs_centrees_sp, 2, coefs_modele_sp, "*")


print("--- 2C. EXTRACTION DU TOP 5 POUR CHAQUE COMMUNE ---")
extraire_top_5_lasso_sp <- function(i) {
  ligne_impact <- matrice_impacts_sp[i, ]
  
  # On garde les "vraies" valeurs brutes pour l'affichage dans le curseur du simulateur
  ligne_valeurs <- matrice_valeurs_reelles_sp[i, ]
  
  indices_top <- order(abs(ligne_impact), decreasing = TRUE)[1:5]
  
  data.frame(
    VAR_1 = noms_variables_lasso_sp[indices_top[1]], 
    VAL_1 = ligne_valeurs[indices_top[1]], 
    SHAP_1 = ligne_impact[indices_top[1]],
    
    VAR_2 = noms_variables_lasso_sp[indices_top[2]], 
    VAL_2 = ligne_valeurs[indices_top[2]], 
    SHAP_2 = ligne_impact[indices_top[2]],
    
    VAR_3 = noms_variables_lasso_sp[indices_top[3]], 
    VAL_3 = ligne_valeurs[indices_top[3]], 
    SHAP_3 = ligne_impact[indices_top[3]],
    
    VAR_4 = noms_variables_lasso_sp[indices_top[4]], 
    VAL_4 = ligne_valeurs[indices_top[4]], 
    SHAP_4 = ligne_impact[indices_top[4]],
    
    VAR_5 = noms_variables_lasso_sp[indices_top[5]], 
    VAL_5 = ligne_valeurs[indices_top[5]], 
    SHAP_5 = ligne_impact[indices_top[5]]
  )
}

# Application à toutes les communes
liste_top5_lasso_sp <- lapply(1:nrow(matrice_impacts_sp), extraire_top_5_lasso_sp)
tableau_top5_lasso_sp <- bind_rows(liste_top5_lasso_sp)


print("--- 2D. ASSEMBLAGE ET EXPORT PARQUET ---")
# On rassemble tout : Identifiants, Réel, Prédit
base_dashboard_lasso_sp <- data.frame(
  GEO = referentiel_communes_sans_pop_2023$GEO,
  GEO_LABEL = referentiel_communes_sans_pop_2023$GEO_LABEL,
  Y_OBSERVE = base_2023_sans_na_sans_pop$Y_GAP_ACT_GLOBAL,
  Y_PREDIT_LASSO = predictions_2023_lasso_sp
)

# On y colle les 5 variables impacts et leurs valeurs
base_dashboard_lasso_sp <- bind_cols(base_dashboard_lasso_sp, tableau_top5_lasso_sp)

# On sauvegarde sur le pc
nom_fichier_export_lasso_sp <- "predictions_lasso_sp_2023.parquet"
write_parquet(base_dashboard_lasso_sp, nom_fichier_export_lasso_sp)

print(paste(" Le fichier", nom_fichier_export_lasso_sp, "a été généré avec succès."))

