#===============================================================================
# FORET ALEATOIRE ALGO CREATION
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

# Vérification des tailles
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
  num.trees = 400,  # 400 arbres après analyse
  mtry = 150,
  min.node.size = 4,
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

print("--- 1. PRÉDICTION SUR LE JEU DE DONNÉES 2023 ---")

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
  
  # A. Extraction de la ligne pour Nantes
  X_2023_features <- base_2023_sans_na %>% select(-Y_GAP_ACT_GLOBAL)
  X_nantes_rf <- X_2023_features[index_commune, , drop = FALSE]
  
  # B. Prédiction Random Forest pour Nantes
  prediction_nantes_rf <- predict(modele_base, data = X_nantes_rf)$predictions
  print(paste("=> L'écart d'activité prédit par le Random Forest pour", nom_commune, "est de :", round(prediction_nantes_rf, 4)))
  
  # C. Calcul valeurs shap allégé. (20 lignes de fond pour éliminer tout risque de crash)
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
  
  # D. Affichage du graphique waterfall pour Nantes
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

# Normalisation : On applique la même règle de centrage / réduction sur 2023
base_normalisee_2023 <- base_2023_sans_na %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

# Séparation de la cible 2023
vecteur_Y_2023 <- base_normalisee_2023$Y_GAP_ACT_GLOBAL

# Transformation en matrice (doit avoir exactement les mêmes colonnes que la matrice_X)
matrice_X_2023 <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee_2023)

print("--- 2. PRÉDICTION SUR LE JEU DE DONNÉES 2023 ---")

# On applique le modèle Lasso (qui a le meilleur lambda) sur la matrice 2023
predictions_2023_lasso <- predict(modele_lasso_cv, s = meilleur_lambda, newx = matrice_X_2023)

print("--- 3. ÉVALUATION DE LA ROBUSTESSE (DATA DRIFT) ---")

# Calcul du R2 sur les données de 2023
r2_2023_lasso <- cor(as.vector(predictions_2023_lasso), vecteur_Y_2023)^2

# Calcul de la RMSE sur 2023
rmse_2023_lasso <- rmse(vecteur_Y_2023, as.vector(predictions_2023_lasso))

print(paste("Performance R2 Lasso sur 2023 :", round(r2_2023_lasso, 4)))
print(paste("Erreur RMSE Lasso sur 2023 :", round(rmse_2023_lasso, 4)))

print("--- BILAN : COMPARAISON RANDOM FOREST vs LASSO ---")
print(paste("RMSE Random Forest :", round(rmse_2023, 4)))
print(paste("RMSE Lasso :", round(rmse_2023_lasso, 4)))