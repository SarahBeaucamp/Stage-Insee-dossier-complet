# ==============================================================================
# PARTIE 4 : L'EXTREME GRADIENT BOOSTING (XGBOOST)
# ==============================================================================

# Installation du package si nécessaire
install.packages("xgboost")
install.packages("rsample")
library(xgboost)
library(rsample)
library(dplyr)

print("--- 1. SÉPARATION DE LA BASE (TRAIN / TEST) ---")
# On coupe la base propre et imputée (base_sans_na) en apprentissage (80%) et test (20%)
set.seed(42)
split_final_sp <- initial_split(base_sans_na_sans_pop, prop = 0.80)
base_train_sp <- training(split_final_sp)
base_test_sp  <- testing(split_final_sp)

print("--- 2. PRÉPARATION DES MATRICES MATHÉMATIQUES ---")
# L'algorithme XGBoost exige des matrices (tout comme le Lasso).
# On sépare la variable cible (Y) des variables explicatives (X)

Y_train_sp <- base_train_sp$Y_GAP_ACT_GLOBAL
# Le "- 1" retire l'intercept pour obtenir une matrice pure
X_train_sp <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_train_sp)

Y_test_sp <- base_test_sp$Y_GAP_ACT_GLOBAL
X_test_sp <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_test_sp)

# Création des objets "DMatrix" : un format ultra-optimisé et compressé propre à XGBoost
dtrain_sp <- xgb.DMatrix(data = X_train_sp, label = Y_train_sp)
dtest_sp  <- xgb.DMatrix(data = X_test_sp, label = Y_test_sp)


print("--- 3. ENTRAÎNEMENT DU MODÈLE (APPRENTISSAGE SÉQUENTIEL) ---")
# Définition des hyperparamètres classiques pour une bonne première exécution
params_xgb_sp <- list(
  objective = "reg:squarederror", # Fonction de perte pour la régression (RMSE)
  eta = 0.1,                      # Taux d'apprentissage (Learning rate)
  max_depth = 6,                  # Profondeur maximale de chaque arbre
  subsample = 0.8,                # Tirage au sort de 80% des lignes à chaque arbre (anti-surapprentissage)
  colsample_bytree = 0.8          # Tirage au sort de 80% des colonnes à chaque arbre
)

# La "watchlist" permet de voir le modèle s'améliorer en direct sur l'échantillon de Test
watchlist_sp <- list(train_sp = dtrain_sp, test_sp = dtest_sp)

# Lancement de l'entraînement
set.seed(42)
modele_xgb_sp <- xgb.train(
  params = params_xgb_sp,
  data = dtrain_sp,
  nrounds = 500,                  # Le modèle va construire un maximum de 500 arbres
  watchlist = watchlist_sp,
  print_every_n = 50,             # Affiche le score tous les 50 arbres
  early_stopping_rounds = 20      # Stoppe tout si le score sur l'échantillon Test ne s'améliore plus pendant 20 tours
)


print("--- 4. LE VERDICT : LE BENCHMARK SUR L'ÉCHANTILLON TEST ---")
# Prédiction finale sur la base de test
preds_xgb_sp <- predict(modele_xgb_sp, dtest_sp)

# Calcul des erreurs et du R²
rmse_xgb_sp <- sqrt(mean((Y_test_sp - preds_xgb_sp)^2))
moy_train_sp <- mean(Y_train_sp)
rmse_naif_sp <- sqrt(mean((Y_test_sp - moy_train_sp)^2))

r2_xgb_sp <- (1 - (rmse_xgb_sp^2 / rmse_naif_sp^2)) * 100

print(paste("-> RMSE du XGBoost (Erreur absolue) :", round(rmse_xgb_sp, 5)))
print(paste("-> R2 du XGBoost (Score final)      :", round(r2_xgb_sp, 2), "%"))

# ==============================================================================
# PARTIE 5B (CORRIGÉE) : LES VALEURS SHAP
# ==============================================================================
install.packages("shapviz")
library(shapviz)
library(ggplot2)

print("--- CALCUL DES VALEURS SHAP ---")

# Correction : on spécifie à la fois X_data et X_pred pour éviter l'erreur d'argument
valeurs_shap_sp <- shapviz(modele_xgb_sp, X_data = X_test_sp, X_pred = X_test_sp)

print("-> Génération du graphique SHAP (Beeswarm) dans l'onglet 'Plots'...")

# Affichage du graphique d'impact
sv_importance(valeurs_shap_sp, kind = "beeswarm", max_display = 15) +
  ggtitle("Impact SHAP des 15 variables principales sur l'écart d'activité (sp)") +
  theme_minimal()
