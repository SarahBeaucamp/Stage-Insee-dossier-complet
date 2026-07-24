# ==============================================================================
# PARTIE 4 : L'EXTREME GRADIENT BOOSTING (XGBOOST)
# ==============================================================================

# Installation du package si nécessaire
# install.packages("xgboost")
library(xgboost)
library(rsample)
library(dplyr)

print("--- 1. SÉPARATION DE LA BASE (TRAIN / TEST) ---")
# On coupe la base propre et imputée (base_sans_na) en apprentissage (80%) et test (20%)
set.seed(42)
split_final <- initial_split(base_sans_na, prop = 0.80)
base_train <- training(split_final)
base_test  <- testing(split_final)

print("--- 2. PRÉPARATION DES MATRICES MATHÉMATIQUES ---")
# L'algorithme XGBoost exige des matrices (tout comme le Lasso).
# On sépare la variable cible (Y) des variables explicatives (X)

Y_train <- base_train$Y_GAP_ACT_GLOBAL
# Le "- 1" retire l'intercept pour obtenir une matrice pure
X_train <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_train)

Y_test <- base_test$Y_GAP_ACT_GLOBAL
X_test <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_test)

# Création des objets "DMatrix" : un format ultra-optimisé et compressé propre à XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = Y_train)
dtest  <- xgb.DMatrix(data = X_test, label = Y_test)


print("--- 3. ENTRAÎNEMENT DU MODÈLE (APPRENTISSAGE SÉQUENTIEL) ---")
# Définition des hyperparamètres classiques pour une bonne première exécution
params_xgb <- list(
  objective = "reg:squarederror", # Fonction de perte pour la régression (RMSE)
  eta = 0.1,                      # Taux d'apprentissage (Learning rate)
  max_depth = 6,                  # Profondeur maximale de chaque arbre
  subsample = 0.8,                # Tirage au sort de 80% des lignes à chaque arbre (anti-surapprentissage)
  colsample_bytree = 0.8          # Tirage au sort de 80% des colonnes à chaque arbre
)

# La "watchlist" permet de voir le modèle s'améliorer en direct sur l'échantillon de Test
watchlist <- list(train = dtrain, test = dtest)

# Lancement de l'entraînement
set.seed(42)
modele_xgb <- xgb.train(
  params = params_xgb,
  data = dtrain,
  nrounds = 500,                  # Le modèle va construire un maximum de 500 arbres
  watchlist = watchlist,
  print_every_n = 50,             # Affiche le score tous les 50 arbres
  early_stopping_rounds = 20      # Stoppe tout si le score sur l'échantillon Test ne s'améliore plus pendant 20 tours
)


print("--- 4. LE VERDICT : LE BENCHMARK SUR L'ÉCHANTILLON TEST ---")
# Prédiction finale sur la base de test
preds_xgb <- predict(modele_xgb, dtest)

# Calcul des erreurs et du R²
rmse_xgb <- sqrt(mean((Y_test - preds_xgb)^2))
moy_train <- mean(Y_train)
rmse_naif <- sqrt(mean((Y_test - moy_train)^2))

r2_xgb <- (1 - (rmse_xgb^2 / rmse_naif^2)) * 100

print(paste("-> RMSE du XGBoost (Erreur absolue) :", round(rmse_xgb, 5)))
print(paste("-> R2 du XGBoost (Score final)      :", round(r2_xgb, 2), "%"))