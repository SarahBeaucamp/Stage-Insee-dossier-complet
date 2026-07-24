# ==============================================================================
# OPTIMISATION DE LA FORÊT ALÉATOIRE : TEST DU NOMBRE D'ARBRES
# ==============================================================================

# Installation de ranger si nécessaire
# install.packages("ranger")
library(ranger)
library(dplyr)

print("--- 1. INITIALISATION DE L'OPTIMISATION ---")

# Création d'un tableau vide pour stocker nos scores
resultats_rf <- data.frame(
  Nb_Arbres = integer(),
  RMSE = numeric(),
  R2 = numeric()
)

# La liste des arbres que tu souhaites tester
liste_arbres <- c(100, 200, 300, 400, 500)

print("--- 2. ENTRAÎNEMENT ET ÉVALUATION EN BOUCLE ---")

# Calcul de l'erreur du modèle naïf (nécessaire pour le calcul du R2)
moyenne_train <- mean(base_train$Y_GAP_ACT_GLOBAL)
rmse_naif <- sqrt(mean((base_test$Y_GAP_ACT_GLOBAL - moyenne_train)^2))

# La boucle qui va tester chaque nombre d'arbres
for (n in liste_arbres) {
  print(paste("-> Entraînement de la forêt avec", n, "arbres..."))
  
  # Fixer la graine pour garantir la reproductibilité parfaite de tes résultats
  set.seed(42)
  
  # Entraînement du modèle
  modele_rf_test <- ranger(
    formula = Y_GAP_ACT_GLOBAL ~ .,
    data = base_train,
    num.trees = n,
    importance = 'impurity' # Calcule l'importance Gini en coulisses
  )
  
  # Prédiction sur l'échantillon test
  preds_rf_test <- predict(modele_rf_test, data = base_test)$predictions
  
  # Calcul du RMSE
  rmse_rf_test <- sqrt(mean((base_test$Y_GAP_ACT_GLOBAL - preds_rf_test)^2))
  
  # Calcul du R2 (en pourcentage)
  r2_rf_test <- (1 - (rmse_rf_test^2 / rmse_naif^2)) * 100
  
  # Enregistrement des résultats dans notre tableau
  resultats_rf <- rbind(resultats_rf, data.frame(
    Nb_Arbres = n, 
    RMSE = rmse_rf_test, 
    R2 = r2_rf_test
  ))
}

print("--- 3. VERDICT : ÉVOLUTION DES PERFORMANCES ---")
print(resultats_rf)