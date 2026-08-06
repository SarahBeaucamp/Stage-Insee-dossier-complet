# Le modèle 'modele_base' que tu as entraîné contient déjà l'erreur OOB
print(modele_base)

library(dplyr)

# Extraction des importances
importance_rf <- ranger::importance(modele_base)

# Transformation en beau tableau trié
top_variables_rf <- data.frame(
  Variable = names(importance_rf),
  Importance = as.numeric(importance_rf)
) %>%
  arrange(desc(Importance))

# Affichage du Top 15 des variables selon la Forêt Aléatoire
print(head(top_variables_rf, 15))

# On récupère les prédictions sur l'échantillon de validation
predictions_valeurs <- predict(modele_base, data = base_val)$predictions
valeurs_reelles <- base_val$Y_GAP_ACT_GLOBAL

# Nuage de points Prédictions vs Réalité
plot(valeurs_reelles, predictions_valeurs, 
     main = "Random Forest : Valeurs Réelles vs Prédites",
     xlab = "Écart d'activité réel", 
     ylab = "Écart d'activité prédit par la Forêt",
     pch = 20, col = rgb(0, 0, 1, 0.3))

# Ajout de la bissectrice parfaite (ligne y = x)
abline(0, 1, col = "red", lwd = 2)