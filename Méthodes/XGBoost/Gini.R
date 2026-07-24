# ==============================================================================
# PARTIE 5A : L'IMPORTANCE CLASSIQUE (RÉDUCTION D'IMPURETÉ / GAIN)
# ==============================================================================

# Le package Ckmeans.1d.dp est souvent recommandé par xgboost pour un meilleur rendu graphique
install.packages("Ckmeans.1d.dp") 
library(Ckmeans.1d.dp)
library(ggplot2)

print("--- CALCUL DE L'IMPORTANCE DES VARIABLES (GAIN) ---")

# Extraction de la matrice d'importance directement depuis le modèle XGBoost
# On lui fournit les noms des colonnes pour qu'il ne nous affiche pas juste "f1, f2..."
importance_matrice <- xgb.importance(
  feature_names = colnames(X_train), 
  model = modele_xgb
)

# Affichage du Top 15 brut dans la console
print("--- TOP 15 DES VARIABLES (IMPORTANCE GLOBALE) ---")
print(head(importance_matrice, 15))

# Génération du graphique
print("-> Génération du graphique dans l'onglet 'Plots'...")
xgb.plot.importance(
  importance_matrix = importance_matrice, 
  top_n = 15, 
  measure = "Gain", 
  main = "Top 15 des variables explicatives (Méthode du Gain / Gini)"
)