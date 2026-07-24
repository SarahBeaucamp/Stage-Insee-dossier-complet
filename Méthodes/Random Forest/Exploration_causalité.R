# ==============================================================================
# PARTIE 6 : OUVERTURE CAUSALE - DOUBLE MACHINE LEARNING (DML)
# ==============================================================================

# Installation des packages nécessaires (décommente si besoin)
install.packages(c("DoubleML", "mlr3", "mlr3learners"))
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(xgboost)

print("--- 1. PRÉPARATION DES DONNÉES POUR LE DOUBLE ML ---")

# On définit notre variable cible (Y) et notre variable de "traitement" (D)
# Le traitement est la variable dont on veut isoler l'effet causal (ex: le Taux de pauvreté)
variable_cible <- "Y_GAP_ACT_GLOBAL"
variable_traitement <- "Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie"

# Configuration de l'objet de données DML
dml_data <- DoubleMLData$new(
  data = base_sans_na,
  y_col = variable_cible,
  d_cols = variable_traitement
)

print("--- 2. CONFIGURATION DES APPRENTIS (XGBOOST) ---")

# On définit que l'on va utiliser XGBoost pour modéliser Y (l'écart) et D (la pauvreté)
# On fixe des hyperparamètres rapides et robustes
ml_g = lrn("regr.xgboost", max_depth = 6, eta = 0.1, nrounds = 100) # Pour modéliser Y
ml_m = lrn("regr.xgboost", max_depth = 6, eta = 0.1, nrounds = 100) # Pour modéliser D

print("--- 3. ESTIMATION CAUSALE (MODÈLE PARTIELLEMENT LINÉAIRE) ---")

# Création de l'objet DoubleML Partially Linear Regression (PLR)
set.seed(42)
dml_plr = DoubleMLPLR$new(
  data = dml_data,
  ml_l = ml_g,
  ml_m = ml_m,
  n_folds = 5 # Validation croisée pour éviter le surapprentissage
)

# Lancement du calcul (peut prendre quelques minutes)
dml_plr$fit()

print("--- VERDICT CAUSAL ---")
print(dml_plr$summary())