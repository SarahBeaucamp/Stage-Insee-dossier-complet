# ==============================================================================
# PARTIE 3 (CORRIGÉE) : LE MODÈLE LINÉAIRE PÉNALISÉ (GLM LASSO) AVEC NORMALISATION
# ==============================================================================

library(glmnet)
library(dplyr)

print("--- 1. NORMALISATION ET PRÉPARATION DE LA MATRICE ---")

# NORMALISATION : On centre et on réduit toutes les variables (sauf le Y)
base_normalisee <- base_sans_na %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

# Séparation de la variable cible (Y)
vecteur_Y <- base_normalisee$Y_GAP_ACT_GLOBAL

# Transformation du tableau en matrice mathématique pure
matrice_X <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee)

print(paste("Dimension de la matrice X :", nrow(matrice_X), "lignes et", ncol(matrice_X), "colonnes."))

print("--- 2. ENTRAÎNEMENT ET VALIDATION CROISÉE INTERNE ---")

# On laisse standardize = FALSE car nous l'avons fait nous-mêmes !
modele_lasso_cv <- cv.glmnet(
  x = matrice_X, 
  y = vecteur_Y, 
  alpha = 1,            
  standardize = FALSE,   
  nfolds = 5            
)

# Extraction de la meilleure pénalité trouvée
meilleur_lambda <- modele_lasso_cv$lambda.min
print(paste("La meilleure pénalité mathématique (lambda) trouvée est :", round(meilleur_lambda, 5)))

print("--- 3. LE VERDICT : LA GRANDE ÉLIMINATION (COEFFICIENTS STANDARDISÉS) ---")

# On extrait les coefficients
coefficients_lasso <- coef(modele_lasso_cv, s = "lambda.min")
noms_variables <- rownames(coefficients_lasso)
valeurs_coefs <- as.numeric(coefficients_lasso)

tableau_coefs <- data.frame(
  Variable = noms_variables,
  Coefficient = valeurs_coefs
) %>%
  filter(Coefficient != 0) %>% 
  arrange(desc(abs(Coefficient)))

variables_conservees <- nrow(tableau_coefs) - 1 

print(paste("Sur les", ncol(matrice_X), "variables initiales, le Lasso en a écrasé", 
            ncol(matrice_X) - variables_conservees, "et n'en a conservé que", variables_conservees, "!"))

print("--- VOICI LE NOUVEAU TOP 15 COMPARABLE DE TON ÉQUATION ---")
print(head(tableau_coefs, 16))