# ==============================================================================
# PARTIE 3 (CORRIGÉE) : LE MODÈLE LINÉAIRE PÉNALISÉ (GLM LASSO) AVEC NORMALISATION
# ==============================================================================
install.packages("plotly")
install.packages("glmnet")
library(glmnet)
library(dplyr)

print("--- 1. NORMALISATION ET PRÉPARATION DE LA MATRICE ---")

# NORMALISATION : On centre et on réduit toutes les variables (sauf le Y)
base_normalisee <- base_sans_na %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

# Séparation de la variable cible (Y)
vecteur_Y <- base_normalisee$Y_GAP_ACT_GLOBAL

# Transformation du tableau en matrice mathématique
matrice_X <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee)

print(paste("Dimension de la matrice X :", nrow(matrice_X), "lignes et", ncol(matrice_X), "colonnes."))

print("--- 2. ENTRAÎNEMENT ET VALIDATION CROISÉE INTERNE ---")

modele_lasso_cv <- cv.glmnet(
  x = matrice_X, 
  y = vecteur_Y, 
  alpha = 1,            
  standardize = FALSE,   # Déjà fait 
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

# ==============================================================================
# POST-LASSO : OBTENTION DES P-VALUES ET TESTS DE STUDENT
# ==============================================================================

#Nous permet d'avoir les p-values de toutes nos variables. Les variables de tête sont trés significatives avec des p values miniscule et celles avec des pénalités ne le sont pas. 

# 1. On récupère les noms des variables sélectionnées par le Lasso (en excluant l'intercept)
variables_selectionnees_noms <- tableau_coefs %>%
  filter(Variable != "(Intercept)") %>%
  pull(Variable)

# 2. On reconstruit une formule dynamique avec uniquement ces variables
formule_post_lasso <- as.formula(paste("Y_GAP_ACT_GLOBAL ~", paste(variables_selectionnees_noms, collapse = " + ")))

# 3. On lance une régression linéaire classique (OLS) sur ces variables
modele_post_lasso <- lm(formule_post_lasso, data = base_normalisee)

# 4. Affichage du résumé complet (Coefficients, Erreurs types, t-value et p-values !)
summary(modele_post_lasso)