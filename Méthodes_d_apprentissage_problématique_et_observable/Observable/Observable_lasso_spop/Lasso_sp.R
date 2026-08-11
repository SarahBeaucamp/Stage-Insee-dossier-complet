# ==============================================================================
# PARTIE 3 : LE MODÈLE LINÉAIRE PÉNALISÉ (GLM LASSO) AVEC NORMALISATION
# ==============================================================================
install.packages("glmnet")
library(glmnet)
library(dplyr)

print("--- 1. NORMALISATION ET PRÉPARATION DE LA MATRICE ---")

# Normalisation : On centre et on réduit toutes les variables (sauf le Y)
base_normalisee_sans_pop <- base_sans_na_sans_pop %>%
  mutate(across(-Y_GAP_ACT_GLOBAL, ~ scale(.) %>% as.numeric()))

# Séparation de la variable cible (Y)
vecteur_Y_sans_pop <- base_normalisee_sans_pop$Y_GAP_ACT_GLOBAL

# Transformation du tableau en matrice mathématique pure
matrice_X_sans_pop <- model.matrix(Y_GAP_ACT_GLOBAL ~ . - 1, data = base_normalisee_sans_pop)

print(paste("Dimension de la matrice X :", nrow(matrice_X_sans_pop), "lignes et", ncol(matrice_X_sans_pop), "colonnes."))

print("--- 2. ENTRAÎNEMENT ET VALIDATION CROISÉE INTERNE ---")


modele_lasso_cv_sans_pop <- cv.glmnet(
  x = matrice_X_sans_pop, 
  y = vecteur_Y_sans_pop, 
  alpha = 1,            
  standardize = FALSE,   # Déjà fait 
  nfolds = 5            
)

# Extraction de la meilleure pénalité trouvée
meilleur_lambda_sans_pop <- modele_lasso_cv_sans_pop$lambda.min
print(paste("La meilleure pénalité mathématique (lambda) trouvée est :", round(meilleur_lambda, 5)))

print("--- 3. LE VERDICT : LA GRANDE ÉLIMINATION (COEFFICIENTS STANDARDISÉS) ---")

# On extrait les coefficients
coefficients_lasso_sans_pop <- coef(modele_lasso_cv_sans_pop, s = "lambda.min")
noms_variables_sans_pop <- rownames(coefficients_lasso_sans_pop)
valeurs_coefs_sans_pop <- as.numeric(coefficients_lasso_sans_pop)

tableau_coefs_sans_pop <- data.frame(
  Variable = noms_variables,
  Coefficient = valeurs_coefs
) %>%
  filter(Coefficient != 0) %>% 
  arrange(desc(abs(Coefficient)))

variables_conservees_sans_pop <- nrow(tableau_coefs) - 1 

print(paste("Sur les", ncol(matrice_X_sans_pop), "variables initiales, le Lasso en a écrasé", 
            ncol(matrice_X_sans_pop) - variables_conservees_sans_pop, "et n'en a conservé que", variables_conservees_sans_pop, "!"))

print("--- VOICI LE NOUVEAU TOP 15 COMPARABLE DE TON ÉQUATION ---")
print(head(tableau_coefs, 16))

# ==============================================================================
# POST-LASSO : OBTENTION DES P-VALUES ET TESTS DE STUDENT
# ==============================================================================

# Nous permet d'avoir les p-values de toutes nos variables. Les variables de tête sont trés significatives avec des p values miniscule et celles avec des pénalités ne le sont pas. 

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