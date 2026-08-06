# ==============================================================================
# Modèle de régression Lasso
# ==============================================================================
install.packages("glmnet")
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

modele_lasso_cv <- cv.glmnet(
  x = matrice_X, 
  y = vecteur_Y, 
  alpha = 1,            
  standardize = FALSE, # Déjà fait  
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

# ==============================================================================
# CRITÈRES D'INFORMATION : AIC ET BIC
# ==============================================================================

print("--- 5. CALCUL DE L'AIC ET DU BIC ---")

# L'AIC et le BIC se calculent très facilement sur le modèle OLS final (modele_post_lasso)
aic_valeur <- AIC(modele_post_lasso)
bic_valeur <- BIC(modele_post_lasso)

print(paste("Score AIC du modèle Post-Lasso :", round(aic_valeur, 2)))
print(paste("Score BIC du modèle Post-Lasso :", round(bic_valeur, 2)))

# Pour que l'analyse soit utile, il faut les comparer au modèle OLS "complet" (avec toutes les variables)
# On crée rapidement un modèle classique sans sélection Lasso pour comparer :
modele_complet_ols <- lm(Y_GAP_ACT_GLOBAL ~ ., data = base_normalisee)

aic_complet <- AIC(modele_complet_ols)
bic_complet <- BIC(modele_complet_ols)

print(paste("Score AIC du modèle complet (sans sélection) :", round(aic_complet, 2)))
print(paste("Score BIC du modèle complet (sans sélection) :", round(bic_complet, 2)))

print(paste("Gain d'AIC grâce au Lasso :", round(aic_complet - aic_valeur, 2)))
print(paste("Gain de BIC grâce au Lasso :", round(bic_complet - bic_valeur, 2)))

# ==============================================================================
# 1. ÉVOLUTION DE L'ERREUR (VALIDATION CROISÉE LASSO)
# ==============================================================================

# Ce graphique trace la courbe de l'erreur en fonction du paramètre Lambda
plot(modele_lasso_cv)
title("Validation croisée : Choix du paramètre de pénalité (Lambda)", line = 3)

# ==============================================================================
# 2, 3, 4. DIAGNOSTICS DU MODÈLE POST-LASSO (RÉGRESSION LINÉAIRE)
# ==============================================================================

# On divise la fenêtre graphique en 4 (2 lignes, 2 colonnes) pour tout voir d'un coup
par(mfrow = c(2, 2))

# On génère les 4 graphiques de diagnostic
plot(modele_post_lasso)

# On remet la fenêtre graphique à la normale
par(mfrow = c(1, 1))


# ==============================================================================
# TEST DE MORAN : AUTOCORRÉLATION SPATIALE DES RÉSIDUS ( si je m'ennuie et que je charge un fonc de carte avec le tracé des communes)
# ==============================================================================
install.packages("sf")
install.packages("spdep")
library(sf)
library(spdep)
library(dplyr)

print("--- 1. PRÉPARATION DES DONNÉES SPATIALES ---")
# On extrait les résidus de ton modèle Post-Lasso
# (Assure-toi que base_normalisee contient bien la colonne GEO / Code Commune)
base_residus <- data.frame(
  GEO = base_normalisee$GEO, 
  Residus = residuals(modele_post_lasso)
)

# /!\ REMPLACE 'carte_communes' par le nom de ton vrai objet spatial dans R
# On fusionne la géométrie avec les résidus de notre modèle
carte_analyse <- carte_communes %>%
  inner_join(base_residus, by = "GEO")

print("--- 2. CRÉATION DE LA MATRICE DE VOISINAGE ---")
# On identifie les voisins de chaque commune (contiguïté : elles partagent une frontière)
voisins <- poly2nb(carte_analyse)

# On transforme ces voisinages en matrice de poids mathématiques
# (zero.policy = TRUE permet d'éviter que le code plante sur les îles comme Ouessant ou la Corse)
poids_spatiaux <- nb2listw(voisins, style = "W", zero.policy = TRUE)

print("--- 3. LE TEST DE MORAN SUR LE MODÈLE ---")
# On lance le test mathématique global
test_moran <- lm.morantest(modele_post_lasso, listw = poids_spatiaux, zero.policy = TRUE)

# Affichage du résultat
print(test_moran)