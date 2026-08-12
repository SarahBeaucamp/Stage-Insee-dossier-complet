# ==============================================================================
# DOUBLE MACHINE LEARNING (INFÉRENCE CAUSALE)
# ==============================================================================
library(ranger)
library(dplyr)

print("--- 1. DÉFINITION DES VARIABLES DU DML ---")

# Y : Notre variable cible
var_cible <- "Y_GAP_ACT_GLOBAL"

# T : Le "Traitement" (La politique publique que l'on veut évaluer isolément)
var_traitement <- "Établissements...Aide.à.domicile...Accueil.de.jeunes.enfants...Activités.des.ménages.en.tant.qu.employeurs.de.personnel.domestique"

# X : Les facteurs de confusion (Toutes les autres variables de la base)
variables_X <- setdiff(names(base_train), c(var_cible, var_traitement))


print("--- 2. CRÉATION DES DEUX MODÈLES RANDOM FOREST (CROSS-FITTING) ---")
# Pour éviter le surapprentissage, le DML exige de prédire sur des données non vues.
# On utilise ici le découpage existant (base_train pour apprendre, base_val pour prédire).

# Formules dynamiques
formule_Y <- as.formula(paste(var_cible, "~", paste(variables_X, collapse = " + ")))
formule_T <- as.formula(paste(var_traitement, "~", paste(variables_X, collapse = " + ")))

# Modèle 1 : Prédire Y (Écart d'activité) à partir de X (sans T)
rf_Y <- ranger(
  formula = formule_Y, 
  data = base_train, 
  num.trees = 400, mtry = 150, min.node.size = 4
)

# Modèle 2 : Prédire T (Présence de crèches) à partir de X (sans Y)
rf_T <- ranger(
  formula = formule_T, 
  data = base_train, 
  num.trees = 400, mtry = 150, min.node.size = 4
)


print("--- 3. CALCUL DES RÉSIDUS SUR LA BASE DE VALIDATION ---")
# On isole la partie que la machine n'arrive pas à expliquer (l'aléa "pur")
pred_Y_val <- predict(rf_Y, data = base_val)$predictions
residus_Y <- base_val[[var_cible]] - pred_Y_val

pred_T_val <- predict(rf_T, data = base_val)$predictions
residus_T <- base_val[[var_traitement]] - pred_T_val


print("--- 4. L'ESTIMATION CAUSALE FINALE ---")
# On régresse les résidus de Y sur les résidus de T
modele_causal <- lm(residus_Y ~ residus_T)

# Affichage des résultats purs
summary(modele_causal)

# ==============================================================================
# DOUBLE MACHINE LEARNING SUR LE TAUX DE PAUVRETÉ
# ==============================================================================
library(ranger)
library(dplyr)

print("--- 1. DÉFINITION DES VARIABLES DU DML (PAUVRETÉ) ---")

var_cible <- "Y_GAP_ACT_GLOBAL"

var_traitement <- "Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie"

variables_X <- setdiff(names(base_train), c(var_cible, var_traitement))


print("--- 2. ENTRAÎNEMENT DES DEUX MODÈLES RANDOM FOREST ---")
formule_Y <- as.formula(paste(var_cible, "~", paste(variables_X, collapse = " + ")))
formule_T <- as.formula(paste(var_traitement, "~", paste(variables_X, collapse = " + ")))

print("Entraînement du modèle Y (Écart d'activité)...")
rf_Y_pauvrete <- ranger(
  formula = formule_Y, 
  data = base_train, 
  num.trees = 400, mtry = 150, min.node.size = 4
)

print("Entraînement du modèle T (Taux de pauvreté)...")
rf_T_pauvrete <- ranger(
  formula = formule_T, 
  data = base_train, 
  num.trees = 400, mtry = 150, min.node.size = 4
)


print("--- 3. CALCUL DES RÉSIDUS SUR LA BASE DE VALIDATION ---")
pred_Y_val_pauvrete <- predict(rf_Y_pauvrete, data = base_val)$predictions
residus_Y_pauvrete <- base_val[[var_cible]] - pred_Y_val_pauvrete

pred_T_val_pauvrete <- predict(rf_T_pauvrete, data = base_val)$predictions
residus_T_pauvrete <- base_val[[var_traitement]] - pred_T_val_pauvrete


print("--- 4. L'ESTIMATION CAUSALE FINALE ---")
modele_causal_pauvrete <- lm(residus_Y_pauvrete ~ residus_T_pauvrete)

summary(modele_causal_pauvrete)

# ==============================================================================
# DOUBLE MACHINE LEARNING (FAMILLES NOMBREUSES & SALAIRES HOMMES)
# ==============================================================================
library(ranger)
library(dplyr)

print("--- PRÉPARATION DES VARIABLES ---")
var_cible <- "Y_GAP_ACT_GLOBAL"

var_traitement_1 <- "Nombre.de.famille...4.enfants.ou.plus.de.moins.de.24.ans"
var_traitement_2 <- "Salaire.net.EQTP.mensuel.moyen...Homme.x.De.25.à.39.ans"

# ==============================================================================
# TEST 1 : L'EFFET DES FAMILLES DE 4 ENFANTS OU PLUS
# ==============================================================================
print("--- TEST 1 : ENTRAÎNEMENT DML SUR LES FAMILLES NOMBREUSES ---")

variables_X_1 <- setdiff(names(base_train), c(var_cible, var_traitement_1))
formule_Y_1 <- as.formula(paste(var_cible, "~", paste(variables_X_1, collapse = " + ")))
formule_T_1 <- as.formula(paste(var_traitement_1, "~", paste(variables_X_1, collapse = " + ")))

rf_Y_1 <- ranger(formula = formule_Y_1, data = base_train, num.trees = 200, mtry = 150, min.node.size = 4)
rf_T_1 <- ranger(formula = formule_T_1, data = base_train, num.trees = 200, mtry = 150, min.node.size = 4)

residus_Y_1 <- base_val[[var_cible]] - predict(rf_Y_1, data = base_val)$predictions
residus_T_1 <- base_val[[var_traitement_1]] - predict(rf_T_1, data = base_val)$predictions

modele_causal_1 <- lm(residus_Y_1 ~ residus_T_1)

# ==============================================================================
# TEST 2 : L'EFFET DU SALAIRE "MONSIEUR GAGNE-PAIN"
# ==============================================================================
print("--- TEST 2 : ENTRAÎNEMENT DML SUR LE SALAIRE MASCULIN (25-39 ANS) ---")

variables_X_2 <- setdiff(names(base_train), c(var_cible, var_traitement_2))
formule_Y_2 <- as.formula(paste(var_cible, "~", paste(variables_X_2, collapse = " + ")))
formule_T_2 <- as.formula(paste(var_traitement_2, "~", paste(variables_X_2, collapse = " + ")))

rf_Y_2 <- ranger(formula = formule_Y_2, data = base_train, num.trees = 200, mtry = 150, min.node.size = 4)
rf_T_2 <- ranger(formula = formule_T_2, data = base_train, num.trees = 200, mtry = 150, min.node.size = 4)

residus_Y_2 <- base_val[[var_cible]] - predict(rf_Y_2, data = base_val)$predictions
residus_T_2 <- base_val[[var_traitement_2]] - predict(rf_T_2, data = base_val)$predictions

modele_causal_2 <- lm(residus_Y_2 ~ residus_T_2)

# ==============================================================================
# RÉSULTATS FINAUX
# ==============================================================================
print("====== RÉSULTAT CAUSAL 1 : FAMILLES DE 4 ENFANTS OU PLUS ======")
print(summary(modele_causal_1))

print("====== RÉSULTAT CAUSAL 2 : SALAIRE MASCULIN (25-39 ans) ======")
print(summary(modele_causal_2))