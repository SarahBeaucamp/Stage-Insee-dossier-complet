# ==============================================================================
# ETAPE 9 : DOUBLE MACHINE LEARNING (INFÉRENCE CAUSALE)
# ==============================================================================
library(ranger)
library(dplyr)

print("--- 1. DÉFINITION DES VARIABLES DU DML ---")

# Y : Notre variable cible (L'écart d'activité)
var_cible <- "Y_GAP_ACT_GLOBAL"

# T : Le "Traitement" (La politique publique que l'on veut évaluer isolément)
# Tu peux changer cette variable pour évaluer l'impact d'autre chose !
var_traitement <- "Établissements...Aide.à.domicile...Accueil.de.jeunes.enfants...Activités.des.ménages.en.tant.qu.employeurs.de.personnel.domestique"

# X : Les facteurs de confusion (Toutes les autres variables de la base)
variables_X <- setdiff(names(base_train), c(var_cible, var_traitement))


print("--- 2. CRÉATION DES DEUX MODÈLES RANDOM FOREST (CROSS-FITTING) ---")
# Pour éviter le surapprentissage, le DML exige de prédire sur des données non vues.
# On utilise ici ton découpage existant (base_train pour apprendre, base_val pour prédire).

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
# ETAPE 9-B : DOUBLE MACHINE LEARNING SUR LE TAUX DE PAUVRETÉ
# ==============================================================================
library(ranger)
library(dplyr)

print("--- 1. DÉFINITION DES VARIABLES DU DML (PAUVRETÉ) ---")

# Y : Notre variable cible (L'écart d'activité)
var_cible <- "Y_GAP_ACT_GLOBAL"

# T : Le "Traitement" (Le Taux de pauvreté au seuil de 60%)
var_traitement <- "Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie"

# X : Les facteurs de confusion (Toutes les autres variables de la base)
variables_X <- setdiff(names(base_train), c(var_cible, var_traitement))


print("--- 2. ENTRAÎNEMENT DES DEUX MODÈLES RANDOM FOREST ---")
# Formules dynamiques
formule_Y <- as.formula(paste(var_cible, "~", paste(variables_X, collapse = " + ")))
formule_T <- as.formula(paste(var_traitement, "~", paste(variables_X, collapse = " + ")))

# Modèle 1 : Prédire l'Écart d'activité (Y) sans le taux de pauvreté
print("Entraînement du modèle Y (Écart d'activité)...")
rf_Y_pauvrete <- ranger(
  formula = formule_Y, 
  data = base_train, 
  num.trees = 400, mtry = 150, min.node.size = 4
)

# Modèle 2 : Prédire le Taux de Pauvreté (T) avec toutes les autres caractéristiques
print("Entraînement du modèle T (Taux de pauvreté)...")
rf_T_pauvrete <- ranger(
  formula = formule_T, 
  data = base_train, 
  num.trees = 400, mtry = 150, min.node.size = 4
)


print("--- 3. CALCUL DES RÉSIDUS SUR LA BASE DE VALIDATION ---")
# Extraction des prédictions sur l'échantillon de validation (pour éviter le surapprentissage)
pred_Y_val_pauvrete <- predict(rf_Y_pauvrete, data = base_val)$predictions
residus_Y_pauvrete <- base_val[[var_cible]] - pred_Y_val_pauvrete

pred_T_val_pauvrete <- predict(rf_T_pauvrete, data = base_val)$predictions
residus_T_pauvrete <- base_val[[var_traitement]] - pred_T_val_pauvrete


print("--- 4. L'ESTIMATION CAUSALE FINALE ---")
# On régresse le "choc inexpliqué d'activité" sur le "choc inexpliqué de pauvreté"
modele_causal_pauvrete <- lm(residus_Y_pauvrete ~ residus_T_pauvrete)

# Affichage des résultats purs
summary(modele_causal_pauvrete)