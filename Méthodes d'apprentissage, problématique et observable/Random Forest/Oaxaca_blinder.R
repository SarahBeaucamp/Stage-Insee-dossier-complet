# ==============================================================================
# ETAPE 10 : DÉCOMPOSITION D'OAXACA-BLINDER
# ==============================================================================

# Installation et chargement du package (si ce n'est pas déjà fait)
# install.packages("oaxaca")
library(oaxaca)
library(dplyr)

# 1. Création d'une variable binaire pour diviser tes communes en 2 groupes
# Par exemple : On sépare les communes selon la médiane du taux de pauvreté
# Groupe 1 = Communes plus pauvres que la médiane (1)
# Groupe 0 = Communes moins pauvres que la médiane (0)
mediane_pauvrete <- median(base_train$Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie, na.rm = TRUE)

base_train_oax <- base_train %>%
  mutate(groupe_pauvrete = ifelse(Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie > mediane_pauvrete, 1, 0))

# 2. Sélection des variables explicatives clés pour le modèle 
# (On évite de mettre les 317 variables sinon le modèle va exploser, on prend les plus importantes identifiées par le Lasso/RF)
formule_oaxaca <- Y_GAP_ACT_GLOBAL ~ Niveau.de.vie.médian..en.euros. + 
  Population...De.25.à.54.ans.x.Ouvriers + 
  Nombre.de.famille + 
  Population...15.ans.ou.plus.x.Célibataire

# 3. Lancement de la décomposition d'Oaxaca-Blinder
resultat_oaxaca <- oaxaca(
  formula = formule_oaxaca,
  data = base_train_oax,
  group.variable = "groupe_pauvrete"
)

# 4. Affichage du résumé des résultats
print(summary(resultat_oaxaca))

# 5. Graphique de synthèse de la décomposition
plot(resultat_oaxaca)