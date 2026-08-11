# ==============================================================================
# ETAPE 10 : DÉCOMPOSITION D'OAXACA-BLINDER
# ==============================================================================

install.packages("oaxaca")
library(oaxaca)
library(dplyr)

# 1. Création d'une variable binaire pour diviser tes communes en 2 groupes
# On sépare les communes selon la médiane du taux de pauvreté
# Groupe 1 = Communes plus pauvres que la médiane (1)
# Groupe 0 = Communes moins pauvres que la médiane (0)
mediane_pauvrete <- median(base_train$Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie, na.rm = TRUE)

base_train_oax <- base_train %>%
  mutate(groupe_pauvrete = ifelse(Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie > mediane_pauvrete, 1, 0))

# 2. Nouvelle formule avec le séparateur "|" pour le groupe (groupe_pauvrete)
formule_oaxaca_corrigee <- Y_GAP_ACT_GLOBAL ~ Niveau.de.vie.médian..en.euros. + 
  Population...De.25.à.54.ans.x.Ouvriers + 
  Nombre.de.famille + 
  Population...15.ans.ou.plus.x.Célibataire | groupe_pauvrete

# 3. Lancement de la décomposition d'Oaxaca-Blinder (sans l'argument group.variable)
resultat_oaxaca <- oaxaca(
  formula = formule_oaxaca_corrigee,
  data = base_train_oax
)

# 4. Affichage du résumé des résultats
print(summary(resultat_oaxaca))

# 5. Graphique de synthèse de la décomposition
plot(resultat_oaxaca)

# ==============================================================================
# OAXACA-BLINDER : TEST SUR LES FAMILLES NOMBREUSES
# ==============================================================================

# 1. On trouve la médiane des familles nombreuses
var_groupe_nom <- "Nombre.de.famille...4.enfants.ou.plus.de.moins.de.24.ans"
mediane_familles <- median(base_train[[var_groupe_nom]], na.rm = TRUE)

# 2. On crée le groupe binaire (1 = Forte concentration de familles nombreuses, 0 = Faible)
base_train_oax_fam <- base_train %>%
  mutate(groupe_familles_nombreuses = ifelse(base_train[[var_groupe_nom]] > mediane_familles, 1, 0))

# 3. On met à jour la formule (On inclut la pauvreté comme variable explicative cette fois)
formule_oax_fam <- Y_GAP_ACT_GLOBAL ~ Niveau.de.vie.médian..en.euros. + 
  Population...De.25.à.54.ans.x.Ouvriers + 
  Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie + 
  Population...15.ans.ou.plus.x.Célibataire | groupe_familles_nombreuses

# 4. Lancement de la décomposition
res_oax_fam <- oaxaca(formula = formule_oax_fam, data = base_train_oax_fam)

# 5. Résultats
print(summary(res_oax_fam))
plot(res_oax_fam)

# ==============================================================================
# OAXACA-BLINDER : TEST SUR LE SALAIRE "MONSIEUR GAGNE-PAIN" (25-39 ans)
# ==============================================================================

# 1. On trouve la médiane des salaires des hommes de 25-39 ans
var_groupe_salaire <- "Salaire.net.EQTP.mensuel.moyen...Homme.x.De.25.à.39.ans"
mediane_salaire_H <- median(base_train[[var_groupe_salaire]], na.rm = TRUE)

# 2. Création du groupe binaire 
# (1 = Communes où les jeunes hommes gagnent PLUS que la médiane, 0 = MOINS que la médiane)
base_train_oax_sal <- base_train %>%
  mutate(groupe_salaire_hommes = ifelse(base_train[[var_groupe_salaire]] > mediane_salaire_H, 1, 0))

# 3. Mise à jour de la formule 
# On remet les familles nombreuses et la pauvreté dans les variables explicatives
formule_oax_sal <- Y_GAP_ACT_GLOBAL ~ Niveau.de.vie.médian..en.euros. + 
  Population...De.25.à.54.ans.x.Ouvriers + 
  Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie + 
  Nombre.de.famille...4.enfants.ou.plus.de.moins.de.24.ans | groupe_salaire_hommes

# 4. Lancement de la décomposition
res_oax_sal <- oaxaca(formula = formule_oax_sal, data = base_train_oax_sal)

# 5. Résultats et graphiques
print(summary(res_oax_sal))
plot(res_oax_sal)

# ==============================================================================
# OAXACA-BLINDER : TEST SUR LA STRUCTURE DU MARCHÉ DU TRAVAIL (OUVRIERS)
# ==============================================================================

# 1. On trouve la médiane de la proportion d'ouvriers (25-54 ans)
var_groupe_ouvriers <- "Population...De.25.à.54.ans.x.Ouvriers"
mediane_ouvriers <- median(base_train[[var_groupe_ouvriers]], na.rm = TRUE)

# 2. Création du groupe binaire 
# (1 = Communes très ouvrières, 0 = Communes moins ouvrières/plus tertiaires)
base_train_oax_ouv <- base_train %>%
  mutate(groupe_ouvriers = ifelse(base_train[[var_groupe_ouvriers]] > mediane_ouvriers, 1, 0))

# 3. Mise à jour de la formule 
# On réintègre le Taux de pauvreté, le Niveau de vie, les Familles nombreuses et le Salaire masculin
formule_oax_ouv <- Y_GAP_ACT_GLOBAL ~ Niveau.de.vie.médian..en.euros. + 
  Taux.de.pauvreté..en....au.seuil.de.60...de.la.médiane.du.niveau.de.vie + 
  Nombre.de.famille...4.enfants.ou.plus.de.moins.de.24.ans +
  Salaire.net.EQTP.mensuel.moyen...Homme.x.De.25.à.39.ans | groupe_ouvriers

# 4. Lancement de la décomposition
res_oax_ouv <- oaxaca(formula = formule_oax_ouv, data = base_train_oax_ouv)

# 5. Résultats et graphiques
print(summary(res_oax_ouv))
plot(res_oax_ouv)