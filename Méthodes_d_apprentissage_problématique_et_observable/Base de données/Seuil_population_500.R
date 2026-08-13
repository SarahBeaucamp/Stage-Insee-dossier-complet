# ==============================================================================
# ÉTAPE 1 : JUSTIFICATION STATISTIQUE DU SEUIL DE POPULATION (500 HABITANTS)
# ==============================================================================

# Chargement des librairies nécessaires
library(ggplot2)
library(dplyr)
library(scales)

print("--- 1. LE GRAPHIQUE EN ENTONNOIR (FUNNEL PLOT) ---")

# Génération du graphique de dispersion
graphique_entonnoir <- ggplot(base_prete_rf, aes(x = Population, y = Y_GAP_ACT_GLOBAL)) +
  geom_point(alpha = 0.2, color = "#2c3e50", size = 1) +
  scale_x_log10(labels = label_comma()) + # Échelle log pour étirer l'axe des petites communes
  geom_vline(xintercept = 500, color = "#e74c3c", linetype = "dashed", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Dispersion de l'écart d'activité selon la taille de la commune",
    subtitle = "La ligne rouge marque le seuil de basculement fixé à 500 habitants",
    x = "Population de la commune (échelle logarithmique)",
    y = "Écart d'activité (Hommes - Femmes)"
  ) +
  annotate("text", x = 150, y = max(base_prete_rf$Y_GAP_ACT_GLOBAL, na.rm = TRUE) * 0.9, 
           label = "Forte volatilité\n(Bruit statistique)", color = "#e74c3c", fontface = "bold") +
  annotate("text", x = 1500, y = max(base_prete_rf$Y_GAP_ACT_GLOBAL, na.rm = TRUE) * 0.9, 
           label = "Variance stabilisée\n(Signal fiable)", color = "#27ae60", fontface = "bold")

# Affichage du graphique dans la fenêtre Plots
print(graphique_entonnoir)


print("--- 2. LE TABLEAU DE VOLATILITÉ MATHÉMATIQUE ---")

# Calcul de l'écart-type et des NAs par tranches de population
tableau_volatilite <- base_prete_rf %>%
  mutate(Tranche_Pop = cut(Population, 
                           breaks = c(0, 150, 300, 500, 1000, 5000, Inf),
                           labels = c("< 150", "150-300", "300-500", "500-1000", "1000-5000", "> 5000"))) %>%
  group_by(Tranche_Pop) %>%
  summarise(
    Nb_Communes = n(),
    Ecart_Type_Cible = round(sd(Y_GAP_ACT_GLOBAL, na.rm = TRUE), 4),
    Taux_NA_Cible = round(mean(is.na(Y_GAP_ACT_GLOBAL)) * 100, 2)
  )

print(as.data.frame(tableau_volatilite))


print("--- 3. IMPACT DU FILTRE : PERTE DE COMMUNES ET D'HABITANTS ---")

# Calcul de ce que le seuil de 500 habitants nous fait conserver et perdre
impact_filtre <- base_prete_rf %>%
  mutate(Statut = ifelse(Population >= 500, "1. Conservées (>= 500)", "2. Exclues (< 500)")) %>%
  group_by(Statut) %>%
  summarise(
    Nb_Communes = n(),
    Population_Totale = sum(Population, na.rm = TRUE)
  ) %>%
  mutate(
    Pct_Communes = round((Nb_Communes / sum(Nb_Communes)) * 100, 2),
    Pct_Population = round((Population_Totale / sum(Population_Totale)) * 100, 2)
  ) %>%
  arrange(Statut)

print(as.data.frame(impact_filtre))