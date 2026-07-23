# --- VERROUILLAGE DE LA BASE DÉFINITIVE (Seuil = 430) ---

# 1. Application du filtre final
base_definitive <- base_sans_na %>%
  filter(Population >= 430) %>%
  select(-Population) # On jette la clé pour que l'algorithme ne triche pas

# 2. Découpage final de sécurité (80% / 20%)
set.seed(42)
split_principal <- initial_split(base_definitive, prop = 0.80)
base_train_val  <- training(split_principal)
base_test_finale <- testing(split_principal) # Base de test pure (à garder pour la fin)

# 3. Découpage interne d'apprentissage Train / Validation (75% / 25%)
set.seed(42)
split_interne <- initial_split(base_train_val, prop = 0.75)
base_train <- training(split_interne)
base_val   <- testing(split_interne)

print(paste("Nombre de communes pour l'entraînement définitif :", nrow(base_train)))