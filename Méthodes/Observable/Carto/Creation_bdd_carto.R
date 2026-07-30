library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
# install.packages("plotly") # À décommenter si besoin
library(plotly)
# install.packages("patchwork") # À décommenter si besoin
library(patchwork)

# --- Connexion DuckDB ---
con <- dbConnect(duckdb(), dbdir = "base.duckdb")
dbExecute(con, "INSTALL httpfs; LOAD httpfs; INSTALL aws; LOAD aws;")
dbExecute(con, "CREATE OR REPLACE SECRET (
  TYPE S3,
  PROVIDER CREDENTIAL_CHAIN,
  ENDPOINT 'minio.lab.sspcloud.fr',
  URL_STYLE 'path'
);")

# --- 1. Chargement et filtrage du fichier communes depuis ton S3 ---
chemin_communes <- "s3://sarahbeaucamp/communes_france_2023.parquet"
communes_raw <- tbl(con, paste0("read_parquet('", chemin_communes, "')"))

# On garde uniquement ce qui nous intéresse
communes_geo <- communes_raw %>%
  select(
    GEO = code_insee,
    lat = latitude_centre,
    lon = longitude_centre,
    population = population
  ) %>%
  # On exclut les DROM-COM pour la carte métropolitaine
  filter(!substr(GEO, 1, 2) %in% c("97", "98")) %>%
  collect()

# --- 2. Chargement de tes prédictions XGBoost ---
chemin_xgb <- "s3://sarahbeaucamp/predictions_xgboost_2023.parquet"
preds_xgb <- tbl(con, paste0("read_parquet('", chemin_xgb, "')")) %>%
  collect()

# --- 3. Jointure des deux tables ---
df_carte <- preds_xgb %>%
  inner_join(communes_geo, by = "GEO") %>%
  # On exclut les valeurs manquantes de coordonnées
  filter(!is.na(lat) & !is.na(lon)) %>%
  # AJOUT : Filtre de robustesse pour éviter le bruit et alléger la carte
  filter(population >= 500)

# --- Définition des limites de l'échelle de couleur commune ---
min_val <- min(c(df_carte$Y_OBSERVE, df_carte$Y_PREDIT_XGB), na.rm = TRUE)
max_val <- max(c(df_carte$Y_OBSERVE, df_carte$Y_PREDIT_XGB), na.rm = TRUE)

# --- Carte 1 : Valeurs Réelles ---
p1 <- ggplot(df_carte, aes(x = lon, y = lat, 
                           size = population, 
                           color = Y_OBSERVE,
                           text = paste0("<b>", GEO_LABEL, "</b><br>",
                                         "Réel : ", round(Y_OBSERVE * 100, 1), "%<br>",
                                         "Pop : ", population))) +
  geom_point(alpha = 0.6, stroke = 0) +
  scale_size_continuous(range = c(0.2, 3), guide = "none") + 
  scale_color_viridis_c(
    option = "plasma", 
    limits = c(min_val, max_val), 
    name = "Taux"
  ) +
  coord_quickmap() + 
  theme_void() +
  labs(title = "Valeur Réelle Observée (2023)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- Carte 2 : Prédictions XGBoost ---
p2 <- ggplot(df_carte, aes(x = lon, y = lat, 
                           size = population, 
                           color = Y_PREDIT_XGB,
                           text = paste0("<b>", GEO_LABEL, "</b><br>",
                                         "Prédit : ", round(Y_PREDIT_XGB * 100, 1), "%<br>",
                                         "Pop : ", population))) +
  geom_point(alpha = 0.6, stroke = 0) +
  scale_size_continuous(range = c(0.2, 3), guide = "none") +
  scale_color_viridis_c(
    option = "plasma", 
    limits = c(min_val, max_val), 
    name = "Taux"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(title = "Prédiction XGBoost (2023)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- Assemblage et conversion en Plotly interactif ---
carte_interactive <- subplot(
  ggplotly(p1, tooltip = "text"),
  ggplotly(p2, tooltip = "text"),
  nrows = 1,
  shareX = TRUE,
  shareY = TRUE,
  titleX = FALSE,
  titleY = FALSE
) %>% 
  layout(title = "Comparaison de l'Écart d'Activité : Réel vs Prédit")

carte_interactive