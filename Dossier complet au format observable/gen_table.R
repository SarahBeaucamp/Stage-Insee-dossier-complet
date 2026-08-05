library(arrow)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Chargement des données
df_complet <- read_parquet("https://minio.lab.sspcloud.fr/sarahbeaucamp/dossier_complet.parquet")
df_geo <- read_parquet("https://minio.lab.sspcloud.fr/sarahbeaucamp/communes_france_2023.parquet")

# 2. Filtrage
df_pop_sup <- df_complet %>%
  filter(ID_TAB == "POP_T1", GEO_OBJECT_LABEL == "Commune") %>%
  filter(TAB_MEASURE %in% c("POP", "SUP")) %>%
  mutate(
    Mesure = if_else(TAB_MEASURE == "POP", "population", "superficie"),
    valeur = as.numeric(str_replace(as.character(OBS_VALUE), ",", "."))
  ) %>%
  select(GEO, GEO_LABEL, TIME_PERIOD, Mesure, valeur)

# 3. Le Pivot 
df_pivot <- df_pop_sup %>%
  pivot_wider(names_from = Mesure, values_from = valeur, values_fn = max) %>%
  filter(!is.na(population), !is.na(superficie), superficie > 0) %>%
  mutate(
    superficie_km2 = superficie / 100,           
    densite = population / superficie_km2        
  ) %>%
  select(-superficie) %>%                        
  rename(superficie = superficie_km2)         

# 4. Nettoyage et récupération des coordonnées géographiques
df_geo_clean <- df_geo %>%
  select(code_insee, latitude_centre, longitude_centre) %>%
  mutate(
    lat = as.numeric(str_replace(as.character(latitude_centre), ",", ".")),
    lon = as.numeric(str_replace(as.character(longitude_centre), ",", "."))
  ) %>%
  filter(!is.na(lat), !is.na(lon))

# 5. Jointure finale et export
df_final <- df_pivot %>%
  inner_join(df_geo_clean, by = c("GEO" = "code_insee")) %>%
  select(GEO, GEO_LABEL, TIME_PERIOD, lat, lon, population, superficie, densite)

write_parquet(df_final, "densite_communes_km2.parquet")