# ==============================================================================
# PRÉPARATION DES FONDS DE CARTE (SANS LES DONNÉES)
# ==============================================================================
library(sf)
library(dplyr)

print("--- 1. TÉLÉCHARGEMENT DES GPKG ---")
system("aws s3 cp s3://sarahbeaucamp/fm_commune.gpkg fm_commune.gpkg")
system("aws s3 cp s3://sarahbeaucamp/fm_dep.gpkg fm_dep.gpkg")

carte_communes <- st_read("fm_commune.gpkg")
carte_deps <- st_read("fm_dep.gpkg")

print("--- 2. NETTOYAGE ET ALLÈGEMENT ---")
# On ne garde strictement que le code et la géométrie pour avoir un fichier poids plume
communes_light <- carte_communes %>%
  select(code, geometry) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

deps_light <- carte_deps %>%
  select(code, geometry) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

print("--- 3. EXPORT S3 ---")
st_write(communes_light, "geo_communes_obs.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(deps_light, "geo_deps_obs.geojson", driver = "GeoJSON", delete_dsn = TRUE)

system("aws s3 cp geo_communes_obs.geojson s3://sarahbeaucamp/geo_communes_obs.geojson")
system("aws s3 cp geo_deps_obs.geojson s3://sarahbeaucamp/geo_deps_obs.geojson")

print("✅ Fonds de carte prêts !")

