# Charger le package
install.packages("sf")
library(sf)

#importer fond de carte dep
dep <- st_read("U:/ODL/PSAR_Melodi/Stage Sarah/fond_carte/fm_dep.gpkg")

#afficher les départements
plot(dep$geometry)

#importer fond de carte commune 2025
commune <- st_read("fm_commune.gpkg")

#afficher les communes 2025
plot(commune$geometry)
