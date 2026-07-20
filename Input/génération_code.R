# On charge le nécessaire à l'affichage (en dehors des données), car les requêtes sont trop longues à la volée
# Stocké en CSV
# TODO à lancer à chaque mise à jour du CI?

url_dossier_complet <- "https://minio.lab.sspcloud.fr/sarahbeaucamp/dossier_complet.parquet"
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Liste des territoires, avec un ordre d'affichage privilégié
geo <- DBI::dbGetQuery(con, glue::glue(
  "SELECT DISTINCT GEO_OBJECT, GEO_OBJECT_LABEL, GEO, GEO_LABEL, ",
  "CASE GEO_OBJECT
    WHEN 'COM' THEN 1
    WHEN 'EPCI' THEN 2
    WHEN 'DEP' THEN 3
    WHEN 'REG' THEN 4
    WHEN 'FRANCE' THEN 5
    ELSE 6
  END AS GEO_OBJECT_ORDER ",
  "FROM '{url_dossier_complet}' ",
  "ORDER BY GEO_OBJECT_ORDER, GEO "
))

# Liste des tableaux
tableaux <- DBI::dbGetQuery(con, glue::glue(
  "SELECT DISTINCT ID_TAB, ID_TAB_LABEL ",
  "FROM '{url_dossier_complet}' ",
  "ORDER BY ID_TAB "
))

# TODO mieux gérer le chemin d'écriture
data.table::fwrite(geo, "Input/geo.csv")
data.table::fwrite(tableaux, "Input/tableaux.csv")
