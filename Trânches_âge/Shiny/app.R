library(DBI)
library(duckdb)
library(dplyr)
library(shiny)
library(ggplot2)
library(scales)

# ── Connexion S3 ──────────────────────────────────────────────────────────────
con <- dbConnect(duckdb(), dbdir = "base.duckdb")
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
dbExecute(con, "INSTALL aws;   LOAD aws;")
dbExecute(con, "CREATE OR REPLACE SECRET (
  TYPE S3, PROVIDER CREDENTIAL_CHAIN,
  ENDPOINT 'minio.lab.sspcloud.fr', URL_STYLE 'path'
);")

chemin_s3       <- "s3://sarahbeaucamp/dossier_complet.parquet"
dossier_complet <- tbl(con, paste0("read_parquet('", chemin_s3, "')"))

tranches_age <- c(
  "Population – Moins de 15 ans",
  "Population – De 15 à 24 ans ",
  "Population – De 25 à 39 ans",
  "Population – De 40 à 54 ans",
  "Population – De 55 à 64 ans",
  "Population – De 65 à 79 ans",
  "Population – 80 ans ou plus"
)

# ── On récupère juste la liste des villes (léger) ────────────────────────────
villes <- dossier_complet %>%
  filter(TIME_PERIOD == "2022", GEO_OBJECT_LABEL == "Commune") %>%
  distinct(GEO_LABEL) %>%
  collect() %>%
  arrange(GEO_LABEL) %>%
  pull(GEO_LABEL)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Répartition de la population par tranche d'âge — INSEE 2022"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ville", "Ville", choices = villes)
    ),
    mainPanel(
      plotOutput("pie")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output) {
  
  donnees <- reactive({
    # On ne collecte que la ville sélectionnée à chaque clic
    pop_totale <- dossier_complet %>%
      filter(TIME_PERIOD == "2022", GEO_OBJECT_LABEL == "Commune",
             GEO_LABEL == input$ville,
             TAB_MEASURE_LABEL == "Population") %>%
      summarise(pop_totale = sum(OBS_VALUE, na.rm = TRUE)) %>%
      collect() %>%
      pull(pop_totale)
    
    dossier_complet %>%
      filter(TIME_PERIOD == "2022", GEO_OBJECT_LABEL == "Commune",
             GEO_LABEL == input$ville,
             TAB_MEASURE_LABEL %in% tranches_age) %>%
      group_by(TAB_MEASURE_LABEL) %>%
      summarise(total = sum(OBS_VALUE, na.rm = TRUE)) %>%
      collect() %>%
      mutate(
        part              = total / sum(total),
        population_exacte = round(part * pop_totale),
        TAB_MEASURE_LABEL = factor(TAB_MEASURE_LABEL, levels = tranches_age)
      )
  })
  
  output$pie <- renderPlot({
    d <- donnees()
    ggplot(d, aes(x = "", y = population_exacte, fill = TAB_MEASURE_LABEL)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_viridis_d(option = "plasma", name = "Tranche d'âge") +
      geom_text(aes(label = percent(part, accuracy = 0.1)),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 3.5) +
      labs(title    = paste("Structure par âge —", input$ville),
           subtitle = "Source : INSEE 2022",
           x = NULL, y = NULL) +
      theme_void() +
      theme(plot.title    = element_text(face = "bold",   size = 14, hjust = 0.5),
            plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5),
            legend.position = "right")
  })
}

shinyApp(ui = ui, server = server)