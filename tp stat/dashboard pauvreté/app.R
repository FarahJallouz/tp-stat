# =============================================================================
# Application Shiny : Carte Interactive de la Pauvreté en Tunisie
# =============================================================================
# Affiche une carte choroplèthe du taux de pauvreté au niveau des délégations
# avec filtrage par district et gouvernorat.
# =============================================================================

library(shiny)
library(sf)
library(leaflet)
library(readxl)
library(dplyr)
library(bslib)
library(bsicons)

# =============================================================================
# PRÉPARATION DES DONNÉES
# =============================================================================

# Chargement du shapefile
tunisia_sf <- st_read("Tunisie_snuts4.shp", quiet = TRUE)

# Assigner CRS (UTM 32N) et reprojeter en WGS84
st_crs(tunisia_sf) <- 32632
tunisia_sf <- st_transform(tunisia_sf, 4326)

# Corriger les géométries invalides (résout l'erreur "Loop 0 is not valid")
tunisia_sf <- st_make_valid(tunisia_sf)

# Chargement des données de pauvreté
poverty_data <- read_excel("pauvreté.xlsx")
names(poverty_data) <- c("Gouvernorat", "Taux_pauvrete", "District")
poverty_data <- poverty_data %>%
  mutate(Gouvernorat = toupper(trimws(Gouvernorat)), District = as.integer(District))
# Mapping SNUTS3 → Gouvernorats
gov_map <- data.frame(
  id_snuts3 = c(
    "TS111", "TS112", "TS113", "TS114", "TS115", "TS116", "TS117",
    "TS121", "TS122", "TS123", "TS124",
    "TS211", "TS212", "TS213", "TS214",
    "TS221", "TS222", "TS223",
    "TS311", "TS312", "TS313",
    "TS321", "TS322", "TS323"
  ),
  Gouvernorat = c(
    "TUNIS", "ARIANA", "BEN AROUS", "MANOUBA", "NABEUL", "ZAGHOUAN", "BIZERTE",
    "BEJA", "JENDOUBA", "LE KEF", "SILIANA",
    "SOUSSE", "MONASTIR", "MAHDIA", "SFAX",
    "KAIROUAN", "KASSERINE", "SIDI BOUZID",
    "GABES", "MEDENINE", "TATAOUINE",
    "GAFSA", "TOZEUR", "KEBILI"
  ),
  stringsAsFactors = FALSE
)

# Jointures
tunisia_sf <- tunisia_sf %>%
  left_join(gov_map, by = "id_snuts3") %>%
  left_join(poverty_data, by = "Gouvernorat") %>%
  filter(!is.na(Taux_pauvrete))

all_governorates <- sort(unique(poverty_data$Gouvernorat))

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

ui <- page_sidebar(
  title = "Carte de la Pauvreté en Tunisie",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Inter"),
    heading_font = font_google("Lexend Deca")
  ),
  sidebar = sidebar(
    title = "Filtres",
    selectInput("district", "District :", c(
      "Tous" = "Tous",
      "District 1" = "1", "District 2" = "2", "District 3" = "3",
      "District 4" = "4", "District 5" = "5"
    )),
    selectInput("gouvernorat", "Gouvernorat :", c("Tous" = "Tous"))
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Taux Moyen",
      value = textOutput("avg_poverty"),
      showcase = bs_icon("bar-chart-fill"),
      theme = "primary"
    ),
    value_box(
      title = "Taux Maximum",
      value = textOutput("max_poverty"),
      showcase = bs_icon("graph-up-arrow"),
      theme = "danger"
    ),
    value_box(
      title = "Taux Minimum",
      value = textOutput("min_poverty"),
      showcase = bs_icon("graph-down-arrow"),
      theme = "info"
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Carte Interactive"),
    leafletOutput("map", height = "600px")
  )
)

# =============================================================================
# SERVEUR
# =============================================================================

server <- function(input, output, session) {
  observeEvent(input$district, {
    if (input$district == "Tous") {
      choices <- c("Tous" = "Tous", setNames(all_governorates, all_governorates))
    } else {
      filtered <- poverty_data %>% filter(District == as.integer(input$district))
      choices <- c("Tous" = "Tous", setNames(sort(filtered$Gouvernorat), sort(filtered$Gouvernorat)))
    }
    updateSelectInput(session, "gouvernorat", choices = choices)
  })

  filtered_sf <- reactive({
    data <- tunisia_sf
    # Ne filtrer que si un district spécifique est sélectionné
    if (!is.null(input$district) && input$district != "Tous") {
      data <- data %>% filter(District == as.integer(input$district))
    }
    # Ne filtrer que si un gouvernorat spécifique est sélectionné
    if (!is.null(input$gouvernorat) && input$gouvernorat != "Tous") {
      data <- data %>% filter(Gouvernorat == input$gouvernorat)
    }
    data
  })

  stats_data <- reactive({
    if (input$gouvernorat != "Tous") {
      poverty_data %>% filter(Gouvernorat == input$gouvernorat)
    } else if (input$district != "Tous") {
      poverty_data %>% filter(District == as.integer(input$district))
    } else {
      poverty_data
    }
  })

  output$avg_poverty <- renderText({
    paste0(round(mean(stats_data()$Taux_pauvrete, na.rm = TRUE), 1), "%")
  })
  output$max_poverty <- renderText({
    paste0(round(max(stats_data()$Taux_pauvrete, na.rm = TRUE), 1), "%")
  })
  output$min_poverty <- renderText({
    paste0(round(min(stats_data()$Taux_pauvrete, na.rm = TRUE), 1), "%")
  })

  output$map <- renderLeaflet({
    data <- filtered_sf()
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(9.5, 34, 6))
    }

    # Palette exacte comme l'image de référence
    pal <- colorBin(
      palette = c(
        "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
        "#fddbc7", "#f4a582", "#d6604d", "#b2182b"
      ),
      domain = c(0, 55),
      bins = c(0, 5, 8.5, 12, 16.7, 20.7, 25.9, 33.4, 55)
    )

    bbox <- st_bbox(data)

    # Infobulles au survol
    labels <- sprintf(
      "<strong>%s</strong><br/>Gouvernorat: %s<br/><span style='color:#e74c3c;font-weight:bold;'>%.1f%%</span>",
      data$id, data$Gouvernorat, data$Taux_pauvrete
    ) %>% lapply(htmltools::HTML)

    # Calculer les centroïdes des gouvernorats pour les étiquettes (sans doublons)
    gov_centroids <- data %>%
      group_by(Gouvernorat) %>%
      summarise(geometry = st_union(geometry), .groups = "drop") %>%
      distinct(Gouvernorat, .keep_all = TRUE)

    # Calculer les coordonnées des centroïdes
    centroids_coords <- st_centroid(gov_centroids$geometry)
    gov_centroids$lng <- st_coordinates(centroids_coords)[, 1]
    gov_centroids$lat <- st_coordinates(centroids_coords)[, 2]

    # Carte avec fond blanc
    leaflet(data, options = leafletOptions(zoomControl = TRUE)) %>%
      addPolygons(
        fillColor = ~ pal(Taux_pauvrete),
        weight = 0.8, color = "#333333", fillOpacity = 0.9,
        highlightOptions = highlightOptions(weight = 2, color = "#000", fillOpacity = 1),
        label = labels
      ) %>%
      # Ajouter les noms des gouvernorats
      addLabelOnlyMarkers(
        data = gov_centroids,
        lng = ~lng, lat = ~lat,
        label = ~Gouvernorat,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "font-size" = "10px",
            "font-weight" = "bold",
            "color" = "#000000",
            "text-shadow" = "1px 1px 2px white, -1px -1px 2px white, 1px -1px 2px white, -1px 1px 2px white"
          )
        )
      ) %>%
      addLegend("bottomleft",
        pal = pal, values = ~Taux_pauvrete,
        title = "Taux de pauvreté (%)", opacity = 1
      ) %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })
}

shinyApp(ui, server)
