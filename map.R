source("styles.R")

# ---------------------------------------------------------
# NOTE: This file now depends on 'api_utils.R'
# You must create that file (see Step 2) to avoid errors.
# ---------------------------------------------------------
if (file.exists("api_utils.R")) {
  source("api_utils.R")
}

# Map page UI
# map.R

map_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .map-container { position: relative; height: calc(100vh - 140px); width: 100%; }
        .map-controls {
          position: absolute; top: 20px; right: 20px; z-index: 1000;
          background: rgba(255, 255, 255, 0.95); padding: 15px;
          border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          width: 250px; backdrop-filter: blur(5px);
        }
        .control-label { font-size: 0.8rem; font-weight: 600; color: #6c757d; margin-bottom: 5px; }
      "))
    ),
    
    div(
      class = "page-wrapper",
      div(class = "page-header", div(class = "container", h2("Live Network Map"), back_button())),
      
      div(
        class = "map-container",
        # The Map Output
        leafletOutput("mapPlotOut", height = "100%", width = "100%"),
        
        # Floating Controls
        div(
          class = "map-controls",
          
          # 1. Route Selector (Triggers update automatically)
          div(class = "control-label", "SELECT ROUTE"),
          pickerInput(
            inputId = "map_route_select",
            label = NULL,
            choices = if(exists("routes")) routes$route_id else c("Loading..."),
            options = list(size = 5),
            width = "100%"
          ),
          
          # 2. Layer Transparency
          div(class = "control-label", "STOP VISIBILITY"),
          sliderInput("whatMapalpha", NULL, min = 0, max = 1, value = 0.7, step = 0.1, ticks = FALSE)
        )
      )
    )
  )
}

# Helper function for map creation (Unchanged from your logic)
create_route_map <- function(sf_stops, alpha_var = "occupancy_rate") {
  routes <- unique(sf_stops$route_id)
  m <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)
  
  if(alpha_var == "occupancy_rate") {
    min_val <- min(sf_stops$occupancy_rate); max_val <- max(sf_stops$occupancy_rate); size_label <- "Occupancy Rate"
  } else {
    min_val <- min(sf_stops$delay_min); max_val <- max(sf_stops$delay_min); size_label <- "Delay (minutes)"
  }
  
  for (route in routes) {
    route_data <- sf_stops %>% filter(route_id == route)
    route_coords <- do.call(rbind, lapply(route_data$geometry, function(x) st_coordinates(x)))
    route_line <- st_linestring(route_coords)
    route_sf <- st_sf(geometry = st_sfc(route_line, crs = st_crs(sf_stops)))
    
    m <- m %>% 
      addPolylines(data = route_sf, color = route_data$route_color[1], weight = 4, opacity = 0.7, group = route_data$route_name[1]) %>%
      addCircleMarkers(
        data = route_data,
        radius = ~ 6 + (get(alpha_var) - min_val) / (max_val - min_val) * 8,
        stroke = TRUE, color = "white", weight = 1.5,
        fillColor = route_data$route_color[1], fillOpacity = 0.85,
        popup = ~paste0("<strong>Stop:</strong> ", stop_id, "<br><strong>Delay:</strong> ", round(delay_min*60), "s"),
        group = route_data$route_name[1]
      )
  }
  return(m)
}

makemap <- function(data_sf, alpha_var = "occupancy_rate") {
  create_route_map(data_sf, alpha_var)
}