source("styles.R")

# ---------------------------------------------------------
# NOTE: This file now depends on 'api_utils.R'
# You must create that file (see Step 2) to avoid errors.
# ---------------------------------------------------------
if (file.exists("api_utils.R")) {
  source("api_utils.R")
}

# Map page UI
map_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        /* --- Real-time Section Styles --- */
        .realtime-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .realtime-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .realtime-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
        
        /* Vehicle Cards */
        .vehicles-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 1.5rem; margin-bottom: 2rem; }
        .vehicle-card { background: transparent; border: 1px solid #f0f0f0; padding: 1rem; border-radius: 4px; }
        .vehicle-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 0.75rem; padding-bottom: 0.75rem; border-bottom: 1px solid #f0f0f0; }
        .vehicle-id { font-size: 0.85rem; font-weight: 600; color: #2c3e50; }
        .vehicle-status { font-size: 0.75rem; padding: 0.25rem 0.5rem; border: 1px solid #dee2e6; border-radius: 4px; }
        .vehicle-status.on-time { color: #2ECC71; border-color: #2ECC71; background: rgba(46, 204, 113, 0.1); }
        .vehicle-status.delayed { color: #E74C3C; border-color: #E74C3C; background: rgba(231, 76, 60, 0.1); }
        
        /* AI Section */
        .ai-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .ai-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .ai-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; }
        .btn-ai { flex: 1; padding: 0.75rem 1.2rem; background: transparent; color: #495057; border: 1px solid #dee2e6; cursor: pointer; transition: all 0.2s; }
        .btn-ai:hover { border-color: #3498DB; color: #3498DB; }
        .btn-refresh { background: transparent; border: 1px solid #dee2e6; color: #495057; padding: 0.5rem 1rem; font-size: 0.85rem; }
        .btn-refresh:hover { border-color: #adb5bd; }
        
        .map-controls { margin-bottom: 2rem; }
        .map-container { border-bottom: 1px solid #f0f0f0; padding-bottom: 2rem; }
      "))
    ),
    
    div(
      class = "page-wrapper",
      
      div(
        class = "page-header",
        div(
          class = "container",
          h2("Map Dashboard"),
          back_button()
        )
      ),
      
      div(
        class = "page-content",
        
        # 1. NEW: Real-time Vehicle Tracker
        div(
          class = "page-section",
          div(class = "realtime-section",
            div(
              class = "realtime-header",
              div(class = "realtime-title", "Live Vehicle Locations"),
              div(
                style = "display: flex; gap: 1rem;",
                selectInput("live_route_select", NULL, choices = c("Route A", "Route B", "Route C"), selected = "Route A", width = "150px"),
                actionButton("refresh_live_map", "Refresh Data", class = "btn-refresh")
              )
            ),
            uiOutput("vehicles_display") # Server must render this
          )
        ),
        
        # 2. NEW: AI Insight Section
        div(
          class = "page-section",
          div(class = "ai-section",
            div(
              class = "ai-header",
              div(class = "ai-title", "AI Stop-Level Summary"),
              span()
            ),
            uiOutput("ai_stop_display"), # Server must render this
            div(
              class = "ai-buttons",
              actionButton("get_stop_insight", "Generate AI Insight", class = "btn-ai")
            )
          )
        ),
        
        # 3. EXISTING: Map Visualization
        div(
          class = "page-section",
          div(class = "map-controls",
            prettyRadioButtons(
              inputId = "whatMapalpha",
              label = "Size Circles By:",
              choices = c("Occupancy Rate" = "occupancy_rate", "Delay Minutes" = "delay_min"),
              icon = icon("check"),
              bigger = TRUE,
              status = "info",
              animation = "jelly",
              inline = TRUE
            )
          ),
          div(
            class = "map-container",
            leafletOutput("mapPlotOut", height = 750)
          )
        )
      ),
      
      div(
        class = "page-footer",
        div(class = "container", p("SmartTransit Analytics Dashboard © 2025"))
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

makemap <- function(alpha_var = "occupancy_rate") {
  create_route_map(sf_stops, alpha_var)
}