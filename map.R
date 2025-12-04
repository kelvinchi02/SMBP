library(leaflet)
library(shiny)
library(shinyWidgets)
library(bsicons)

# -------------------------------------------------------------------------
# UI COMPONENT
# -------------------------------------------------------------------------
map_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .map-container { position: relative; height: calc(100vh - 140px); width: 100%; }
        .map-controls {
          position: absolute; top: 20px; right: 20px; z-index: 1000;
          background: rgba(255, 255, 255, 0.95); padding: 20px;
          border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.15);
          width: 280px; backdrop-filter: blur(5px);
        }
        .control-label { font-size: 0.8rem; font-weight: 700; color: #2c3e50; margin-bottom: 8px; letter-spacing: 0.5px; }
        .btn-refresh-map { width: 100%; background-color: #2c3e50; color: white; border: none; font-weight: 600; }
        .btn-refresh-map:hover { background-color: #34495e; }
      "))
    ),
    
    div(
      class = "page-wrapper",
      div(class = "page-header", div(class = "container", h2("Live Network Map"), back_button())),
      
      div(
        class = "map-container",
        # Map Output
        leafletOutput("mapPlotOut", height = "100%", width = "100%"),
        
        # Floating Controls
        div(
          class = "map-controls",
          
          # 1. Route Selector
          div(class = "control-label", "SELECT ROUTE"),
          pickerInput(
            inputId = "map_route_select",
            label = NULL,
            choices = if(exists("routes")) routes$route_id else c("Loading..."),
            options = list(size = 5),
            width = "100%"
          ),
          
          # 2. Refresh Button
          div(style = "margin-top: 15px;",
            actionButton("refresh_live_map", "Refresh Locations", icon = icon("sync"), class = "btn-refresh-map")
          ),
          
          # 3. Layer Transparency
          div(class = "control-label", style = "margin-top: 20px;", "STOP VISIBILITY"),
          sliderInput("whatMapalpha", NULL, min = 0, max = 1, value = 0.8, step = 0.1, ticks = FALSE)
        )
      )
    )
  )
}

# -------------------------------------------------------------------------
# LOGIC COMPONENT
# -------------------------------------------------------------------------

makemap <- function(sf_data, alpha_val, selected_route) {
  
  # 1. Safety Check
  if (is.null(sf_data) || nrow(sf_data) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -74.006, lat = 40.7128, zoom = 12))
  }

  # 2. Filter data by the selected route
  if (!is.null(selected_route) && selected_route != "") {
    # Use standard subsetting for SF objects
    map_data <- sf_data[sf_data$route_id == selected_route, ]
  } else {
    map_data <- sf_data
  }
  
  # 3. Extract columns DIRECTLY (prevents "object not found" error)
  stop_names <- map_data$stop_name
  route_ids  <- map_data$route_id
  
  # Handle color
  fill_cols <- if("route_color" %in% names(map_data)) map_data$route_color else rep("#3498db", nrow(map_data))

  # 4. Create Map
  leaflet(data = map_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      radius = 7,
      stroke = TRUE, color = "white", weight = 1,
      fillOpacity = alpha_val,
      fillColor = fill_cols, 
      popup = paste0("<b>Stop:</b> ", stop_names, "<br><b>Route:</b> ", route_ids)
    )
}