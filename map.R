library(leaflet)
library(shiny)
library(shinyWidgets)

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
          
          # 1. Route Selector
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

# -------------------------------------------------------------------------
# LOGIC COMPONENT (Fixed)
# -------------------------------------------------------------------------

makemap <- function(sf_data, alpha_val) {
  
  # 1. Safety Check: If data is missing or empty, return a blank map
  if (is.null(sf_data) || nrow(sf_data) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -74.006, lat = 40.7128, zoom = 12))
  }

  # 2. Check if route_color exists, default to blue if not
  # (Handles cases where pre.R might not have joined the color column)
  color_formula <- if("route_color" %in% names(sf_data)) {
    ~route_color 
  } else {
    "blue"
  }
  
  # 3. Create Map
  leaflet(data = sf_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      radius = 6,
      stroke = FALSE,
      fillOpacity = alpha_val,
      fillColor = color_formula, 
      popup = ~paste0(
        "<b>Stop:</b> ", stop_name, "<br>",
        "<b>Route:</b> ", route_id
      ),
      # Optional: Cluster options for better performance with many stops
      # clusterOptions = markerClusterOptions() 
    )
}