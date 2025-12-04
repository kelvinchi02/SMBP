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
          width: 200px; backdrop-filter: blur(5px);
        }
        .control-label { font-size: 0.8rem; font-weight: 600; color: #6c757d; margin-bottom: 5px; }
      "))
    ),
    
    div(
      class = "page-wrapper",
      div(class = "page-header", div(class = "container", h2("Live Network Map"), back_button())),
      
      div(
        class = "map-container",
        # Map Output
        leafletOutput("mapPlotOut", height = "100%", width = "100%"),
        
        # Floating Control (Only Visibility, no Route Selection)
        div(
          class = "map-controls",
          div(class = "control-label", "STOP VISIBILITY"),
          sliderInput("whatMapalpha", NULL, min = 0, max = 1, value = 0.7, step = 0.1, ticks = FALSE)
        )
      )
    )
  )
}

# -------------------------------------------------------------------------
# LOGIC COMPONENT (Crash Fixed)
# -------------------------------------------------------------------------

makemap <- function(sf_data, alpha_val) {
  
  # 1. Safety Check
  if (is.null(sf_data) || nrow(sf_data) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -74.006, lat = 40.7128, zoom = 12))
  }

  # 2. Extract columns DIRECTLY to avoid "object not found" error
  # This avoids the formula '~' scope issues
  stop_names <- sf_data$stop_name
  route_ids  <- sf_data$route_id
  
  # Handle color safely
  fill_cols <- if("route_color" %in% names(sf_data)) sf_data$route_color else rep("blue", nrow(sf_data))

  # 3. Create Map
  leaflet(data = sf_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      radius = 6,
      stroke = FALSE,
      fillOpacity = alpha_val,
      fillColor = fill_cols, 
      # FIX: Use vector directly, not formula
      popup = paste0(
        "<b>Stop:</b> ", stop_names, "<br>",
        "<b>Route:</b> ", route_ids
      )
    )
}