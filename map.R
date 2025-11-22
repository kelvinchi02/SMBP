source("styles.R")

# Map page UI
map_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .map-controls {
          margin-bottom: 2rem;
        }
        
        .map-container {
          border-bottom: 1px solid #f0f0f0;
          padding-bottom: 2rem;
        }
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
        div(
          class = "container",
          p("SmartTransit Analytics Dashboard © 2025")
        )
      )
    )
  )
}

# Create interactive route map with stops
create_route_map <- function(sf_stops, alpha_var = "occupancy_rate") {
  routes <- unique(sf_stops$route_id)
  
  m <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  if(alpha_var == "occupancy_rate") {
    min_val <- min(sf_stops$occupancy_rate)
    max_val <- max(sf_stops$occupancy_rate)
    size_label <- "Occupancy Rate"
    format_value <- function(x) paste0(round(x*100, 1), "%")
  } else {
    min_val <- min(sf_stops$delay_min)
    max_val <- max(sf_stops$delay_min)
    size_label <- "Delay (minutes)"
    format_value <- function(x) paste0(round(x*60, 0), "s")
  }
  
  for (route in routes) {
    route_data <- sf_stops %>% filter(route_id == route)
    route_color <- as.character(route_data$route_color[1])
    route_name <- as.character(route_data$route_name[1])
    
    route_coords <- do.call(rbind, lapply(route_data$geometry, function(x) st_coordinates(x)))
    route_line <- st_linestring(route_coords)
    route_sf <- st_sf(geometry = st_sfc(route_line, crs = st_crs(sf_stops)))
    
    m <- m %>% 
      addPolylines(
        data = route_sf,
        color = route_color,
        weight = 4,
        opacity = 0.7,
        group = route_name
      )
    
    if(alpha_var == "occupancy_rate") {
      size_values <- route_data$occupancy_rate
    } else {
      size_values <- route_data$delay_min
    }
    
    m <- m %>%
      addCircleMarkers(
        data = route_data,
        radius = ~ 6 + (get(alpha_var) - min_val) / (max_val - min_val) * 8,
        stroke = TRUE,
        color = "white",
        weight = 1.5,
        fillColor = route_color,
        fillOpacity = 0.85,
        popup = ~paste0(
          "<strong>Stop ID:</strong> ", stop_id, "<br>",
          "<strong>Route:</strong> ", route_name, "<br>",
          "<strong>Occupancy Rate:</strong> ", round(occupancy_rate*100, 1), "%<br>",
          "<strong>Delay:</strong> ", round(delay_min*60, 0), " seconds"
        ),
        label = ~paste0(
          "Stop: ", stop_id, " | ",
          "Route: ", route_name, " | ",
          "Occupancy: ", round(occupancy_rate*100, 1), "% | ",
          "Delay: ", round(delay_min*60, 0), "s"
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px", "background-color" = "white"),
          textsize = "13px",
          direction = "auto",
          offset = c(0, -10),
          sticky = FALSE,
          opacity = 0.9
        ),
        group = route_name
      )
  }
  
  legend_html <- paste0(
    "<div style='padding:6px; background-color:white; border-radius:4px; box-shadow:0 0 10px rgba(0,0,0,0.2);'>",
    "<div><strong>Circle Size:</strong> ", size_label, "</div>",
    "<div style='margin-top:5px;'><strong>Hover:</strong> Stop Details</div>",
    "<div style='margin-top:5px;'><strong>Click:</strong> Full Information</div>",
    "</div>"
  )
  
  m <- m %>%
    addControl(html = legend_html, position = "bottomright") %>%
    addLayersControl(
      overlayGroups = unique(sf_stops$route_name),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(m)
}

makemap <- function(alpha_var = "occupancy_rate") {
  create_route_map(sf_stops, alpha_var)
}
