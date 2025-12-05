library(data.table)
library(plotly)
library(scales)
library(RColorBrewer)

source("styles.R")
source("api_utils.R")

# -------------------------------------------------------------------------
# UI DEFINITION
# -------------------------------------------------------------------------

crowd_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        /* Cleaned up CSS */
        .crowd-grid { display: grid; grid-template-columns: 1fr; gap: 3rem; }
        .crowd-item { border-bottom: 1px solid #f0f0f0; padding-bottom: 2rem; }
        
        .realtime-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .realtime-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .realtime-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
        
        .kpi-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 2rem; margin-bottom: 1.5rem; }
        .kpi-card { background: transparent; padding: 0; text-align: center; border-bottom: 1px solid #f0f0f0; padding-bottom: 1rem; }
        .kpi-label { font-size: 0.75rem; color: #6c757d; margin-bottom: 0.5rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.05em; }
        .kpi-value { font-size: 1.5rem; font-weight: 700; color: #2c3e50; }
        .kpi-value.normal { color: #2ECC71; }
        .kpi-value.busy { color: #F39C12; }
        .kpi-value.critical { color: #E74C3C; }
        
        .breakdown-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 1.5rem; }
        .breakdown-item { text-align: center; padding: 0.75rem 0; border-bottom: 1px solid #f0f0f0; }
        .breakdown-label { font-size: 0.75rem; color: #6c757d; margin-bottom: 0.5rem; }
        .breakdown-value { font-size: 1.2rem; font-weight: 700; }
        .breakdown-value.low { color: #2ECC71; }
        .breakdown-value.medium { color: #F39C12; }
        .breakdown-value.high { color: #E74C3C; }
        
        .ai-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .ai-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .ai-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
        .ai-content { background: transparent; padding: 1rem 0; margin-bottom: 1.5rem; }
        .ai-analysis { font-size: 0.9rem; line-height: 1.6; color: #495057; margin-bottom: 1rem; }
        
        .btn-ai { flex: 1; padding: 0.75rem 1.2rem; background: transparent; color: #495057; border: 1px solid #dee2e6; font-size: 0.9rem; font-weight: 500; cursor: pointer; transition: all 0.2s; }
        .btn-ai:hover { border-color: #adb5bd; }
        .btn-refresh { background: transparent; border: 1px solid #dee2e6; color: #495057; padding: 0.5rem 1rem; font-size: 0.85rem; transition: all 0.2s; }
        .btn-refresh:hover { border-color: #adb5bd; }
      "))
    ),
    
    div(
      class = "page-wrapper",
      
      div(
        class = "page-header",
        div(
          class = "container",
          h2("Crowd Dashboard"),
          back_button()
        )
      ),

      # REMOVED: Large Decorative Icon Banner

      div(
        class = "page-content",
        
        # Real-time Crowding Section
        div(
          class = "page-section",
          div(class = "realtime-section",
            div(
              class = "realtime-header",
              div(class = "realtime-title", "Live Crowding Status"),
              actionButton("refresh_crowding", "Refresh Data", class = "btn-refresh")
            ),
            uiOutput("crowding_kpis"),
            uiOutput("crowding_breakdown_display")
          )
        ),
        
        # AI Advice Section
        div(
          class = "page-section",
          div(class = "ai-section",
            div(
              class = "ai-header",
              div(class = "ai-title", "AI Crowding & Capacity Advice"),
              span()
            ),
            uiOutput("ai_crowding_display"),
            div(
              class = "ai-buttons",
              actionButton("get_crowding_insight", "Generate AI Insight", class = "btn-ai")
              # REMOVED: "Add Trip" button (Now handled in Scheduler)
            )
          )
        ),
        
        # Plot Grid
        div(
          class = "page-section",
          div(class = "crowd-grid",
            div(class = "crowd-item", plotlyOutput("crowd_ggplot1", height = "600px")),
            div(class = "crowd-item", plotOutput("crowd_ggplot2", height = "600px"))
          )
        )
      ),
      
      div(class = "page-footer", div(class = "container", p("SmartTransit Analytics Dashboard © 2025")))
    )
  )
}

# -------------------------------------------------------------------------
# VISUALIZATION LOGIC
# -------------------------------------------------------------------------

# FUNCTION 1: Stacked Bar Chart for Crowding Levels
create_crowd_bar <- function(data) {
  
  # Calculate aggregation dynamically from live data
  plot_data <- data[, .N, .(route_id, stop_id, crowding_level)][order(stop_id)]
  
  ggplot(plot_data, aes(x = stop_id, y = N, fill = crowding_level)) +
    geom_col(position = "fill", width = 0.8) +
    scale_fill_manual(values = c('Low' = '#2ca02c', 'Medium' = '#ff7f0e', 'High' = '#d62728')) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_wrap(vars(route_id), scales = "free_x") +
    theme_minimal() +
    labs(x = "Stop ID", y = "Proportion", fill = "Crowding Level", title = "Crowding Level Proportions by Stop and Route") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      strip.text = element_text(size = 12, face = "bold")
    )
}

# FUNCTION 2: Scatter Map for Occupancy
create_crowd_map <- function(data) {
  
  # Prepare live data for the map
  redata <- data[, .(lon, lat, occupancy_rate, trip_id)]
  redata[, hover_info := paste0(
    "<b>Trip ID:</b> ", trip_id, "<br>",
    "<b>Occupancy:</b> ", scales::percent(occupancy_rate, accuracy = 0.1)
  )]

  plot_ly(
    data = redata,
    x = ~lon,
    y = ~lat,
    type = 'scatter',
    mode = 'markers',
    color = ~occupancy_rate,
    colors = "YlOrRd", 
    text = ~hover_info,
    hovertemplate = "%{text}<extra></extra>",
    marker = list(size = 12, line = list(color = '#333333', width = 1), opacity = 0.9)
  ) %>%
    layout(
      title = list(text = "<b>Trip Locations & Occupancy Rates</b>"),
      xaxis = list(title = "Longitude", zeroline = FALSE),
      yaxis = list(title = "Latitude", zeroline = FALSE, scaleanchor = "x", scaleratio = 1),
      legend = list(title = list(text = "Occupancy"))
    ) %>%
    colorbar(title = "Rate", tickformat = ".0%")
}