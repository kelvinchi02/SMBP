source("styles.R")
source("api_utils.R")

# -------------------------------------------------------------------------
# UI DEFINITION
# -------------------------------------------------------------------------

rider_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        /* Cleaned up CSS without banner styles */
        .ridership-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 3rem; margin-bottom: 3rem; }
        .ridership-item { border-bottom: 1px solid #f0f0f0; padding-bottom: 2rem; }
        .ridership-full { border-bottom: 1px solid #f0f0f0; padding-bottom: 2rem; }
        .ridership-controls { margin-bottom: 1.5rem; }
        
        .realtime-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .realtime-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .realtime-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
        
        .kpi-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 2rem; }
        .kpi-card { background: transparent; padding: 0; text-align: center; border-bottom: 1px solid #f0f0f0; padding-bottom: 1rem; }
        .kpi-label { font-size: 0.75rem; color: #6c757d; margin-bottom: 0.5rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.05em; }
        .kpi-value { font-size: 1.5rem; font-weight: 700; color: #2c3e50; }
        .kpi-value.normal { color: #2ECC71; }
        .kpi-value.busy { color: #F39C12; }
        .kpi-value.overcrowded { color: #E74C3C; }
        
        .ai-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .ai-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .ai-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; }
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
          h2("Ridership Dashboard"),
          back_button()
        )
      ),

      # REMOVED: Large Decorative Icon Banner

      div(
        class = "page-content",
        
        # Real-time KPI Section
        div(
          class = "page-section",
          div(class = "realtime-section",
            div(
              class = "realtime-header",
              div(class = "realtime-title", "Real-time Ridership Metrics"),
              actionButton("refresh_ridership", "Refresh Data", class = "btn-refresh")
            ),
            uiOutput("ridership_kpis")
          )
        ),
        
        # AI Section
        div(
          class = "page-section",
          div(class = "ai-section",
            div(
              class = "ai-header",
              div(class = "ai-title", "AI Demand & Capacity Insight"),
              span()
            ),
            uiOutput("ai_ridership_display"),
            div(
              class = "ai-buttons",
              actionButton("get_ridership_insight", "Generate AI Insight", class = "btn-ai")
              # REMOVED: "Add Trip" button (Now handled in Scheduler)
            )
          )
        ),
        
        # Trend Plot
        div(
          class = "page-section",
          div(class = "ridership-item",
            plotOutput("trendPlot", height = "600px")
          )
        ),
        
        # Daily Map Plot
        div(
          class = "page-section",
          div(class = "ridership-controls",
            # Note: We keep the input static for now as updating choices dynamically requires more server logic.
            # It will show dates available at initial load.
            pickerInput(
              inputId = "ride_date_select",
              label = "Select Service Date:",
              # Fallback to current date if 'info' isn't available yet (e.g. at strict startup)
              choices = if(exists("info")) sort(unique(info$service_date)) else Sys.Date(),
              selected = if(exists("info")) min(info$service_date) else Sys.Date(),
              options = list(`actions-box` = TRUE, `style` = "btn-primary"),
              multiple = FALSE
            )
          ),
          div(
            class = "ridership-full",
            plotlyOutput("dailyMap", height = "750px")
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

# -------------------------------------------------------------------------
# DYNAMIC VISUALIZATION LOGIC
# -------------------------------------------------------------------------

# Plot 1: Daily Time Series (Filtered by day)
create_ride_plot1 <- function(data, whatday, routes_ref) {
  # Robust check: whatday comes from input, might be NULL initially
  if(is.null(whatday) || is.null(data) || nrow(data) == 0) return(NULL)
  
  ggplot(data[service_date == whatday], aes(x = datetime, y = passengers_onboard, colour = route_id)) +
    geom_point(aes(shape = direction_name)) +
    geom_line(aes(group = trip_id)) +
    theme_bw() +
    scale_color_manual(
      name = "Route",
      values = setNames(routes_ref[, route_color], routes_ref[, route_id]),
      labels = setNames(routes_ref[, route_name], routes_ref[, route_id])
    ) +
    scale_shape_discrete(name = "Direction") +
    scale_x_datetime(name = "Time", breaks = "1 hour", date_labels = "%H:%M", expand = expansion(mult = 0.02)) +
    ylab("Passengers Onboard") +
    ggtitle(paste("Passenger Count by Route on", format(as.Date(whatday), "%B %d, %Y"))) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# Plot 2: Bar Chart (All days)
create_ride_trend_bar <- function(data, routes_ref) {
  # RECALCULATE AGGREGATION ON LIVE DATA
  ride.data <- data[, .(sum(passengers_onboard)), .(route_id, service_date)]
  setnames(ride.data, "V1", "total_passengers")
  
  ggplot(ride.data, aes(x = service_date, y = total_passengers, fill = route_id)) +
    geom_col(width = 0.8, color = 'white', size = 0.2) +
    geom_text(aes(label = total_passengers), position = position_stack(vjust = 0.5), color = "white", size = 3.5, fontface = "bold") +
    scale_x_date(name = "Day", breaks = "1 day", date_labels = "%m-%d", expand = expansion(mult = 0)) +
    scale_y_continuous(name = "Passengers Onboard", expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(
      name = "Route Info",
      values = setNames(routes_ref$route_color, routes_ref$route_id),
      labels = setNames(routes_ref$route_name, routes_ref$route_id),
      guide = guide_legend(reverse = TRUE) 
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      plot.title = element_text(face = "bold", size = 14)
    ) +
    labs(title = "Daily Passengers by Route")
}