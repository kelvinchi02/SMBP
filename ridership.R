source("styles.R")
source("api_utils.R")

# -------------------------------------------------------------------------
# DATA PREPARATION (Must run before UI)
# -------------------------------------------------------------------------

# Aggregated data for the Trend Plot (Bar Chart)
ride.data <- info[, .(sum(passengers_onboard)), .(route_id, service_date)]
setnames(ride.data, "V1", "total_passengers") # Clean column name

# Choices for the Dropdown
date_choices <- sort(unique(ride.data$service_date))

# -------------------------------------------------------------------------
# UI DEFINITION
# -------------------------------------------------------------------------

rider_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .page-icon-banner { background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 2rem 0; margin-bottom: 2rem; }
        .icon-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 2rem; max-width: 1200px; margin: 0 auto; padding: 0 2rem; }
        .icon-item { text-align: center; color: white; transition: transform 0.3s ease; }
        .icon-item:hover { transform: translateY(-5px); }
        .icon-circle { width: 80px; height: 80px; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 1rem; font-size: 2rem; background: rgba(255, 255, 255, 0.2); backdrop-filter: blur(10px); border: 2px solid rgba(255, 255, 255, 0.3); }
        .icon-item.passengers .icon-circle { background: rgba(52, 152, 219, 0.9); }
        .icon-item.trends .icon-circle { background: rgba(46, 204, 113, 0.9); }
        .icon-item.daily .icon-circle { background: rgba(155, 89, 182, 0.9); }
        .icon-item.peak .icon-circle { background: rgba(243, 156, 18, 0.9); }
        .icon-label { font-size: 0.95rem; font-weight: 600; margin-bottom: 0.5rem; }
        .icon-description { font-size: 0.8rem; opacity: 0.9; line-height: 1.4; }
        
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
        .btn-add-trip { background: #2c3e50; color: white; border: 1px solid #2c3e50; }
        .btn-add-trip:hover { border-color: #1a252f; }
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

      div(
        class = "page-icon-banner",
        div(
          class = "icon-grid",
          div(class = "icon-item passengers", div(class = "icon-circle", "👥"), div(class = "icon-label", "Passenger Count"), div(class = "icon-description", "Track total ridership")),
          div(class = "icon-item trends", div(class = "icon-circle", "📈"), div(class = "icon-label", "Demand Trends"), div(class = "icon-description", "Analyze usage patterns")),
          div(class = "icon-item daily", div(class = "icon-circle", "📅"), div(class = "icon-label", "Daily Statistics"), div(class = "icon-description", "View daily breakdowns")),
          div(class = "icon-item peak", div(class = "icon-circle", "⏰"), div(class = "icon-label", "Peak Hours"), div(class = "icon-description", "Identify busy periods"))
        )
      ),

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
            uiOutput("ridership_kpis") # Server must render this
          )
        ),
        
        # AI Section
        div(
          class = "page-section",
          div(class = "ai-section",
            div(
              class = "ai-header",
              div(class = "ai-title", "AI Demand & Extra-Trip Recommendation"),
              span()
            ),
            uiOutput("ai_ridership_display"), # Server must render this
            div(
              class = "ai-buttons",
              actionButton("get_ridership_insight", "Generate AI Insight", class = "btn-ai"),
              actionButton("add_ridership_trip_btn", "Add Trip to Route", class = "btn-ai btn-add-trip")
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
            pickerInput(
              inputId = "ride_date_select",
              label = "Select Service Date:",
              choices = as.character(date_choices),
              selected = as.character(min(date_choices)),
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
# VISUALIZATION LOGIC
# -------------------------------------------------------------------------

# Plot 1: Daily Time Series (Filtered by day)
ride.plot1 <- function(whatday) {
  # Robust check: whatday comes from input, might be NULL initially
  if(is.null(whatday)) return(NULL)
  
  ggplot(info[service_date == whatday], aes(x = datetime, y = passengers_onboard, colour = route_id)) +
    geom_point(aes(shape = direction_name)) +
    geom_line(aes(group = trip_id)) +
    theme_bw() +
    scale_color_manual(
      name = "Route",
      values = setNames(routes[, route_color], routes[, route_id]),
      labels = setNames(routes[, route_name], routes[, route_id])
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
ride.plot2 <- ggplot(ride.data, aes(x = service_date, y = total_passengers, fill = route_id)) +
  geom_col(width = 0.8, color = 'white', size = 0.2) +
  geom_text(aes(label = total_passengers), position = position_stack(vjust = 0.5), color = "white", size = 3.5, fontface = "bold") +
  scale_x_date(name = "Day", breaks = "1 day", date_labels = "%m-%d", expand = expansion(mult = 0)) +
  scale_y_continuous(name = "Passengers Onboard", expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    name = "Route Info",
    values = setNames(routes$route_color, routes$route_id),
    labels = setNames(routes$route_name, routes$route_id),
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