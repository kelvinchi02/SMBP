source("styles.R")
source("api_utils.R")

# -------------------------------------------------------------------------
# UI DEFINITION
# -------------------------------------------------------------------------
delay_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .page-icon-banner { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 2rem 0; margin-bottom: 2rem; }
        .icon-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 2rem; max-width: 1200px; margin: 0 auto; padding: 0 2rem; }
        .icon-item { text-align: center; color: white; transition: transform 0.3s ease; }
        .icon-item:hover { transform: translateY(-5px); }
        .icon-circle { width: 80px; height: 80px; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 1rem; font-size: 2rem; background: rgba(255, 255, 255, 0.2); backdrop-filter: blur(10px); border: 2px solid rgba(255, 255, 255, 0.3); }
        .icon-item.delay .icon-circle { background: rgba(231, 76, 60, 0.9); }
        .icon-item.punctuality .icon-circle { background: rgba(46, 204, 113, 0.9); }
        .icon-item.issues .icon-circle { background: rgba(243, 156, 18, 0.9); }
        .icon-item.routes .icon-circle { background: rgba(52, 152, 219, 0.9); }
        .icon-label { font-size: 0.95rem; font-weight: 600; margin-bottom: 0.5rem; }
        .icon-description { font-size: 0.8rem; opacity: 0.9; line-height: 1.4; }

        .delay-grid { display: grid; grid-template-columns: 0.8fr 1.2fr; gap: 3rem; }
        .delay-item { border-bottom: 1px solid #f0f0f0; padding-bottom: 2rem; }
        .delay-controls { margin-bottom: 1.5rem; }
        
        .realtime-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .realtime-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .realtime-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
        
        .kpi-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 2rem; margin-bottom: 1.5rem; }
        .kpi-card { background: transparent; padding: 0; text-align: center; border-bottom: 1px solid #f0f0f0; padding-bottom: 1rem; }
        .kpi-label { font-size: 0.75rem; color: #6c757d; margin-bottom: 0.5rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.05em; }
        .kpi-value { font-size: 1.5rem; font-weight: 700; color: #2c3e50; }
        .kpi-value.good { color: #2ECC71; }
        .kpi-value.warning { color: #F39C12; }
        .kpi-value.bad { color: #E74C3C; }
        
        .worst-stops { background: transparent; padding: 1rem 0; }
        .worst-stops-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; margin-bottom: 1rem; text-transform: uppercase; letter-spacing: 0.05em; }
        .stop-item { display: flex; justify-content: space-between; padding: 0.75rem 0; border-bottom: 1px solid #f0f0f0; font-size: 0.85rem; color: #495057; }
        
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
          h2("Delay Dashboard"),
          back_button()
        )
      ),

      div(
        class = "page-icon-banner",
        div(
          class = "icon-grid",
          div(class = "icon-item delay", div(class = "icon-circle", "⏱️"), div(class = "icon-label", "Delay Tracking"), div(class = "icon-description", "Monitor real-time delays")),
          div(class = "icon-item punctuality", div(class = "icon-circle", "✓"), div(class = "icon-label", "Punctuality Rate"), div(class = "icon-description", "Track on-time performance")),
          div(class = "icon-item issues", div(class = "icon-circle", "⚠️"), div(class = "icon-label", "Problem Stops"), div(class = "icon-description", "Identify delay hotspots")),
          div(class = "icon-item routes", div(class = "icon-circle", "🚌"), div(class = "icon-label", "Route Analysis"), div(class = "icon-description", "Analyze route performance"))
        )
      ),

      div(
        class = "page-content",
        
        # Real-time Section (Server must render realtime_kpis and worst_stops_display)
        div(
          class = "page-section",
          div(class = "realtime-section",
            div(
              class = "realtime-header",
              div(class = "realtime-title", "Real-time Performance Metrics"),
              actionButton("refresh_realtime", "Refresh Data", class = "btn-refresh")
            ),
            uiOutput("realtime_kpis"),
            uiOutput("worst_stops_display")
          )
        ),
        
        # AI Section (Server must render ai_insight_display)
        div(
          class = "page-section",
          div(class = "ai-section",
            div(
              class = "ai-header",
              div(class = "ai-title", "AI Delay & Wait-Time Insight"),
              span()
            ),
            uiOutput("ai_insight_display"),
            div(
              class = "ai-buttons",
              actionButton("get_ai_insight", "Generate AI Insight", class = "btn-ai"),
              actionButton("add_trip_btn", "Add Trip to Route", class = "btn-ai btn-add-trip")
            )
          )
        ),
        
        # Plot Section
        div(
          class = "page-section",
          div(class = "delay-grid",
            div(class = "delay-item", plotOutput("hour_delay_plot1", height = "750px")),
            div(
              class = "delay-item",
              div(class = "delay-controls",
                pickerInput(
                  inputId = "hourwhatRoute",
                  label = NULL,
                  choices = final_choices[-1], # Exclude "ALL" for this specific plot
                  choicesOpt = list(style = final_styles[-1])
                )
              ),
              plotOutput("hour_delay_plot2", height = "700px")
            )
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

# Pre-calculate data for the Schedule Adherence Plot
# We use 'info' from pre.R
hour.data <- info[delay_category != 'On-time', mean(delay_min), .(delay_category, route_id, hour)]
hour_plot_data <- merge(hour.data, routes, by = "route_id")

hour_plot1 <- ggplot(hour_plot_data, aes(x = V1, y = route_name, color = delay_category)) +
  geom_vline(xintercept = 0, color = "grey60", linetype = "dashed") +
  geom_segment(aes(x = 0, xend = V1, y = route_name, yend = route_name), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(V1, 2), hjust = ifelse(V1 > 0, -0.4, 1.4)), size = 3, fontface = "bold", show.legend = FALSE) +
  facet_wrap(vars(hour), ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Delayed" = "#E74C3C", "Early" = "#2ECC71")) +
  scale_x_continuous(limits = c(-1.2, 1.2), breaks = seq(-1, 1, 0.5)) +
  labs(
    title = "Hourly Schedule Adherence",
    subtitle = "Deviation in minutes by Route and Hour",
    x = "Minutes (Negative = Early, Positive = Delayed)",
    y = NULL,
    color = "Status"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#f8f9fa", color = "grey80"),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

hour_plot2 <- function(whatRoutine) {
  # Safety check: if user selects nothing, default to first route
  if (is.null(whatRoutine)) whatRoutine <- routes$route_id[1]
  
  ggplot(info[route_id == whatRoutine], aes(x = hour, y = delay_min)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = delay_category), position = position_jitter(width = 0.2, height = 0), alpha = 0.4, size = 2) +
    scale_color_manual(values = delay_colors, name = "Delay Status") +
    labs(title = "Distribution of Delays by Hour", x = "Hour of Scheduled Arrival", y = "Delay (minutes)") +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}