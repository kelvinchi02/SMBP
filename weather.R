library(ggridges)
library(DT)
library(highcharter)
library(dplyr)
library(data.table)

source("styles.R")
source("api_utils.R")

# -------------------------------------------------------------------------
# UI DEFINITION
# -------------------------------------------------------------------------
weather_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .page-icon-banner { background: linear-gradient(135deg, #30cfd0 0%, #330867 100%); padding: 2rem 0; margin-bottom: 2rem; }
        .icon-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 2rem; max-width: 1200px; margin: 0 auto; padding: 0 2rem; }
        .icon-item { text-align: center; color: white; transition: transform 0.3s ease; }
        .icon-item:hover { transform: translateY(-5px); }
        .icon-circle { width: 80px; height: 80px; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 1rem; font-size: 2rem; background: rgba(255, 255, 255, 0.2); backdrop-filter: blur(10px); border: 2px solid rgba(255, 255, 255, 0.3); }
        .icon-item.weather .icon-circle { background: rgba(52, 152, 219, 0.9); }
        .icon-item.temperature .icon-circle { background: rgba(231, 76, 60, 0.9); }
        .icon-item.rain .icon-circle { background: rgba(52, 73, 94, 0.9); }
        .icon-item.forecast .icon-circle { background: rgba(46, 204, 113, 0.9); }
        .icon-label { font-size: 0.95rem; font-weight: 600; margin-bottom: 0.5rem; }
        .icon-description { font-size: 0.8rem; opacity: 0.9; line-height: 1.4; }
        
        .weather-grid { display: grid; grid-template-columns: 1.2fr 0.8fr; gap: 3rem; margin-bottom: 0; }
        .weather-main { border-bottom: 1px solid #f0f0f0; padding-bottom: 2rem; }
        .weather-side { display: grid; grid-template-rows: 1fr 1fr; gap: 2rem; }
        .weather-chart { border-bottom: 1px solid #f0f0f0; padding-bottom: 1.5rem; }
        
        .realtime-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .realtime-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .realtime-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
        
        .weather-current-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 2rem; margin-bottom: 2rem; }
        .weather-kpi { background: transparent; padding: 0; text-align: center; border-bottom: 1px solid #f0f0f0; padding-bottom: 1rem; }
        .weather-kpi-label { font-size: 0.75rem; color: #6c757d; margin-bottom: 0.5rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.05em; }
        .weather-kpi-value { font-size: 1.5rem; font-weight: 700; color: #2c3e50; }
        
        .forecast-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 1.5rem; }
        .forecast-item { text-align: center; padding: 1rem; border: 1px solid #f0f0f0; }
        .forecast-time { font-size: 0.85rem; font-weight: 600; color: #495057; margin-bottom: 0.75rem; }
        .forecast-condition { font-size: 0.8rem; color: #6c757d; margin-bottom: 0.5rem; }
        .forecast-temp { font-size: 1.1rem; font-weight: 700; color: #2c3e50; }
        
        .ai-section { margin-bottom: 3rem; padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
        .ai-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; }
        .ai-title { font-size: 0.75rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.12em; margin: 0; }
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
          h2("Weather Dashboard"),
          back_button()
        )
      ),

      div(
        class = "page-icon-banner",
        div(
          class = "icon-grid",
          div(class = "icon-item weather", div(class = "icon-circle", "☁️"), div(class = "icon-label", "Weather Conditions"), div(class = "icon-description", "Current weather status")),
          div(class = "icon-item temperature", div(class = "icon-circle", "🌡️"), div(class = "icon-label", "Temperature Impact"), div(class = "icon-description", "Monitor thermal effects")),
          div(class = "icon-item rain", div(class = "icon-circle", "☔"), div(class = "icon-label", "Precipitation"), div(class = "icon-description", "Track rain & snow impact")),
          div(class = "icon-item forecast", div(class = "icon-circle", "🔮"), div(class = "icon-label", "Forecast Analysis"), div(class = "icon-description", "Plan for conditions ahead"))
        )
      ),

      div(
        class = "page-content",
        
        # Real-time Weather Section
        div(
          class = "page-section",
          div(class = "realtime-section",
            div(
              class = "realtime-header",
              div(class = "realtime-title", "Current Weather & Forecast"),
              actionButton("refresh_weather", "Refresh Data", class = "btn-refresh")
            ),
            uiOutput("weather_current_display"),
            uiOutput("weather_forecast_display")
          )
        ),
        
        # AI Advice Section
        div(
          class = "page-section",
          div(class = "ai-section",
            div(
              class = "ai-header",
              div(class = "ai-title", "AI Weather-Based Operations Advice"),
              span()
            ),
            uiOutput("ai_weather_display"),
            div(
              class = "ai-buttons",
              actionButton("get_weather_insight", "Generate AI Insight", class = "btn-ai"),
              actionButton("add_weather_trip_btn", "Add Trip to Routes", class = "btn-ai btn-add-trip")
            )
          )
        ),
        
        # Charts Section
        div(
          class = "page-section",
          div(class = "weather-grid",
            div(class = "weather-main", highchartOutput("wea_hc_output", height = "600px")),
            div(
              class = "weather-side",
              div(class = "weather-chart", plotOutput("wea_gg_output1", height = "280px")),
              div(class = "weather-chart", plotOutput("wea_gg_output2", height = "280px"))
            )
          )
        ),
        
        div(class = "page-section", plotOutput("weather_impact_chart", height = "400px"))
      ),
      
      div(class = "page-footer", div(class = "container", p("SmartTransit Analytics Dashboard © 2025")))
    )
  )
}

# -------------------------------------------------------------------------
# DYNAMIC PLOT GENERATORS
# -------------------------------------------------------------------------

# 1. Polar Column Chart (Highchart)
create_weather_polar_chart <- function(data) {
  # RECALCULATE ON LIVE DATA
  wea.table <- data[, .(mean(delay_min)), .(weather_hourly_conditions, delay_category)] |> 
    dcast(weather_hourly_conditions ~ delay_category, value.var = 'V1')
  
  if ("On-time" %in% names(wea.table)) wea.table <- wea.table[, !'On-time']

  wea.table[, Label := fcase(
    weather_hourly_conditions == "Clear", "Clear ☀️",
    weather_hourly_conditions == "Cloudy", "Cloudy ☁️",
    weather_hourly_conditions == "Rain", "Rain ☔"
  )]

  highchart() %>%
    hc_chart(type = "column", inverted = TRUE, polar = TRUE) %>%
    hc_title(text = "<b>Commute Time Deviations</b>", align = "center") %>%
    hc_subtitle(text = "Comparison of Delay vs. Early arrival by Weather", align = "center") %>%
    hc_pane(size = '85%', innerSize = '20%', endAngle = 270) %>%
    hc_xAxis(categories = wea.table$Label, tickmarkPlacement = 'on', lineWidth = 0) %>%
    hc_yAxis(min = 0, gridLineInterpolation = 'polygon', lineWidth = 0, showLastLabel = FALSE) %>%
    hc_plotOptions(column = list(grouping = TRUE, shadow = FALSE, borderWidth = 0)) %>%
    hc_add_series(name = "Delayed", data = wea.table$Delayed, color = "#E74C3C") %>%
    hc_add_series(name = "Early", data = abs(wea.table$Early), color = "#2ECC71")
}

# 2. Density Ridges (ggplot)
create_weather_ridge_plot <- function(data) {
  ggplot(data[delay_min != 0], aes(x = delay_min, y = weather_hourly_conditions, fill = delay_category)) +
    geom_density_ridges(alpha = 0.7, color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_fill_manual(values = c("Delayed" = "#E74C3C", "Early" = "#2ECC71"), name = "Delay Status") +
    scale_x_continuous(name = "Delay (minutes)", expand = c(0.01, 0)) +
    labs(title = "Delay Distribution by Weather", subtitle = "Density of early/delayed trips") +
    theme_bw() + theme(legend.position = "top")
}

# 3. Jitter Plot (ggplot)
create_weather_jitter_plot <- function(data) {
  ggplot(data, aes(x = weather_hourly_conditions, y = delay_min)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = delay_category), position = position_jitter(width = 0.2, height = 0), alpha = 0.4, size = 2) +
    scale_color_manual(values = c("Delayed" = "#E74C3C", "Early" = "#2ECC71", "On-time" = "#3498DB"), name = "Delay Status") +
    labs(title = "Impact of Weather on Delays", x = "Weather Condition", y = "Delay (minutes)") +
    theme_bw() + theme(legend.position = "top")
}