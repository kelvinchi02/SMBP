library(ggridges)
library(DT)
library(highcharter)
library(dplyr)
library(data.table)

source("styles.R")

# Weather page UI
weather_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .weather-grid {
          display: grid;
          grid-template-columns: 1.2fr 0.8fr;
          gap: 3rem;
          margin-bottom: 0;
        }
        
        .weather-main {
          border-bottom: 1px solid #f0f0f0;
          padding-bottom: 2rem;
        }
        
        .weather-side {
          display: grid;
          grid-template-rows: 1fr 1fr;
          gap: 2rem;
        }
        
        .weather-chart {
          border-bottom: 1px solid #f0f0f0;
          padding-bottom: 1.5rem;
        }
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
        class = "page-content",
        
        div(
          class = "page-section",
          div(class = "weather-grid",
            div(
              class = "weather-main",
              highchartOutput("wea_hc_output", height = "600px")
            ),
            div(
              class = "weather-side",
              div(
                class = "weather-chart",
                plotOutput("wea_gg_output1", height = "280px")
              ),
              div(
                class = "weather-chart",
                plotOutput("wea_gg_output2", height = "280px")
              )
            )
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

# Prepare weather summary data
wea.table <- info[, .(mean(delay_min)), .(weather_hourly_conditions, delay_category)] |> 
  dcast(weather_hourly_conditions ~ delay_category, value.var = 'V1')
wea.table <- wea.table[, !'On-time']

wea.table[, Label := fcase(
  weather_hourly_conditions == "Clear", "Clear ☀️",
  weather_hourly_conditions == "Cloudy", "Cloudy ☁️",
  weather_hourly_conditions == "Rain", "Rain ☔"
)]

table.wea <- highchart() %>%
  hc_chart(type = "column", inverted = TRUE, polar = TRUE) %>%
  hc_title(text = "<b>Commute Time Deviations</b>", align = "center") %>%
  hc_subtitle(text = "Comparison of Delay vs. Early arrival by Weather", align = "center") %>%
  hc_pane(size = '85%', innerSize = '20%', endAngle = 270) %>%
  hc_xAxis(
    categories = wea.table$Label,
    tickmarkPlacement = 'on',
    lineWidth = 0,
    labels = list(style = list(fontSize = '14px', fontWeight = 'bold'), distance = 15)
  ) %>%
  hc_yAxis(
    min = 0,
    gridLineInterpolation = 'polygon',
    lineWidth = 0,
    showLastLabel = FALSE,
    labels = list(enabled = FALSE)
  ) %>%
  hc_plotOptions(
    column = list(grouping = TRUE, shadow = FALSE, borderWidth = 0, pointPadding = 0, groupPadding = 0.1)
  ) %>%
  hc_add_series(name = "Delayed", data = wea.table$Delayed, color = "#E74C3C", tooltip = list(valueSuffix = " min delay")) %>%
  hc_add_series(name = "Early", data = abs(wea.table$Early), color = "#2ECC71", tooltip = list(valueSuffix = " min saved")) %>%
  hc_tooltip(shared = TRUE, headerFormat = '<span style="font-size: 12px"><b>{point.key}</b></span><br/>')

plot.wea1 <- ggplot(info[delay_min != 0], aes(x = delay_min, y = weather_hourly_conditions, fill = delay_category)) +
  geom_density_ridges(alpha = 0.7, color = "white", linetype = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  scale_fill_manual(values = c("Delayed" = "#E74C3C", "Early" = "#2ECC71"), name = "Delay Status") +
  scale_x_continuous(name = "Delay (minutes)", expand = c(0.01, 0), breaks = pretty_breaks(n = 8)) +
  scale_y_discrete(name = "Weather Conditions") +
  labs(title = "Delay Distribution by Weather Condition", subtitle = "Density of early and delayed trips across different weather conditions") +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "italic")
  )

plot.wea2 <- ggplot(info, aes(x = weather_hourly_conditions, y = delay_min)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = delay_category), position = position_jitter(width = 0.2, height = 0), alpha = 0.4, size = 2) +
  scale_color_manual(values = delay_colors, name = "Delay Status") +
  labs(
    title = "Impact of Weather Conditions on Transit Delays",
    subtitle = "Distribution of delays across different weather conditions",
    x = "Weather Condition",
    y = "Delay (minutes)",
    caption = "Negative values indicate early arrivals, positive values indicate delays"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )