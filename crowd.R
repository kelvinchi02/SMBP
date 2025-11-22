library(data.table)
library(plotly)
library(scales)
library(RColorBrewer)

source("styles.R")

# Crowding page UI
crowd_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .crowd-grid {
          display: grid;
          grid-template-columns: 1fr;
          gap: 3rem;
        }
        
        .crowd-item {
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
          h2("Crowd Dashboard"),
          back_button()
        )
      ),
      
      div(
        class = "page-content",
        
        div(
          class = "page-section",
          div(class = "crowd-grid",
            div(
              class = "crowd-item",
              plotlyOutput("crowd_ggplot1", height = "600px")
            ),
            div(
              class = "crowd-item",
              plotOutput("crowd_ggplot2", height = "600px")
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

data.crowd <- info[, .N, .(route_id, stop_id, crowding_level)][order(stop_id)]

crowd.plot1 <- ggplot(data.crowd, aes(x = stop_id, y = N, fill = crowding_level)) +
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

redata <- info[, .(lon, lat, occupancy_rate, trip_id)]

redata[, hover_info := paste0(
  "<b>Trip ID:</b> ", trip_id, "<br>",
  "<b>Occupancy:</b> ", scales::percent(occupancy_rate, accuracy = 0.1)
)]

crowd.plot2 <- plot_ly(
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