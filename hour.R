source("styles.R")

# Delay page UI
delay_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .delay-grid {
          display: grid;
          grid-template-columns: 0.8fr 1.2fr;
          gap: 3rem;
        }
        
        .delay-item {
          border-bottom: 1px solid #f0f0f0;
          padding-bottom: 2rem;
        }
        
        .delay-controls {
          margin-bottom: 1.5rem;
        }
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
        class = "page-content",
        
        div(
          class = "page-section",
          div(class = "delay-grid",
            div(
              class = "delay-item",
              plotOutput("hour_delay_plot1", height = "750px")
            ),
            div(
              class = "delay-item",
              div(class = "delay-controls",
                pickerInput(
                  inputId = "hourwhatRoute",
                  label = NULL,
                  choices = final_choices[-1],
                  choicesOpt = list(style = final_styles[-1])
                )
              ),
              plotOutput("hour_delay_plot2", height = "700px")
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