source("styles.R")

# Ridership page UI
rider_ui <- function() {
  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .ridership-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 3rem;
          margin-bottom: 3rem;
        }
        
        .ridership-item {
          border-bottom: 1px solid #f0f0f0;
          padding-bottom: 2rem;
        }
        
        .ridership-full {
          border-bottom: 1px solid #f0f0f0;
          padding-bottom: 2rem;
        }
        
        .ridership-controls {
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
          h2("Ridership Dashboard"),
          back_button()
        )
      ),
      
      div(
        class = "page-content",
        
        div(
          class = "page-section",
          div(class = "ridership-item",
            plotOutput("trendPlot", height = "600px")
          )
        ),
        
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
        div(
          class = "container",
          p("SmartTransit Analytics Dashboard © 2025")
        )
      )
    )
  )
}

ride.data <- info[, .(sum(passengers_onboard)), .(route_id, service_date)]
date_choices <- unique(ride.data$service_date)

ride.plot1 <- function(whatday) {
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

ride.plot2 <- ggplot(ride.data, aes(x = service_date, y = V1, fill = route_id)) +
  geom_col(width = 1, color = 'white', size = 0.2) +
  geom_text(aes(label = V1), position = position_stack(vjust = 0.5), color = "white", size = 3.5, fontface = "bold") +
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