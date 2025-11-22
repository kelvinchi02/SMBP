library(data.table)
library(plotly)
library(ggrepel)
library(ggplot2)
library(sf)
library(scales)
library(lubridate)
library(shiny)

source("database_connection.R")

back_button <- actionButton("back_to_home", "← Back to Home", class = "btn-light mb-4")

Sys.setlocale("LC_TIME", "en_US.UTF-8")

info <- load_supabase_table("SmartTransit_Integrated")

info[, hour := hour(scheduled_arrival)]
info[, hour := factor(hour, levels = sort(unique(hour)))]
info[, delay_category := fcase(
  delay_min > 0, "Delayed",
  delay_min < 0, "Early",
  delay_min == 0, "On-time"
)]

stops <- info[, .(
  lon = mean(lon),
  lat = mean(lat),
  occupancy_rate = mean(occupancy_rate),
  delay_min = mean(delay_min)
), .(stop_id, route_id, route_name, route_long_name, route_color)][order(stop_id)]

routes <- unique(stops[, .(route_id, route_color, route_name)])
routes <- routes[info[, .N, .(route_id, route_length_km)], on = "route_id"][, !"N"][order(route_id)]

sf_stops <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4326)

delay_colors <- c("Delayed" = "#E74C3C", "Early" = "#2ECC71", "On-time" = "#3498DB")

choices_from_data <- routes$route_id
names(choices_from_data) <- routes$route_name

styles_from_data <- sprintf("color: %s; font-weight: bold;", routes$route_color)

final_choices <- c("ALL" = "ALL", choices_from_data)
final_styles <- c("color: black; font-weight: bold;", styles_from_data)

summary.plot1 <- ggplot(
  info,
  aes(y = factor(route_id, levels = routes$route_id), x = occupancy_rate, color = route_id)
) +
  geom_boxplot(
    aes(fill = after_scale(alpha(color, 0.3))),
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.7,
    width = 0.6
  ) +
  scale_color_manual(
    values = setNames(routes[, route_color], routes[, route_id]),
    labels = setNames(routes[, route_name], routes[, route_id]),
    name = "Transit Route"
  ) +
  scale_x_continuous(
    name = "Occupancy Rate",
    labels = percent_format(),
    limits = function(x) c(max(0, min(x) - 0.05), min(1, max(x) + 0.05))
  ) +
  scale_y_discrete(
    name = "Route",
    labels = sprintf("%s", routes$route_id)
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Occupancy Rate Distribution by Transit Route",
    subtitle = "Comparing passenger load across different routes",
    caption = "Dashed red line indicates 100% capacity"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

summary.plot2 <- ggplot(info, aes(x = route_id, y = delay_min)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(
    aes(color = delay_category),
    position = position_jitter(width = 0.2, height = 0),
    alpha = 0.4,
    size = 2
  ) +
  stat_summary(
    fun = mean,
    fun.min = function(x) mean(x) - sd(x),
    fun.max = function(x) mean(x) + sd(x),
    geom = "pointrange",
    size = 0.8,
    color = "black"
  ) +
  scale_color_manual(values = delay_colors, name = "Delay Status") +
  scale_x_discrete(
    name = "Transit Route",
    labels = setNames(sprintf("%s", routes$route_name), routes$route_id)
  ) +
  labs(
    title = "Transit Delays of Routes",
    subtitle = "Distribution of delays across different routes\nwith mean values (black dots) and standard deviation (error bars)",
    y = "Delay (minutes)",
    caption = "Negative values indicate early arrivals, positive values indicate delays"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

create_crowding_pie <- function(data, routes_info, route_idi = "ALL") {
  crowding_colors <- c("Low" = "#2ECC71", "Medium" = "#F39C12", "High" = "#E74C3C")

  if (route_idi == "ALL") {
    plot_data <- data[, .(N = sum(N)), by = crowding_level]
    main_title <- "All Routes Combined"
    title_color <- "#333333"
  } else {
    plot_data <- data[route_id == route_idi]
    route_index <- which(routes_info$route_id == route_idi)
    route_name <- routes_info$route_name[route_index]
    route_length <- routes_info$route_length_km[route_index]
    route_color <- routes_info$route_color[route_index]
    main_title <- paste0("Route ", route_idi, ": ", route_name, " (", route_length, " km)")
    title_color <- route_color
  }

  total <- sum(plot_data$N)
  all_levels <- c("Low", "Medium", "High")
  plot_data <- plot_data[match(all_levels, crowding_level)]

  fig <- plot_ly(
    plot_data,
    labels = ~crowding_level,
    values = ~N,
    type = "pie",
    marker = list(colors = crowding_colors[plot_data$crowding_level]),
    textinfo = "label+percent",
    hoverinfo = "text+percent",
    text = ~ paste(
      "Crowding:", crowding_level,
      "<br>Count:", N,
      "<br>Percentage:", sprintf("%4.3f%%", 100 * N / total)
    )
  )

  fig <- fig %>% layout(
    title = list(text = main_title, font = list(color = title_color)),
    legend = list(title = list(text = "Crowding Level")),
    margin = list(t = 100, b = 80, l = 50, r = 50)
  )

  return(fig)
}

create_route_map <- function(sf_stops, alpha_var = "occupancy_rate") {
  routes <- unique(sf_stops$route_id)

  m <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)

  if (alpha_var == "occupancy_rate") {
    min_val <- min(sf_stops$occupancy_rate)
    max_val <- max(sf_stops$occupancy_rate)
    size_label <- "Occupancy Rate"
  } else {
    min_val <- min(sf_stops$delay_min)
    max_val <- max(sf_stops$delay_min)
    size_label <- "Delay (minutes)"
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

    m <- m %>%
      addCircleMarkers(
        data = route_data,
        radius = ~ 6 + (get(alpha_var) - min_val) / (max_val - min_val) * 8,
        stroke = TRUE,
        color = "white",
        weight = 1.5,
        fillColor = route_color,
        fillOpacity = 0.85,
        popup = ~ paste0(
          "<strong>Stop ID:</strong> ", stop_id, "<br>",
          "<strong>Route:</strong> ", route_name, "<br>",
          "<strong>Occupancy Rate:</strong> ", round(occupancy_rate * 100, 1), "%<br>",
          "<strong>Delay:</strong> ", round(delay_min * 60, 0), " seconds"
        ),
        label = ~ paste0(
          "Stop: ", stop_id, " | ",
          "Route: ", route_name, " | ",
          "Occupancy: ", round(occupancy_rate * 100, 1), "% | ",
          "Delay: ", round(delay_min * 60, 0), "s"
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


back_button <- function() {
  actionButton("back_to_home", "← Back to Home", class = "btn-light mb-4")
}