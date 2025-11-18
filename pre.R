library(data.table)
library(plotly)
library(ggrepel)
library(ggplot2)
library(sf)
library(scales)
library(lubridate)


##step0: data process
# Ensure English month names regardless of system locale
old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

fread('SmartTransit_Integrated.csv')->info

# Data processing
info[, hour := hour(scheduled_arrival)]
info[, hour := factor(hour, levels = sort(unique(hour)))]
info[, delay_category := fcase(
  delay_min > 0, "Delayed",
  delay_min < 0, "Early",
  delay_min == 0, "On-time"
)]

stops<-info[,.(lon=mean(lon),lat=mean(lat),occupancy_rate=mean(occupancy_rate)),.(stop_id,route_id,route_name,route_long_name,route_color)][order(stop_id)]
routes<-unique(stops[,.(route_id,route_color,route_name)])
routes<-routes[info[,.N,.(route_id,route_length_km)],on='route_id'][,!'N'][order(route_id)]
sf_stops <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4326)
delay_colors <- c("Delayed" = "#E74C3C", "Early" = "#2ECC71", "On-time" = "#3498DB") # Define custom colors for delay categories

choices_from_data <- routes$route_id
names(choices_from_data) <- routes$route_name

styles_from_data <- sprintf(
  "color: %s; font-weight: bold;", 
  routes$route_color
)

final_choices <- c(
  "ALL" = "ALL", 
  choices_from_data
)

final_styles <- c(
  "color: black; font-weight: bold;",
  styles_from_data
)

#####summary1
summary.plot1<-ggplot(
  info,
  aes(y = factor(route_id, levels = routes$route_id), x = occupancy_rate, color = route_id)
) +
  # Add boxplot with improved aesthetics and transparent fill
  geom_boxplot(
    aes(fill = after_scale(alpha(color, 0.3))),  # Set fill to transparent version of color
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.7,
    width = 0.6
  ) +
  
  # Add custom route colors
  scale_color_manual(
    values = setNames(routes[, route_color], routes[, route_id]),
    labels = setNames(routes[, route_name], routes[, route_id]),
    name = "Transit Route"
  ) +
  
  # Format occupancy rate as percentage
  scale_x_continuous(
    name = "Occupancy Rate",
    labels = percent_format(),
    limits = function(x) c(max(0, min(x) - 0.05), min(1, max(x) + 0.05))
  ) +
  
  # Create custom y-axis labels with route lengths
  scale_y_discrete(
    name = "Route",
    labels = sprintf(
      "%s",
      routes$route_id
    )
  ) +
  
  # Add reference line at 100% capacity
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  
  # Add informative labels
  labs(
    title = "Occupancy Rate Distribution by Transit Route",
    subtitle = "Comparing passenger load across different routes",
    caption = "Dashed red line indicates 100% capacity"
  ) +
  
  # Enhanced theme
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

###summary 2
# Create improved scatter plot of delays by route
summary.plot2<-ggplot(info, aes(x = route_id, y = delay_min)) +
  # Add reference line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Add jittered points
  geom_point(
    aes(color = delay_category),
    position = position_jitter(width = 0.2, height = 0),
    alpha = 0.4,
    size = 2
  ) +
  
  # Add mean points with error bars
  stat_summary(
    fun = mean,
    fun.min = function(x) mean(x) - sd(x),
    fun.max = function(x) mean(x) + sd(x),
    geom = "pointrange",
    size = 0.8,
    color = "black"
  ) +
  
  # Apply custom colors for delay categories
  scale_color_manual(
    values = delay_colors,
    name = "Delay Status"
  ) +
  
  # Custom x-axis with route information
  scale_x_discrete(
    name = "Transit Route",
    labels = setNames(
      sprintf("%s", routes$route_name),
      routes$route_id
    )
  ) +
  
  # Add informative labels
  labs(
    title = "Transit Delays of Routes",
    subtitle = "Distribution of delays across different routes\n with mean values (black dots) and standard deviation (error bars)",
    y = "Delay (minutes)",
    caption = "Negative values indicate early arrivals, positive values indicate delays"
  ) +
  
  # Enhance theme
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )
  
##
##summary 3
# Function to create a crowding level pie chart for a specific route or all routes
create_crowding_pie <- function(data, routes_info, route_idi = "ALL") {
  
  # Define consistent colors for crowding levels - define outside to ensure consistency
  crowding_colors <- c(
    "Low" = "#2ECC71",     # Green for low crowding
    "Medium" = "#F39C12",  # Orange for medium crowding
    "High" = "#E74C3C"     # Red for high crowding
  )
  
  # Extract crowding level counts
  if (route_idi == "ALL") {
    # For all routes combined
    plot_data <- data[, .(N = sum(N)), by = crowding_level]
    main_title <- "All Routes Combined"
    title_color <- "#333333"  # Default dark gray
  } else {
    # Filter data for this specific route
    plot_data <- data[route_id == route_idi]
    
    # Extract route information
    route_index <- which(routes_info$route_id == route_idi)
    route_name <- routes_info$route_name[route_index]
    route_length <- routes_info$route_length_km[route_index]
    route_color <- routes_info$route_color[route_index]
    
    # Set title and color
    main_title <- paste0("Route ", route_idi, ": ", route_name, " (", route_length, " km)")
    title_color <- route_color
  }
  
  # Calculate total for percentages
  total <- sum(plot_data$N)
  
  # Make sure plot_data has crowding levels in a consistent order
  all_levels <- c("Low", "Medium", "High")
  plot_data <- plot_data[match(all_levels, crowding_level)]
  
  # Create the pie chart - explicitly mapping colors to ensure consistency
  fig <- plot_ly(
    plot_data,
    labels = ~crowding_level,
    values = ~N,
    type = "pie",
    marker = list(colors = crowding_colors[plot_data$crowding_level]),
    textinfo = "label+percent",
    hoverinfo = "text+percent",
    text = ~paste(
      "Crowding:", crowding_level,
      "<br>Count:", N,
      "<br>Percentage:", sprintf("%4.3f%%", 100*N/total)
    )
  )
  
  # Apply layout with title
  fig <- fig %>% layout(
    title = list(
      text = main_title,
      font = list(color = title_color)
    ),
    legend = list(title = list(text = "Crowding Level")),
    margin = list(t = 100, b = 80, l = 50, r = 50)
  )
  
  return(fig)
}

