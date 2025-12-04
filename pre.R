library(data.table)
library(plotly)
library(ggrepel)
library(ggplot2)
library(sf)
library(scales)
library(lubridate)
library(shiny)

# Initialize System Timer
global_start_time <- Sys.time()
message(paste("[SYSTEM] Initialization started at:", global_start_time))

# -------------------------------------------------------------------------
# 1. DATABASE CONNECTION
# -------------------------------------------------------------------------
source("database_connection.R")

message("[STEP 1] Initiating database connection and data retrieval...")
t1_start <- Sys.time()

# Load data from Supabase using your specific table name
info <- load_supabase_table("SmartTransit_Integrated")

t1_duration <- round(difftime(Sys.time(), t1_start, units = "secs"), 2)
message(sprintf("[STEP 1 COMPLETED] Data retrieval successful. Rows: %d | Columns: %d. Duration: %s seconds.", nrow(info), ncol(info), t1_duration))

# -------------------------------------------------------------------------
# 2. DATA PROCESSING
# -------------------------------------------------------------------------
message("[STEP 2] Processing timestamps and categorical factors...")
t2_start <- Sys.time()

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Ensure date/time columns are correct types (Robustness check)
if(inherits(info$scheduled_arrival, "character")) {
  info[, scheduled_arrival := ymd_hms(scheduled_arrival)]
}

# Extract hour for analysis
info[, hour := hour(scheduled_arrival)]
info[, hour := factor(hour, levels = sort(unique(hour)))]

# Define delay categories
info[, delay_category := fcase(
  delay_min > 0, "Delayed",
  delay_min < 0, "Early",
  delay_min == 0, "On-time"
)]

t2_duration <- round(difftime(Sys.time(), t2_start, units = "secs"), 2)
message(sprintf("[STEP 2 COMPLETED] Data processing finished. Duration: %s seconds.", t2_duration))

# -------------------------------------------------------------------------
# 3. AGGREGATES & METADATA
# -------------------------------------------------------------------------
message("[STEP 3] Aggregating route and stop statistics...")
t3_start <- Sys.time()

# Create unique stops reference
stops <- info[, .(
  lon = mean(lon),
  lat = mean(lat),
  occupancy_rate = mean(occupancy_rate),
  delay_min = mean(delay_min)
), .(stop_id, route_id, route_name, route_long_name, route_color)][order(stop_id)]

# Create unique routes reference
routes <- unique(stops[, .(route_id, route_color, route_name)])
routes <- routes[info[, .N, .(route_id, route_length_km)], on = "route_id"][, !"N"][order(route_id)]

t3_duration <- round(difftime(Sys.time(), t3_start, units = "secs"), 2)
message(sprintf("[STEP 3 COMPLETED] Aggregation finished. Duration: %s seconds.", t3_duration))

# -------------------------------------------------------------------------
# 4. SPATIAL CONVERSION (Performance Critical)
# -------------------------------------------------------------------------
message("[STEP 4] Converting coordinates to Simple Features (sf) object...")
t4_start <- Sys.time()

# Create spatial object (Required by map.R)
sf_stops <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4326)

t4_duration <- round(difftime(Sys.time(), t4_start, units = "secs"), 2)
message(sprintf("[STEP 4 COMPLETED] Spatial conversion finished. Duration: %s seconds.", t4_duration))

# -------------------------------------------------------------------------
# 5. STYLES & UI HELPERS
# -------------------------------------------------------------------------

delay_colors <- c("Delayed" = "#E74C3C", "Early" = "#2ECC71", "On-time" = "#3498DB")

# Dropdown choices generator
choices_from_data <- routes$route_id
names(choices_from_data) <- routes$route_name

styles_from_data <- sprintf("color: %s; font-weight: bold;", routes$route_color)

final_choices <- c("ALL" = "ALL", choices_from_data)
final_styles <- c("color: black; font-weight: bold;", styles_from_data)

# Helper for the back button (Consolidated to one definition)
back_button <- function() {
  actionButton("back_to_home", "← Back to Home", class = "btn-light mb-4")
}

# -------------------------------------------------------------------------
# 6. STATIC PLOTS (Only keeping function definitions if needed)
# -------------------------------------------------------------------------
# NOTE: Removed static plot objects (summary.plot1, summary.plot2) as they 
# are now generated dynamically in the server.
message("[STEP 5] Skipped static plot generation (Now handled dynamically).")

create_crowding_pie <- function(data, routes_info, route_idi = "ALL") {
  crowding_colors <- c("Low" = "#2ECC71", "Medium" = "#F39C12", "High" = "#E74C3C")

  if (route_idi == "ALL") {
    # If using 'info' which is defined globally here, we must re-create N/crowding_level
    if (!"crowding_level" %in% names(data)) {
      data[, crowding_level := data.table::fcase(
        occupancy_rate < 0.5, "Low",
        occupancy_rate < 0.85, "Medium",
        default = "High"
      )]
    }
    plot_data <- data[, .(N = .N), by = crowding_level]
    main_title <- "All Routes Combined"
    title_color <- "#333333"
  } else {
    plot_data <- data[route_id == route_idi]
    
    if (!"crowding_level" %in% names(plot_data)) {
      plot_data[, crowding_level := data.table::fcase(
        occupancy_rate < 0.5, "Low",
        occupancy_rate < 0.85, "Medium",
        default = "High"
      )]
    }
    
    plot_data <- plot_data[, .(N = .N), by = crowding_level]
    
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

total_duration <- round(difftime(Sys.time(), global_start_time, units = "secs"), 2)
message(sprintf("[SYSTEM] Pre-processing initialization fully completed. Total time: %s seconds.", total_duration))