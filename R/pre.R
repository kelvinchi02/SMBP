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


message("[STEP 1] Initiating database connection and data retrieval...")
t1_start <- Sys.time()

# Load data from Supabase using your specific table name. load_supabase_table 
# now handles failed connections gracefully by returning an empty data.table.
info <- load_supabase_table("SmartTransit_Integrated")

t1_duration <- round(difftime(Sys.time(), t1_start, units = "secs"), 2)
message(sprintf("[STEP 1 COMPLETED] Data retrieval successful. Rows: %d | Columns: %d. Duration: %s seconds.", nrow(info), ncol(info), t1_duration))

# -------------------------------------------------------------------------
# 2. DATA PROCESSING
# -------------------------------------------------------------------------
message("[STEP 2] Processing timestamps and categorical factors...")
t2_start <- Sys.time()

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Check if data exists before processing
if (nrow(info) > 0) {
  
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
  
} # If data is empty, 'info' remains as returned by load_supabase_table

t2_duration <- round(difftime(Sys.time(), t2_start, units = "secs"), 2)
message(sprintf("[STEP 2 COMPLETED] Data processing finished. Duration: %s seconds.", t2_duration))

# -------------------------------------------------------------------------
# 3. AGGREGATES & METADATA (CRITICAL SAFETY CHECK ADDED HERE)
# -------------------------------------------------------------------------
message("[STEP 3] Aggregating route and stop statistics...")
t3_start <- Sys.time()

if (nrow(info) > 0) {
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
} else {
    # Initialize empty global variables to prevent downstream crashes
    stops <- data.table()
    routes <- data.table(route_id=character(0), route_color=character(0), route_name=character(0))
}

t3_duration <- round(difftime(Sys.time(), t3_start, units = "secs"), 2)
message(sprintf("[STEP 3 COMPLETED] Aggregation finished. Duration: %s seconds.", t3_duration))

# -------------------------------------------------------------------------
# 4. SPATIAL CONVERSION (CRITICAL SAFETY CHECK ADDED HERE)
# -------------------------------------------------------------------------
message("[STEP 4] Converting coordinates to Simple Features (sf) object...")
t4_start <- Sys.time()

if (nrow(stops) > 0) {
    # Create spatial object (Required by map.R)
    sf_stops <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4326)
} else {
    # Initialize empty spatial object
    sf_stops <- st_sf(st_sfc()) 
}

t4_duration <- round(difftime(Sys.time(), t4_start, units = "secs"), 2)
message(sprintf("[STEP 4 COMPLETED] Spatial conversion finished. Duration: %s seconds.", t4_duration))

# -------------------------------------------------------------------------
# 5. STYLES & UI HELPERS
# -------------------------------------------------------------------------

delay_colors <- c("Delayed" = "#E74C3C", "Early" = "#2ECC71", "On-time" = "#3498DB")

# Dropdown choices generator
if (nrow(routes) > 0) {
    choices_from_data <- routes$route_id
    names(choices_from_data) <- routes$route_name
    styles_from_data <- sprintf("color: %s; font-weight: bold;", routes$route_color)
} else {
    choices_from_data <- c("N/A" = "N/A")
    styles_from_data <- "color: gray;"
}

final_choices <- c("ALL" = "ALL", choices_from_data)
final_styles <- c("color: black; font-weight: bold;", styles_from_data)

# Helper for the back button (Consolidated to one definition)
back_button <- function() {
  actionButton("back_to_home", "← Back to Home", class = "btn-light mb-4")
}

# The create_crowding_pie function is already defined in the full pre.R provided previously

total_duration <- round(difftime(Sys.time(), global_start_time, units = "secs"), 2)
message(sprintf("[SYSTEM] Pre-processing initialization fully completed. Total time: %s seconds.", total_duration))