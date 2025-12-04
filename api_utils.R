library(data.table)
library(lubridate)
library(scales)

# -------------------------------------------------------------------------
# API UTILITIES (Simulation Mode)
# -------------------------------------------------------------------------
# Refactored to accept 'data' dynamically for auto-refresh compatibility.
# -------------------------------------------------------------------------

# --- 1. DELAY & PUNCTUALITY ---

get_realtime_kpis <- function(data) {
  # Calculate metrics from the live dataset
  if (is.null(data) || nrow(data) == 0) return(list(punctuality=0, avg_delay=0, active_issues=0))
  
  total_trips <- nrow(data)
  on_time <- nrow(data[delay_category == "On-time"])
  
  # Protect against NaN if no delayed trips exist
  delayed_trips <- data[delay_min > 0]
  if (nrow(delayed_trips) > 0) {
    avg_delay_val <- mean(delayed_trips$delay_min, na.rm = TRUE)
  } else {
    avg_delay_val <- 0
  }
  
  active_issues <- nrow(data[delay_min > 5]) # Arbitrary threshold for "issue"
  
  list(
    punctuality = round((on_time / total_trips) * 100, 1),
    avg_delay = round(avg_delay_val, 1),
    active_issues = active_issues
  )
}

get_worst_stops <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  # Aggregate delays by stop
  worst <- data[, .(avg_delay = mean(delay_min, na.rm=TRUE)), by = .(stop_name, stop_id)]
  worst <- worst[order(-avg_delay)]
  head(worst, 5) # Return top 5
}

generate_ai_delay_summary <- function() {
  # Placeholder for Hugging Face integration
  return("AI Analysis: Traffic congestion on the Downtown Loop is causing 15% higher delays than historical averages. Recommendation: Adjust schedule headway by +2 minutes between 8:00 AM and 9:00 AM.")
}

# --- 2. RIDERSHIP ---

get_ridership_kpis <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list(total_passengers="0", avg_occupancy="0%", status="Unknown", status_class="normal"))

  total_pax <- sum(data$passengers_onboard, na.rm = TRUE)
  avg_occ <- mean(data$occupancy_rate, na.rm = TRUE)
  
  # Determine status text
  status <- fcase(
    avg_occ < 0.5, "Normal",
    avg_occ < 0.85, "Busy",
    default = "Overcrowded"
  )
  
  status_class <- fcase(
    status == "Normal", "normal",
    status == "Busy", "busy",
    default = "overcrowded"
  )
  
  list(
    total_passengers = format(total_pax, big.mark = ","),
    avg_occupancy = percent(avg_occ, accuracy = 1),
    status = status,
    status_class = status_class
  )
}

generate_ai_ridership_summary <- function() {
  return("AI Analysis: Ridership trends indicate a 12% surge on Route B during evening peak hours. Capacity utilization is reaching 92%. Recommendation: Deploy one additional vehicle to Route B starting at 17:00.")
}

add_ridership_trip <- function(route_id, current_headway, current_ridership, extra_trips) {
  # Simulate the math of adding a trip
  new_headway <- round(current_headway * 0.8, 1) # Reduce headway by 20%
  new_wait <- round(new_headway / 2, 1)
  
  list(
    error = NULL,
    new_headway = new_headway,
    extra_trips = extra_trips + 1,
    message = "Trip dispatched successfully.",
    ridership = current_ridership,
    old_wait_min = round(current_headway/2, 1),
    new_wait_min = new_wait
  )
}

# --- 3. CROWDING ---

get_crowding_kpis <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list(avg_occupancy="0%", risk_zones=0, low_count=0, med_count=0, high_count=0))

  avg_occ <- mean(data$occupancy_rate, na.rm = TRUE)
  risk_zones <- nrow(data[occupancy_rate > 0.85])
  
  # Ensure all levels exist in aggregation to avoid errors if one level is missing
  breakdown <- data[, .N, by = crowding_level]
  
  # Safe extraction helper
  get_count <- function(lvl) {
    val <- breakdown[crowding_level == lvl, N]
    if(length(val) == 0) return(0) else return(val)
  }
  
  list(
    avg_occupancy = percent(avg_occ, accuracy = 1),
    risk_zones = risk_zones,
    low_count = get_count("Low"),
    med_count = get_count("Medium"),
    high_count = get_count("High")
  )
}

generate_ai_crowding_summary <- function() {
  return("AI Analysis: High crowding detected at 'University Gate' stop. Pattern suggests students leaving classes. Recommendation: Short-turn a bus at Stop 105 to relieve pressure.")
}

add_crowding_trip <- function(route_id, current_headway, avg_occupancy, extra_trips) {
  list(
    error = NULL,
    new_headway = round(current_headway * 0.85, 1),
    extra_trips = extra_trips + 1,
    message = "Capacity injection successful.",
    old_occupancy = avg_occupancy,
    new_occupancy = avg_occupancy * 0.9, # Simulate 10% reduction
    occupancy_reduction = 0.1,
    old_wait_min = round(current_headway/2, 1),
    new_wait_min = round((current_headway * 0.85)/2, 1)
  )
}

# --- 4. WEATHER ---

get_current_weather <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    # Fallback if no data
    return(list(
      current = list(condition = "Unknown", temperature = 0, humidity = 0, wind_speed = 0),
      forecast = list()
    ))
  }

  # Simulate current weather based on the last row of data
  latest <- tail(data, 1)
  
  list(
    current = list(
      condition = latest$weather_hourly_conditions,
      temperature = latest$weather_temp_c,
      humidity = 65, # Mock value
      wind_speed = 12 # Mock value
    ),
    forecast = list(
      list(time = "Now", condition = latest$weather_hourly_conditions, temperature = latest$weather_temp_c, precipitation_prob = 10),
      list(time = "+1h", condition = "Cloudy", temperature = latest$weather_temp_c - 1, precipitation_prob = 20),
      list(time = "+2h", condition = "Rain", temperature = latest$weather_temp_c - 2, precipitation_prob = 80)
    )
  )
}

get_weather_impact <- function(hour) {
  # Return dummy data for the impact chart (Static simulation)
  list(
    impacts = data.frame(
      condition = c("Clear", "Cloudy", "Rain"),
      avg_delay_min = c(0.5, 1.2, 3.5),
      ridership_change_pct = c(0, -5, -15),
      avg_occupancy = c(0.8, 0.75, 0.6)
    )
  )
}

get_weather_kpis <- function(data) {
  # Wrapper for get_current_weather to match app.R expectations
  w <- get_current_weather(data)
  list(
    condition = w$current$condition,
    temperature = paste0(w$current$temperature, "°C")
  )
}

generate_ai_weather_summary <- function() {
  return("AI Analysis: Rain predicted in 2 hours. Historical data suggests a 3.5 min increase in delays. Recommendation: Issue driver advisory for wet road conditions.")
}

get_ai_weather_advice <- function(current_weather, forecast_conditions, avg_delay, avg_occupancy, headway, extra_trips) {
  # Logic to generate the structured advice object for weather.R
  list(
    analysis = paste("Current conditions:", current_weather, ". Forecast suggests rain."),
    pred_wait_min = round(headway/2 + 2, 1),
    risk_level = "medium",
    future_outlook = "Rain likely.",
    suggestion = "Prepare for slower speeds.",
    peak_recommendations = list("Increase headway buffer", "Notify passengers")
  )
}

# --- 5. MAP (LIVE VEHICLES) ---

get_live_location <- function(stops_data, selected_route_id) {
  # 1. Safety check
  if (is.null(stops_data) || nrow(stops_data) == 0) return(list(vehicles = list()))
  
  # 2. Filter stops for the selected route
  route_stops <- stops_data[route_id == selected_route_id]
  
  # 3. Simulation Logic
  # If we have stops, pick 3 random ones to place "buses" near
  if(nrow(route_stops) > 0) {
    idx <- sample(1:nrow(route_stops), min(3, nrow(route_stops)))
    active_buses <- route_stops[idx]
    
    vehicles <- lapply(1:nrow(active_buses), function(i) {
      stop_data <- active_buses[i]
      
      list(
        vehicle_id = paste0("BUS-", selected_route_id, "-", i),
        status = sample(c("On Time", "Delayed", "Early"), 1),
        speed_kmh = sample(20:50, 1),
        # PURE MATH, NO AI: Add random "jitter" (runif) to lat/lon
        lat = stop_data$lat + runif(1, -0.002, 0.002), 
        lon = stop_data$lon + runif(1, -0.002, 0.002), 
        next_stop = stop_data$stop_name,
        eta_next_stop_min = sample(1:10, 1),
        occupancy = runif(1, 0.2, 0.9)
      )
    })
  } else {
    vehicles <- list()
  }
  
  return(list(vehicles = vehicles))
}

# --- 6. STOP ANALYSIS ---

get_ai_stop_summary <- function(stops_data = NULL, headway_changes = NULL) {
  # Returns a static analysis list for the map AI button
  list(
    analysis = "Stop utilization is uneven. Stop 103 is a bottleneck.",
    avg_wait_min = 12,
    avg_occupancy = 0.8,
    severity = "moderate",
    bottleneck_stops = list("Stop 103", "Stop 108"),
    infrastructure_suggestions = list("Expand shelter at Stop 103", "Add digital signage"),
    route_suggestions = list("Review timing points")
  )
}