library(data.table)
library(lubridate)
library(scales)

# -------------------------------------------------------------------------
# API UTILITIES (Simulation Mode)
# -------------------------------------------------------------------------

# ... [Keep your existing Sections 1-6 unchanged] ...
# (I am including the full file below for completeness to avoid copy-paste errors)

# --- 1. DELAY & PUNCTUALITY ---

get_realtime_kpis <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list(punctuality=0, avg_delay=0, active_issues=0))
  
  total_trips <- nrow(data)
  on_time <- nrow(data[delay_category == "On-time"])
  
  delayed_trips <- data[delay_min > 0]
  if (nrow(delayed_trips) > 0) {
    avg_delay_val <- mean(delayed_trips$delay_min, na.rm = TRUE)
  } else {
    avg_delay_val <- 0
  }
  
  active_issues <- nrow(data[delay_min > 5])
  
  list(
    punctuality = round((on_time / total_trips) * 100, 1),
    avg_delay = round(avg_delay_val, 1),
    active_issues = active_issues
  )
}

get_worst_stops <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  worst <- data[, .(avg_delay = mean(delay_min, na.rm=TRUE)), by = .(stop_name, stop_id)]
  worst <- worst[order(-avg_delay)]
  head(worst, 5) 
}

generate_ai_delay_summary <- function() {
  return("AI Analysis: Traffic congestion on the Downtown Loop is causing 15% higher delays than historical averages. Recommendation: Adjust schedule headway by +2 minutes between 8:00 AM and 9:00 AM.")
}

# --- 2. RIDERSHIP ---

get_ridership_kpis <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list(total_passengers="0", avg_occupancy="0%", status="Unknown", status_class="normal"))

  total_pax <- sum(data$passengers_onboard, na.rm = TRUE)
  avg_occ <- mean(data$occupancy_rate, na.rm = TRUE)
  
  status <- data.table::fcase(
    avg_occ < 0.5, "Normal",
    avg_occ < 0.85, "Busy",
    default = "Overcrowded"
  )
  
  status_class <- data.table::fcase(
    status == "Normal", "normal",
    status == "Busy", "busy",
    default = "overcrowded"
  )
  
  list(
    total_passengers = format(total_pax, big.mark = ","),
    avg_occupancy = scales::percent(avg_occ, accuracy = 1),
    status = status,
    status_class = status_class
  )
}

generate_ai_ridership_summary <- function() {
  return("AI Analysis: Ridership trends indicate a 12% surge on Route B during evening peak hours. Capacity utilization is reaching 92%. Recommendation: Deploy one additional vehicle to Route B starting at 17:00.")
}

add_ridership_trip <- function(route_id, current_headway, current_ridership, extra_trips) {
  new_headway <- round(current_headway * 0.8, 1) 
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
  if (is.null(data) || nrow(data) == 0) return(list(avg_occupancy="0%", risk_zones=0, low_count=0, med_count=0, high_count=0, status="Unknown"))

  avg_occ <- mean(data$occupancy_rate, na.rm = TRUE)
  risk_zones <- nrow(data[occupancy_rate > 0.85])
  
  breakdown <- data[, .N, by = crowding_level]
  
  get_count <- function(lvl) {
    val <- breakdown[crowding_level == lvl, N]
    if(length(val) == 0) return(0) else return(val)
  }
  
  # Determine status text (for consistency)
  status <- data.table::fcase(
    avg_occ < 0.5, "Normal",
    avg_occ < 0.85, "Busy",
    default = "Overcrowded"
  )
  
  list(
    avg_occupancy = scales::percent(avg_occ, accuracy = 1),
    risk_zones = risk_zones,
    low_count = get_count("Low"),
    med_count = get_count("Medium"),
    high_count = get_count("High"),
    status = status
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
    new_occupancy = avg_occupancy * 0.9, 
    occupancy_reduction = 0.1,
    old_wait_min = round(current_headway/2, 1),
    new_wait_min = round((current_headway * 0.85)/2, 1)
  )
}

# --- 4. WEATHER ---

get_current_weather <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      current = list(condition = "Unknown", temperature = 0, humidity = 0, wind_speed = 0),
      forecast = list()
    ))
  }

  latest <- tail(data, 1)
  
  list(
    current = list(
      condition = latest$weather_hourly_conditions,
      temperature = latest$weather_temp_c,
      humidity = 65, 
      wind_speed = 12 
    ),
    forecast = list(
      list(time = "Now", condition = latest$weather_hourly_conditions, temperature = latest$weather_temp_c, precipitation_prob = 10),
      list(time = "+1h", condition = "Cloudy", temperature = latest$weather_temp_c - 1, precipitation_prob = 20),
      list(time = "+2h", condition = "Rain", temperature = latest$weather_temp_c - 2, precipitation_prob = 80)
    )
  )
}

get_weather_impact <- function(hour) {
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
  if (is.null(stops_data) || nrow(stops_data) == 0) return(list(vehicles = list()))
  
  route_stops <- stops_data[route_id == selected_route_id]
  
  if(nrow(route_stops) > 0) {
    idx <- sample(1:nrow(route_stops), min(3, nrow(route_stops)))
    active_buses <- route_stops[idx]
    
    vehicles <- lapply(1:nrow(active_buses), function(i) {
      stop_data <- active_buses[i]
      
      list(
        vehicle_id = paste0("BUS-", selected_route_id, "-", i),
        status = sample(c("On Time", "Delayed", "Early"), 1),
        speed_kmh = sample(20:50, 1),
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

# --- 7. LIVE DATA SERIALIZATION FOR AI (NEW) ---

get_live_kpi_summary <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return("No current operational data available for analysis.")
  }
  
  # Calculate required KPIs
  realtime_kpis <- get_realtime_kpis(data)
  crowding_kpis <- get_crowding_kpis(data)
  ridership_kpis <- get_ridership_kpis(data) 
  
  # Identify the current worst route for delays and crowding
  delay_summary <- data[, .(avg_delay = mean(delay_min, na.rm=TRUE)), by = route_id][order(-avg_delay)][1]
  crowding_summary <- data[, .(avg_occ = mean(occupancy_rate, na.rm=TRUE)), by = route_id][order(-avg_occ)][1]
  
  # Get current weather condition from the latest record
  latest_record <- tail(data, 1)
  current_weather <- paste0(
    latest_record$weather_hourly_conditions[1], 
    " at ", 
    round(latest_record$weather_temp_c[1], 1), "°C"
  )
  
  # --- Assemble the required comprehensive prompt string ---
  summary_string <- paste0(
    "SYSTEM HEALTH: Punctuality=", realtime_kpis$punctuality, "%; AvgDelay=", realtime_kpis$avg_delay, " min. ",
    "WorstDelayRoute: ", delay_summary$route_id, 
    " (AvgDelay: ", round(delay_summary$avg_delay, 1), " min). ",
    
    "RIDERSHIP & CAPACITY: AvgOccupancy=", crowding_kpis$avg_occupancy, ". ",
    "CurrentLoadStatus=", ridership_kpis$status, ". ", 
    "WorstCrowdingRoute: ", crowding_summary$route_id, 
    " (AvgOcc: ", scales::percent(crowding_summary$avg_occ, accuracy = 1), "). ",
    
    "CROWDING BREAKDOWN (Trips): Low=", crowding_kpis$low_count, "; ",
    "Medium=", crowding_kpis$med_count, "; ",
    "High=", crowding_kpis$high_count, "; ",
    "OverloadRiskZones (>85%): ", crowding_kpis$risk_zones, ". ",
    
    "ENVIRONMENT: Weather: ", current_weather, "."
  )
  
  return(summary_string)
}



# --- 8. SCHEDULE MANAGEMENT (NEW) ---

get_next_scheduled_trip <- function(route_id_target, simulation_time) {
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_key <- Sys.getenv("SUPABASE_KEY")
  
  if (supabase_url == "" || supabase_key == "") return(NULL)
  
  # Format simulation time for query: HH:MM:SS
  sim_time_str <- format(simulation_time, "%H:%M:%S")
  
  # Query: route_id = target AND scheduled_departure > sim_time
  endpoint <- paste0(supabase_url, "/rest/v1/bus_schedule",
                     "?route_id=eq.", route_id_target,
                     "&scheduled_departure=gt.", sim_time_str,
                     "&order=scheduled_departure.asc&limit=1")
  
  tryCatch({
    req <- request(endpoint) |>
      req_headers(apikey = supabase_key, Authorization = paste("Bearer", supabase_key))
    
    resp <- req_perform(req)
    result <- resp_body_json(resp, simplifyVector = TRUE)
    
    if (length(result) > 0) {
      return(list(
        next_departure = result$scheduled_departure[1],
        headway = result$headway_min[1]
      ))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

calculate_smart_insert_time <- function(next_sched, current_simulation_time) {
  # S (Next Schedule)
  s_time <- as.POSIXct(paste(Sys.Date(), next_sched$next_departure), format="%Y-%m-%d %H:%M:%S")
  
  # S-1 (Prev Schedule) = S - Headway
  headway_sec <- next_sched$headway * 60
  
  # Calculate Midpoint (Insertion Point) = S - (Headway / 2)
  half_gap <- headway_sec / 2
  insert_time <- s_time - half_gap
  
  # Robustness: If calculated insertion time is already passed in the simulation,
  # we suggest inserting in the NEXT interval (S + Headway/2).
  if (insert_time < current_simulation_time) {
     insert_time <- s_time + half_gap 
  }
  
  return(format(insert_time, "%H:%M:%S"))
}

# --- 9. AI ROUTE PROFILING (NEW) ---

get_route_crowding_profile <- function(live_data, route_id_target) {
  route_data <- live_data[route_id == route_id_target]
  
  if (nrow(route_data) == 0) {
    return(NULL)
  }
  
  # 1. Determine Simulation Time (Latest update in data)
  sim_time <- max(route_data$actual_arrival, na.rm = TRUE)
  
  # 2. Metrics (Last 2 records for trend)
  recent_data <- tail(route_data[order(actual_arrival)], 2)
  
  avg_occ <- mean(recent_data$occupancy_rate, na.rm = TRUE)
  max_waiting <- max(recent_data$passengers_waiting, na.rm = TRUE)
  avg_delay <- mean(recent_data$delay_min, na.rm = TRUE)
  
  # 3. Schedule Context
  next_schedule <- get_next_scheduled_trip(route_id_target, sim_time)
  schedule_info <- "No schedule data."
  suggested_insert <- "N/A"
  
  if (!is.null(next_schedule)) {
    suggested_insert <- calculate_smart_insert_time(next_schedule, sim_time)
    schedule_info <- paste0("Next Bus: ", next_schedule$next_departure, " (Headway: ", next_schedule$headway, "m)")
  }
  
  # 4. Construct Profile
  profile <- paste0(
    "ROUTE: ", route_id_target, "\n",
    "TIME: ", format(sim_time, "%H:%M:%S"), "\n",
    "METRICS: Occupancy=", scales::percent(avg_occ, 1), ", MaxWaiting=", max_waiting, ", Delay=", round(avg_delay, 1), "m.\n",
    "SCHEDULE: ", schedule_info, "\n",
    "POTENTIAL INSERT: ", suggested_insert, "\n",
    "TASK: If Occupancy > 85% OR MaxWaiting > 5, recommend YES. Else NO."
  )
  
  return(list(profile_text = profile, suggested_time = suggested_insert))
}