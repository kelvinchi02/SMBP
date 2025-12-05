library(dotenv)
library(httr2)
library(data.table)
library(lubridate)

# ------------------------------------------------------------
# 1. Load .env only when running locally
# ------------------------------------------------------------
if (file.exists(".env")) {
  load_dot_env(".env")
}

supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_KEY")

# ------------------------------------------------------------
# 2. Safe conversion helper
# ------------------------------------------------------------
convert_safe <- function(col, type) {
  switch(type,
    "POSIXct"  = suppressWarnings(ymd_hms(col, tz = "UTC")),
    "Date"     = suppressWarnings(ymd(col)),
    "integer"  = suppressWarnings(as.integer(col)),
    "numeric"  = suppressWarnings(as.numeric(col)),
    "logical"  = tolower(as.character(col)) %in% c("true", "t", "1", "yes"),
    "character" = as.character(col),
    col
  )
}

# ------------------------------------------------------------
# 3. TYPE MAP
# ------------------------------------------------------------
type_map <- list(
  datetime = "POSIXct", service_date = "Date", route_id = "character", 
  route_name = "character", trip_id = "character", direction_id = "integer",
  direction_name = "character", stop_id = "character", stop_name = "character",
  stop_sequence = "integer", vehicle_id = "character", scheduled_arrival = "POSIXct",
  actual_arrival = "POSIXct", delay_min = "numeric", headway_min = "numeric",
  passengers_waiting = "integer", passengers_onboard = "integer", occupancy_rate = "numeric",
  lat = "numeric", lon = "numeric", weather_temp_c = "numeric", route_length_km = "numeric",
  crowding_level = "character", route_color = "character",
  # Schedule table columns
  scheduled_departure = "character", full_time = "POSIXct"
)

# ------------------------------------------------------------
# 4. LOAD SUPABASE TABLE (With CSV Fail-Safe)
# ------------------------------------------------------------
load_supabase_table <- function(table_name) {
  
  # Helper to create an empty table with correct schema
  create_empty_schema <- function() {
    dt <- data.table()
    for (col_name in names(type_map)) {
      dt[[col_name]] <- vector(mode = "logical", length = 0)
    }
    return(dt)
  }

  # --- ATTEMPT 1: API Connection ---
  if (supabase_url != "" && supabase_key != "") {
    tryCatch({
      endpoint <- paste0(supabase_url, "/rest/v1/", table_name, "?select=*")
      
      req <- request(endpoint) |>
        req_headers(
          "apikey" = supabase_key,
          "Authorization" = paste("Bearer", supabase_key)
        ) |>
        req_timeout(5) # Fast timeout to switch to CSV quickly
      
      resp <- req_perform(req)
      
      if (resp_status(resp) == 200) {
        raw <- resp_body_json(resp, simplifyVector = TRUE)
        dt <- as.data.table(raw)
        
        # Type conversion
        for (col in names(type_map)) {
          if (col %in% names(dt)) {
            dt[[col]] <- convert_safe(dt[[col]], type_map[[col]])
          }
        }
        return(dt)
      }
    }, error = function(e) {
      message(paste("[WARNING] API Failed for", table_name, ":", e$message))
      # Fall through to CSV
    })
  }
  
  # --- ATTEMPT 2: CSV Fail-Safe ---
  csv_file <- paste0(table_name, ".csv")
  # Handle special case for schedule table mapping
  if (table_name == "bus_schedule") csv_file <- "schedule.csv"
  
  if (file.exists(csv_file)) {
    message(paste("[SYSTEM] Switching to Offline Mode: Loading", csv_file))
    dt <- fread(csv_file)
    
    # Apply type conversions to CSV data
    for (col in names(type_map)) {
      if (col %in% names(dt)) {
        dt[[col]] <- convert_safe(dt[[col]], type_map[[col]])
      }
    }
    return(dt)
  }
  
  # --- FINAL FALLBACK: Empty Table ---
  message("[ERROR] No Data Source Available (API & CSV Failed)")
  return(create_empty_schema())
}

# ------------------------------------------------------------
# 5. TIMESTAMP CHECK
# ------------------------------------------------------------
get_latest_timestamp <- function(table_name) {
  
  # Try API First
  if (supabase_url != "" && supabase_key != "") {
    tryCatch({
      endpoint <- paste0(supabase_url, "/rest/v1/", table_name, "?select=datetime&order=datetime.desc&limit=1")
      req <- request(endpoint) |>
        req_headers("apikey" = supabase_key, "Authorization" = paste("Bearer", supabase_key)) |>
        req_timeout(2)
      
      resp <- req_perform(req)
      raw <- resp_body_json(resp, simplifyVector = TRUE)
      
      if (length(raw) > 0) return(raw$datetime[1])
      
    }, error = function(e) {
      # Ignore error, proceed to fallback
    })
  }
  
  # Fallback: Read local CSV to find last timestamp
  # This simulates "live" time by just returning the max time in the CSV
  if (file.exists("SmartTransit_Integrated.csv")) {
    dt <- fread("SmartTransit_Integrated.csv", select = "datetime")
    if (nrow(dt) > 0) {
      return(max(dt$datetime))
    }
  }
  
  return(Sys.time())
}