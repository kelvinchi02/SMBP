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
# 2. Safe conversion helper (remains the same)
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
# 3. TRUE datatype mapping (remains the same)
# ------------------------------------------------------------

type_map <- list(
  datetime                     = "POSIXct",
  service_date                 = "Date",
  route_id                     = "character",
  route_name                   = "character",
  trip_id                      = "character",
  direction_id                 = "integer",
  direction_name               = "character",
  stop_id                      = "character",
  stop_name                    = "character",
  stop_sequence                = "integer",
  vehicle_id                   = "character",

  scheduled_arrival            = "POSIXct",
  actual_arrival               = "POSIXct",

  delay_min                    = "numeric",
  headway_min                  = "numeric",
  passengers_waiting           = "integer",
  passengers_onboard           = "integer",   
  occupancy_rate               = "numeric",

  peak                         = "character",
  trip_start_time              = "character",
  trip_end_time                = "character",

  trip_vehicle_id              = "character",
  vehicle_model                = "character",
  vehicle_capacity             = "integer",
  vehicle_wheelchair_capacity  = "integer",
  vehicle_bike_rack            = "logical",

  stop_zone                    = "character",
  route_long_name              = "character",
  route_length_km              = "numeric",
  route_color                  = "character",

  weather_temp_c               = "numeric",
  weather_precip_mm            = "numeric",
  weather_hourly_conditions    = "character"
)

# ------------------------------------------------------------
# 4. Main Supabase fetch function (CRITICAL CHANGE)
# ------------------------------------------------------------

load_supabase_table <- function(table_name) {
  # Perform environment check ONLY when the data is actually needed
  if (supabase_url == "" || supabase_key == "") {
    message("[ERROR] Supabase credentials missing. Returning empty data table.")
    # Return an empty data table with correct columns to prevent further errors
    empty_df <- data.table(
        datetime=as.POSIXct(character(0)), service_date=as.Date(character(0)), 
        route_id=character(0), delay_min=numeric(0), hour=factor(character(0), levels=NULL)
    )
    # Add other critical columns needed for pre.R to function
    for (col in names(type_map)) {
        if (!col %in% names(empty_df)) {
             empty_df[, (col) := vector(mode(type_map[[col]]), 0)]
        }
    }
    return(empty_df)
  }
  
  # API call logic inside tryCatch to handle transient network errors
  tryCatch({
    endpoint <- paste0(supabase_url, "/rest/v1/", table_name, "?select=*")

    req <- request(endpoint) |>
      req_headers(
        "apikey" = supabase_key,
        "Authorization" = paste("Bearer", supabase_key)
      )

    resp <- req_perform(req)

    raw <- resp_body_json(resp, simplifyVector = TRUE)

    dt <- as.data.table(raw)

    # Apply type conversion where columns match the map
    for (col in names(type_map)) {
      if (col %in% names(dt)) {
        dt[[col]] <- convert_safe(dt[[col]], type_map[[col]])
      }
    }

    return(dt)
  }, error = function(e) {
    message(paste("[ERROR] Failed to fetch data from Supabase:", e$message))
    return(data.table()) # Return empty table on API failure
  })
}


# ------------------------------------------------------------
# 5. Lightweight Check Function (CRITICAL CHANGE)
# ------------------------------------------------------------

get_latest_timestamp <- function(table_name) {
  if (supabase_url == "" || supabase_key == "") {
    return(Sys.time()) # Return current time so reactivePoll doesn't crash
  }
  
  tryCatch({
    # Logic: Select 'datetime', Order by 'datetime' DESC, Limit to 1 row.
    endpoint <- paste0(supabase_url, "/rest/v1/", table_name,
                       "?select=datetime&order=datetime.desc&limit=1")

    req <- request(endpoint) |>
      req_headers(
        "apikey" = supabase_key,
        "Authorization" = paste("Bearer", supabase_key)
      )

    resp <- req_perform(req)
    raw <- resp_body_json(resp, simplifyVector = TRUE)

    # Return the specific timestamp string or current time on empty result
    if (length(raw) == 0 || nrow(raw) == 0) {
      return(Sys.time())
    }
    return(raw$datetime[1])
  }, error = function(e) {
    message(paste("[ERROR] Lightweight check failed:", e$message))
    return(Sys.time()) # Return current time on failure
  })
}