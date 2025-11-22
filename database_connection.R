library(dotenv)
library(httr2)
library(data.table)
library(lubridate)

# ------------------------------------------------------------
# 1. Load .env only when running locally (Posit Cloud won't use this)
# ------------------------------------------------------------
if (file.exists(".env")) {
  load_dot_env(".env")
}

supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_KEY")

if (supabase_url == "" || supabase_key == "") {
  stop("Missing SUPABASE_URL or SUPABASE_KEY in your environment.")
}

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
# 3. TRUE datatype mapping (from your CSV structure)
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
  passengers_onboard           = "integer",   # 🔥 important fix
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
# 4. Main Supabase fetch function
# ------------------------------------------------------------

load_supabase_table <- function(table_name) {

  endpoint <- paste0(supabase_url, "/rest/v1/", table_name, "?select=*")

  req <- request(endpoint) |>
    req_headers(
      "apikey" = supabase_key,
      "Authorization" = paste("Bearer", supabase_key)
    )

  resp <- req_perform(req)

  raw <- resp_body_json(resp, simplifyVector = TRUE)

  dt <- as.data.table(raw)

  # Debug: print real column names
  print("Columns loaded from Supabase:")
  print(colnames(dt))

  # Apply type conversion where columns match the map
  for (col in names(type_map)) {
    if (col %in% names(dt)) {
      dt[[col]] <- convert_safe(dt[[col]], type_map[[col]])
    }
  }

  return(dt)
}
