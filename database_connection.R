library(dotenv)
library(httr2)
library(data.table)

if (file.exists(".env")) {
  load_dot_env(".env")
}

supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_KEY")

if (supabase_url == "" || supabase_key == "") {
  stop("Missing SUPABASE_URL or SUPABASE_KEY in your environment.")
}

load_supabase_table <- function(table_name) {

  endpoint <- paste0(supabase_url, "/rest/v1/", table_name, "?select=*")

  req <- request(endpoint) |>
    req_headers(
      "apikey" = supabase_key,
      "Authorization" = paste("Bearer", supabase_key)
    )

  resp <- req_perform(req)

  raw <- resp_body_json(resp, check_type = FALSE)

  # Convert list of rows → proper data.table
  dt <- rbindlist(raw, fill = TRUE)

  message("Columns loaded from Supabase:")
  print(colnames(dt))

  return(dt)
}
