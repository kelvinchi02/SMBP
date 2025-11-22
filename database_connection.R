library(dotenv)
library(httr2)

supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_KEY")


if (supabase_url == "" || supabase_key == "") {
  stop("Missing SUPABASE_URL or SUPABASE_KEY in your environment.")
}


load_supabase_table <- function(table_name) {
  
  # Build REST endpoint (Supabase uses PostgREST)
  endpoint <- paste0(supabase_url, "/rest/v1/", table_name, "?select=*")
  
  # Build the request
  req <- request(endpoint) |>
    req_headers(
      "apikey" = supabase_key,
      "Authorization" = paste("Bearer", supabase_key)
    )
  
  # Perform request
  resp <- req_perform(req)
  
  # Parse JSON → data.table
  data <- resp_body_json(resp)
  as.data.table(data)

  print("Columns loaded from Supabase:")
  print(colnames(data))
}