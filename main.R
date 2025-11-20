library(jsonlite)

load_github_repo <- function(branch = "main") {
  repo_api <- paste0(
    "https://api.github.com/repos/kelvinchi02/SMBP/contents?ref=",
    branch
  )

  files <- fromJSON(repo_api)

  r_files <- files[grepl("\\.R$", files$name), ]

  if (nrow(r_files) == 0) {
    stop("No .R files found in this branch.")
  }

  message("Loading R files from branch: ", branch)

  for (i in seq_len(nrow(r_files))) {
    raw_url <- r_files$download_url[i]
    message(" → ", r_files$name[i])
    source(raw_url, local = TRUE)
  }

  message("All R scripts loaded successfully.")
}


load_github_repo(branch = "main")


supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_KEY")

if (supabase_url == "" || supabase_key == "") {
  stop("Missing SUPABASE_URL or SUPABASE_KEY in your environment.")
}

#----------------------------------------------------------
# FUNCTION: Query entire table from Supabase
#-------------------------------- --------------------------

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
}

#----------------------------------------------------------
# Load SmartTransit_Integrated into "info"
#----------------------------------------------------------

info <- load_supabase_table("SmartTransit_Integrated")

# inspect result
print(info)