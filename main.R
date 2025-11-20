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

install.packages(readLines("requirements.txt"))



source("database_connection.R")

info <- load_supabase_table("SmartTransit_Integrated")

head(info)