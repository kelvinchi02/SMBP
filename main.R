library(jsonlite)

# Load Github Repo R Scripts
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

load_github_repo("main")
#----------------------------------------------------------
# check requirements and install missing packages
req_url <- "https://raw.githubusercontent.com/kelvinchi02/SMBP/main/requirements.txt"
reqs <- readLines(req_url)
reqs <- trimws(reqs)
reqs <- reqs[reqs != ""]
install.packages(reqs)



info <- load_supabase_table("SmartTransit_Integrated")

print(dim(info))