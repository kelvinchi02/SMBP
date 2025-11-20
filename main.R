source("main_finction.R")
source("mainfunction.R")
load_github_repo(branch = "main")


supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_KEY")

if (supabase_url == "" || supabase_key == "") {
  stop("Missing SUPABASE_URL or SUPABASE_KEY in your environment.")
}


info <- load_supabase_table("SmartTransit_Integrated")

