install.packages(readLines("requirements.txt"))

source("main_function.R")
source("database_connection.R")
load_github_repo(branch = "main")






info <- load_supabase_table("SmartTransit_Integrated")

