check_and_install_packages <- function(req_file = "requirements.txt") {
  # Read list of packages
  packages <- readLines(req_file)
  
  # Get installed packages
  installed <- rownames(installed.packages())
  
  # Check and install missing ones
  for (pkg in packages) {
    if (!(pkg %in% installed)) {
      message(paste("Installing", pkg, "..."))
      install.packages(pkg)
    } else {
      message(paste(pkg, "is already installed."))
    }
  }
  
  message("All required packages are installed and up to date.")
}
