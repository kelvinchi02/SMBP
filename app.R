library(shiny)
library(bslib)
library(bsicons)
library(fontawesome)
library(htmltools)
library(shinyWidgets)
library(leaflet)
library(sf)
library(dplyr)
library(dotenv)
library(data.table)
library(plotly)
library(highcharter)
library(ggridges)

# Initialize Startup Timer
startup_start <- Sys.time()
message(paste("[SYSTEM] Application startup initiated at:", startup_start))

# -------------------------------------------------------------------------
# 1. SETUP & MODULE LOADING
# -------------------------------------------------------------------------

if (file.exists(".env")) load_dot_env(".env")
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Source Modules
source("database_connection.R")
source("pre.R")  # Loads initial static data for immediate display
source("api_utils.R")
source("dashboard.R")
source("login.R")
source("chat.R")
source("overview.R")
source("map.R")
source("weather.R")
source("crowd.R")
source("ridership.R")
source("hour.R")

# Source the separated Server logic (where the reactive poll lives)
source("server.R")

# Ensure image path exists and register it
if (dir.exists("www/index")) {
  addResourcePath("index", "www/index")
}

# -------------------------------------------------------------------------
# 2. MAIN UI SHELL
# -------------------------------------------------------------------------
ui <- page_fluid(
  style = "padding: 0; margin: 0;",
  
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var userInfo = localStorage.getItem('bus_user_info');
        if (userInfo) {
          Shiny.setInputValue('restore_session', userInfo, {priority: 'event'});
        }
      });
      Shiny.addCustomMessageHandler('saveUserInfo', function(data) {
        localStorage.setItem('bus_user_info', JSON.stringify(data));
      });
      Shiny.addCustomMessageHandler('clearStorage', function(data) {
        localStorage.removeItem('bus_user_info');
      });
    "))
  ),
  
  # The Root UI Router
  uiOutput("root_ui")
)

message(sprintf("[SYSTEM] UI generation complete. Total startup time: %s seconds", round(difftime(Sys.time(), startup_start, units = "secs"), 2)))

# -------------------------------------------------------------------------
# 3. RUN APPLICATION
# -------------------------------------------------------------------------
shinyApp(ui, server)