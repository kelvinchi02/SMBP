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
source("pre.R")
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

# -------------------------------------------------------------------------
# 2. MAIN UI SHELL
# -------------------------------------------------------------------------

# Ensure image path exists and register it
img_folder <- "www/index"
if (!dir.exists(img_folder)) dir.create(img_folder, recursive = TRUE)
addResourcePath("index", img_folder)

# We use a simple uiOutput that defaults to the Login Page. 
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
# 3. SERVER LOGIC
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  message("[SERVER] Client connected. Session started.")

  # --- 1. DATA LOADING (Lazy - Background) ---
  global <- reactiveValues(info = NULL, routes = NULL, stops = NULL, sf_stops = NULL, loaded = FALSE)
  
  observe({
    if (!global$loaded) {
      message("[SERVER] Starting background data loading...")
      data_objects <- load_app_data()
      global$info <- data_objects$info
      global$routes <- data_objects$routes
      global$stops <- data_objects$stops
      global$sf_stops <- data_objects$sf_stops
      global$loaded <- TRUE
      message("[SERVER] Background data loading complete.")
    }
  })

  # --- 2. AUTHENTICATION & ROUTING ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")

  # THE MAIN ROUTER
  output$root_ui <- renderUI({
    if (!authenticated()) {
      login_ui()
    } else {
      tagList(
        dashboard_ui(user_name = user_info()$name),
        chat_ui()
      )
    }
  })

  # Login Button Logic
  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    if (authenticate_user(input$login_user, input$login_pass)) {
      authenticated(TRUE)
      user_info(list(name = input$login_user))
      session$sendCustomMessage("saveUserInfo", list(username = input$login_user, name = input$login_user))
    } else {
      insertUI(selector = "#login_error", where = "afterBegin",
               ui = div(style = "color:red; margin-top:10px; font-weight:bold;", "Invalid username or password."))
    }
  })

  # Auto-login
  observeEvent(input$restore_session, {
    tryCatch({
      user_data <- jsonlite::fromJSON(input$restore_session)
      if (!is.null(user_data$username)) {
        authenticated(TRUE)
        user_info(user_data)
      }
    }, error = function(e) { session$sendCustomMessage("clearStorage", list()) })
  })

  # Logout
  observeEvent(input$logout_btn, {
    authenticated(FALSE)
    user_info(NULL)
    current_view("dashboard")
    session$sendCustomMessage("clearStorage", list())
  })

  # Navigation
  observeEvent(input$nav_selection, { current_view(input$nav_selection) })
  observeEvent(input$back_to_home, { current_view("dashboard") })

  # --- 3. DASHBOARD CONTENT ---
  output$topbar_title_dynamic <- renderUI({
    req(authenticated())
    div(class = "topbar-title", "Smart Bus Management Platform")
  })

  output$dashboard_content <- renderUI({
    req(authenticated())
    switch(current_view(),
      "dashboard" = dashboard_home_content(),
      "overview" = overview_ui(),
      "delay" = delay_ui(),
      "ridership" = rider_ui(),
      "crowding" = crowd_ui(),
      "weather" = weather_ui(),
      "map" = map_ui()
    )
  })

  # --- 4. MODULE SERVER LOGIC ---
  
  # CHAT
  observeEvent(input$chat_message, {
    resp <- call_chatgpt(input$chat_message$text, OPENAI_API_KEY)
    session$sendCustomMessage("chat_response", resp)
  })

  # OVERVIEW PLOTS
  output$summaryOutputPlot1 <- renderPlot({ req(global$info); ggplot(global$info, aes(y = factor(route_id), x = occupancy_rate, color = route_id)) + geom_boxplot() + theme_bw() + guides(color="none") })
  output$summaryOutputPlot2 <- renderPlot({ req(global$info); ggplot(global$info, aes(x = route_id, y = delay_min)) + geom_jitter(alpha=0.1) + theme_bw() })
  output$summaryOutputPlot3 <- renderPlotly({ req(global$info, input$summaryplot3whatRoute); create_crowding_pie(global$info, global$routes, input$summaryplot3whatRoute) })

  # MAP
  output$mapPlotOut <- renderLeaflet({
    req(global$sf_stops)
    makemap(global$sf_stops, input$whatMapalpha)
  })
  
  # OTHER PLOTS
  output$trendPlot <- renderPlot({ req(global$info); ride.plot2 })
  output$dailyMap <- renderPlotly({ req(global$info); p <- ride.plot1(input$ride_date_select); if(is.null(p)) return(NULL); ggplotly(p) %>% hide_legend() })
  output$wea_hc_output <- renderHighchart({ req(global$info); create_weather_polar_chart() })
  output$wea_gg_output1 <- renderPlot({ req(global$info); create_weather_ridge_plot() })
  output$wea_gg_output2 <- renderPlot({ req(global$info); create_weather_jitter_plot() })
  output$crowd_ggplot1 <- renderPlotly({ req(global$info); create_weather_polar_chart() }) 
  output$crowd_ggplot2 <- renderPlot({ req(global$info); ggplot(global$info, aes(x = stop_id, y = N, fill = crowding_level)) + geom_col(position="fill") })
  output$hour_delay_plot1 <- renderPlot({ req(global$info); hour_plot1 })
  output$hour_delay_plot2 <- renderPlot({ req(global$info); hour_plot2(input$hourwhatRoute) })
  
  # API Buttons
  observeEvent(input$refresh_live_map, { showNotification("Live vehicle data refreshed (Simulation)", type="message") })
  observeEvent(input$get_ai_insight, { 
    data <- generate_ai_delay_summary()
    output$ai_insight_display <- renderUI({ div(class="ai-suggestion", HTML(paste0("<strong>AI:</strong> ", data))) })
  })
}

shinyApp(ui, server)