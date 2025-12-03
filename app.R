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
library(shinyjs) # Required for smooth page transitions

# Initialize Startup Timer
startup_start <- Sys.time()
message(paste("[SYSTEM] Application startup initiated at:", startup_start))

# -------------------------------------------------------------------------
# 1. SETUP & MODULE LOADING
# -------------------------------------------------------------------------

# Load Environment Variables
if (file.exists(".env")) {
  load_dot_env(".env")
}
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Source Modules with Performance Logging
source_module <- function(file) {
  t_start <- Sys.time()
  source(file, local = FALSE)
  duration <- round(difftime(Sys.time(), t_start, units = "secs"), 4)
  message(sprintf("[MODULE] Loaded %s - Duration: %s seconds", file, duration))
}

source_module("database_connection.R")
source_module("pre.R")         # Defines load_app_data() function
source_module("api_utils.R")   # Defines simulation logic
source_module("dashboard.R")   # Sidebar UI
source_module("login.R")       # Login UI
source_module("chat.R")        # Chat UI
source_module("overview.R")
source_module("map.R")
source_module("weather.R")
source_module("crowd.R")
source_module("ridership.R")
source_module("hour.R")

# Ensure image path exists to prevent crash
img_folder <- "www/index"
if (!dir.exists(img_folder)) dir.create(img_folder, recursive = TRUE)
addResourcePath("index", img_folder)

# -------------------------------------------------------------------------
# 2. MAIN UI SHELL
# -------------------------------------------------------------------------
ui <- page_fluid(
  style = "padding: 0; margin: 0;",
  useShinyjs(), # Initialize JavaScript utilities
  
  # Session Management Script
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
  
  # Main Content Router
  # We load the Login UI immediately. Dashboard is hidden until authentication.
  div(id = "login_page", login_ui()),
  hidden(div(id = "dashboard_page", uiOutput("app_content")))
)

message(sprintf("[SYSTEM] UI generation complete. Total startup time: %s seconds", round(difftime(Sys.time(), startup_start, units = "secs"), 2)))

# -------------------------------------------------------------------------
# 3. SERVER LOGIC
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  message("[SERVER] Client connected. Session started.")

  # --- 1. LAZY DATA LOADING ---
  # We use reactiveValues to store data. It starts as NULL.
  global <- reactiveValues(
    info = NULL, 
    routes = NULL, 
    stops = NULL, 
    sf_stops = NULL, 
    loaded = FALSE
  )
  
  # This observer runs immediately after the app starts.
  # It effectively moves the heavy processing out of the startup phase.
  observe({
    if (!global$loaded) {
      message("[SERVER] Starting background data loading...")
      t_load <- Sys.time()
      
      # Execute the function from pre.R
      data_objects <- load_app_data()
      
      # Assign to global reactive values
      global$info <- data_objects$info
      global$routes <- data_objects$routes
      global$stops <- data_objects$stops
      global$sf_stops <- data_objects$sf_stops
      global$loaded <- TRUE
      
      duration <- round(difftime(Sys.time(), t_load, units = "secs"), 2)
      message(sprintf("[SERVER] Background data loading complete. Duration: %s seconds", duration))
    }
  })

  # --- 2. AUTHENTICATION & NAVIGATION ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")

  # Login Button Logic
  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    
    if (authenticate_user(input$login_user, input$login_pass)) {
      authenticated(TRUE)
      user_info(list(name = input$login_user))
      
      # Switch UI safely
      shinyjs::hide("login_page")
      shinyjs::show("dashboard_page")
      
      session$sendCustomMessage("saveUserInfo", list(username = input$login_user, name = input$login_user))
    } else {
      insertUI(
        selector = "#login_error", where = "afterBegin",
        ui = div(style = "color:red; margin-top:10px; font-weight:bold;", "Invalid username or password.")
      )
    }
  })

  # Auto-login from LocalStorage
  observeEvent(input$restore_session, {
    tryCatch({
      user_data <- jsonlite::fromJSON(input$restore_session)
      if (!is.null(user_data$username)) {
        authenticated(TRUE)
        user_info(user_data)
        shinyjs::hide("login_page")
        shinyjs::show("dashboard_page")
      }
    }, error = function(e) { session$sendCustomMessage("clearStorage", list()) })
  })

  # Logout Logic
  observeEvent(input$logout_btn, {
    authenticated(FALSE)
    user_info(NULL)
    current_view("dashboard")
    session$sendCustomMessage("clearStorage", list())
    shinyjs::hide("dashboard_page")
    shinyjs::show("login_page")
  })

  # Navigation Handling
  observeEvent(input$nav_selection, { current_view(input$nav_selection) })
  observeEvent(input$back_to_home, { current_view("dashboard") })

  # --- 3. DYNAMIC UI RENDERING ---
  
  # Topbar Title
  output$topbar_title_dynamic <- renderUI({
    req(authenticated())
    title <- switch(current_view(),
      "dashboard" = "Smart Bus Management Platform",
      "overview" = "Overview Dashboard",
      "delay" = "Delay & Punctuality Analysis",
      "ridership" = "Ridership Trends",
      "crowding" = "Crowding Analysis",
      "weather" = "Weather Impact",
      "map" = "Stops & Map",
      "Smart Bus Management Platform"
    )
    div(class = "topbar-title", title)
  })

  # Main Content Switcher
  output$app_content <- renderUI({
    tagList(
      dashboard_ui(user_name = user_info()$name),
      chat_ui()
    )
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

  # --- 4. MODULE SERVER LOGIC (Connecting UI to Data) ---
  
  # CHATBOT
  observeEvent(input$chat_message, {
    resp <- call_chatgpt(input$chat_message$text, OPENAI_API_KEY)
    session$sendCustomMessage("chat_response", resp)
  })

  # OVERVIEW MODULE
  # Note: Plots now wait for global$info to be loaded
  output$summaryOutputPlot1 <- renderPlot({
    req(global$info, global$routes)
    ggplot(global$info, aes(y = factor(route_id), x = occupancy_rate, color = route_id)) +
      geom_boxplot() + theme_bw() + guides(color="none")
  })
  
  output$summaryOutputPlot2 <- renderPlot({
    req(global$info)
    ggplot(global$info, aes(x = route_id, y = delay_min)) +
      geom_jitter(alpha=0.1) + theme_bw()
  })
  
  output$summaryOutputPlot3 <- renderPlotly({
    req(global$info, input$summaryplot3whatRoute)
    create_crowding_pie(global$info, global$routes, input$summaryplot3whatRoute)
  })
  
  # DYNAMIC METRICS (Overview)
  output$dynamic_metrics <- renderUI({
    req(global$info)
    total <- format(nrow(global$info), big.mark = ",")
    div(class="metric-value", total)
  })

  # MAP MODULE
  output$mapPlotOut <- renderLeaflet({
    req(global$sf_stops) # Wait for SF calculation
    makemap(global$sf_stops, input$whatMapalpha)
  })
  
  observeEvent(input$refresh_live_map, {
    showNotification("Live vehicle simulation refreshed.", type="message")
  })
  
  observeEvent(input$get_stop_insight, {
    data <- get_ai_stop_summary()
    output$ai_stop_display <- renderUI({
      div(class="ai-suggestion", HTML(paste0("<strong>AI Analysis:</strong> ", data$analysis)))
    })
  })

  # WEATHER MODULE
  # Note: Uses global$info for live calculations
  observeEvent(input$refresh_weather, { 
    req(global$info)
    # Trigger UI update here if needed using get_weather_kpis()
  })
  
  output$wea_hc_output <- renderHighchart({ req(global$info); create_weather_polar_chart() })
  output$wea_gg_output1 <- renderPlot({ req(global$info); create_weather_ridge_plot() })
  output$wea_gg_output2 <- renderPlot({ req(global$info); create_weather_jitter_plot() })

  # CROWDING MODULE
  output$crowd_ggplot1 <- renderPlotly({ req(global$info); create_weather_polar_chart() }) 
  output$crowd_ggplot2 <- renderPlot({ 
    req(global$info)
    ggplot(global$info, aes(x = stop_id, y = N, fill = crowding_level)) + geom_col() 
  })

  # RIDERSHIP MODULE
  output$trendPlot <- renderPlot({ req(global$info); ride.plot2 })
  output$dailyMap <- renderPlotly({
    req(global$info, input$ride_date_select)
    p <- ride.plot1(input$ride_date_select)
    if(is.null(p)) return(NULL)
    ggplotly(p) |> hide_legend()
  })

  # DELAY MODULE
  output$hour_delay_plot1 <- renderPlot({ req(global$info); hour_plot1 })
  output$hour_delay_plot2 <- renderPlot({ req(global$info); hour_plot2(input$hourwhatRoute) })
  
  observeEvent(input$get_ai_insight, {
    data <- generate_ai_delay_summary()
    output$ai_insight_display <- renderUI({
      div(class="ai-suggestion", HTML(paste0("<strong>AI Analysis:</strong> ", data)))
    })
  })
}

# Run the application
runApp(shinyApp(ui, server), port = 6120, host = "0.0.0.0")