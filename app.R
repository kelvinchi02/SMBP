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
# pre.R runs data loading IMMEDIATELY here. 
# It creates global objects: 'info', 'routes', 'stops', 'sf_stops'
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

# Ensure image path exists and register it
# (Assuming you renamed the folder to 'www' lowercase as discussed)
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
# 3. SERVER LOGIC
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  message("[SERVER] Client connected. Session started.")

  # --- 1. DATA STATE ---
  # We use the GLOBAL 'info' object loaded by pre.R
  # For real-time updates, we store results in reactiveVals
  realtime_delay <- reactiveVal(NULL)
  
  # --- 2. AUTHENTICATION & ROUTING ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")

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

  # Login Logic
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    # 1. Get the full result list
    auth_result <- authenticate_user(input$login_username, input$login_password)
    
    # 2. Check success boolean
    if (auth_result$success) {
      authenticated(TRUE)
      user_info(list(name = input$login_username))
      session$sendCustomMessage("saveUserInfo", list(username = input$login_username, name = input$login_username))
    } else {
      insertUI(selector = "#login_error", where = "afterBegin",
               ui = div(style = "color:red; margin-top:10px; font-weight:bold;", auth_result$message))
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
  
  # Chat
  observeEvent(input$chat_message, {
    resp <- call_chatgpt(input$chat_message$text, OPENAI_API_KEY)
    session$sendCustomMessage("chat_response", resp)
  })

  # Overview
  output$summaryOutputPlot1 <- renderPlot({ ggplot(info, aes(y = factor(route_id), x = occupancy_rate, color = route_id)) + geom_boxplot() + theme_bw() + guides(color="none") })
  output$summaryOutputPlot2 <- renderPlot({ ggplot(info, aes(x = route_id, y = delay_min)) + geom_jitter(alpha=0.1) + theme_bw() })
  output$summaryOutputPlot3 <- renderPlotly({ req(input$summaryplot3whatRoute); create_crowding_pie(info, routes, input$summaryplot3whatRoute) })

  # Map
  output$mapPlotOut <- renderLeaflet({ 
    # Use global sf_stops from pre.R, pass explicitly to fix "unused argument" error
    makemap(sf_stops, input$whatMapalpha) 
  })
  
  # Other Plots
  output$trendPlot <- renderPlot({ ride.plot2 })
  output$dailyMap <- renderPlotly({ p <- ride.plot1(input$ride_date_select); if(is.null(p)) return(NULL); ggplotly(p) %>% hide_legend() })
  
  output$wea_hc_output <- renderHighchart({ create_weather_polar_chart() })
  output$wea_gg_output1 <- renderPlot({ create_weather_ridge_plot() })
  output$wea_gg_output2 <- renderPlot({ create_weather_jitter_plot() })
  
  # --- CROWDING PLOT FIX ---
  output$crowd_ggplot1 <- renderPlotly({ create_weather_polar_chart() }) # Using polar chart as per your setup
  
  output$crowd_ggplot2 <- renderPlot({ 
    # FIX: We must aggregate the data here to create the 'N' column
    # The previous code failed because info has no 'N' column
    plot_data <- info[, .N, .(route_id, stop_id, crowding_level)]
    
    ggplot(plot_data, aes(x = stop_id, y = N, fill = crowding_level)) + 
      geom_col(position="fill") + 
      theme_minimal() +
      scale_fill_manual(values = c('Low'='#2ca02c', 'Medium'='#ff7f0e', 'High'='#d62728')) +
      labs(y = "Proportion", title = "Crowding Level by Stop") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Delay
  output$hour_delay_plot1 <- renderPlot({ hour_plot1 })
  output$hour_delay_plot2 <- renderPlot({ hour_plot2(input$hourwhatRoute) })
  
  # API Buttons
  observeEvent(input$refresh_live_map, { showNotification("Live vehicle data refreshed (Simulation)", type="message") })
  
  observeEvent(input$get_ai_insight, { 
    data <- generate_ai_delay_summary()
    output$ai_insight_display <- renderUI({ div(class="ai-suggestion", HTML(paste0("<strong>AI:</strong> ", data))) })
  })
  
  observeEvent(input$get_stop_insight, {
    data <- get_ai_stop_summary()
    output$ai_stop_display <- renderUI({ div(class="ai-suggestion", HTML(paste0("<strong>AI:</strong> ", data$analysis))) })
  })
  
  observeEvent(input$get_weather_insight, {
    data <- generate_ai_weather_summary()
    output$ai_weather_display <- renderUI({ div(class="ai-suggestion", HTML(paste0("<strong>AI:</strong> ", data))) })
  })
  
  observeEvent(input$get_crowding_insight, {
    data <- generate_ai_crowding_summary()
    output$ai_crowding_display <- renderUI({ div(class="ai-suggestion", HTML(paste0("<strong>AI:</strong> ", data))) })
  })
  
  observeEvent(input$get_ridership_insight, {
    data <- generate_ai_ridership_summary()
    output$ai_ridership_display <- renderUI({ div(class="ai-suggestion", HTML(paste0("<strong>AI:</strong> ", data))) })
  })
}

# -------------------------------------------------------------------------
# CRITICAL: Return the app object (Do NOT use port=...)
# -------------------------------------------------------------------------
shinyApp(ui, server)