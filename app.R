message(">>> THIS IS THE ACTIVE app.R <<<")
message(">>> WORKING DIRECTORY: ", getwd())
message(">>> FILES HERE: ", paste(list.files(), collapse = ", "))

list.files(recursive = TRUE)

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

# Load environment variables (critical for Supabase)
safe_source <- function(f) {
  message(">>> Sourcing: ", f)
  tryCatch(
    source(f),
    error = function(e) {
      message(">>> ERROR in ", f, ": ", e$message)
      stop(e)
    }
  )
}

files <- c(
  "database_connection.R","pre.R","api_utils.R","dashboard.R","login.R",
  "chat.R","overview.R","map.R","weather.R","crowd.R","ridership.R","hour.R"
)

for (f in files) safe_source(f)



# Ensure image path exists and register it
if (dir.exists("www/index")) {
  addResourcePath("index", "www/index")
}

# -------------------------------------------------------------------------
# 2. MAIN UI SHELL (Global Definition)
# -------------------------------------------------------------------------



server <- function(input, output, session) {
  
  message("[SERVER] Client connected. Session started.")
  
  # -----------------------------------------------------------------------
  # 0. AUTO-REFRESH LOGIC (Reactive Polling)
  # -----------------------------------------------------------------------
  
  # This reactive reader polls the database to check for updates.
  live_info <- reactivePoll(
    intervalMillis = 5000,  # Check every 5 seconds
    session = session,
    
    # Check Function: Fast lightweight query
    checkFunc = function() {
      # Checks the latest timestamp to see if data has changed
      get_latest_timestamp("SmartTransit_Integrated")
    },
    
    # Value Function: Heavy download + DATA CLEANING
    valueFunc = function() {
      message("[SYSTEM] New data detected. Refreshing dataset...")
      
      # 1. Load raw data from Supabase
      df <- load_supabase_table("SmartTransit_Integrated")
      
      # 2. APPLY PRE.R CLEANING LOGIC 
      # (This is critical: without this, the app crashes on refresh because columns are missing)
      
      # A. Timestamp Conversion
      if(inherits(df$scheduled_arrival, "character")) {
        df[, scheduled_arrival := lubridate::ymd_hms(scheduled_arrival)]
      }
      
      # B. Create 'hour' column for analysis
      df[, hour := lubridate::hour(scheduled_arrival)]
      df[, hour := factor(hour, levels = sort(unique(hour)))]
      
      # C. Create 'delay_category'
      df[, delay_category := data.table::fcase(
        delay_min > 0, "Delayed",
        delay_min < 0, "Early",
        delay_min == 0, "On-time"
      )]
      
      return(df)
    }
  )

  # --- 1. AUTHENTICATION & ROUTING ---
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
    auth_result <- authenticate_user(input$login_username, input$login_password)
    
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

  # -----------------------------------------------------------------------
  # OVERVIEW PAGE (Now Connected to Live Data)
  # -----------------------------------------------------------------------
  output$summaryOutputPlot1 <- renderPlot({ 
    current_data <- live_info()
    req(current_data, exists("routes")) 
    create_occupancy_box(current_data, routes)
  })
  
  output$summaryOutputPlot2 <- renderPlot({ 
    current_data <- live_info()
    req(current_data, exists("routes")) 
    create_delay_jitter(current_data, routes)
  })
  
  output$summaryOutputPlot3 <- renderPlotly({ 
    req(input$summaryplot3whatRoute) 
    current_data <- live_info()
    req(current_data, exists("routes"))
    create_crowding_pie(current_data, routes, input$summaryplot3whatRoute) 
  })

  # -----------------------------------------------------------------------
  # MAP PAGE
  # -----------------------------------------------------------------------
  output$mapPlotOut <- renderLeaflet({ 
    req(exists("sf_stops")) 
    makemap(sf_stops, input$whatMapalpha, input$map_route_select) 
  })
  
  # -----------------------------------------------------------------------
  # RIDERSHIP PAGE (Plots and KPIs)
  # -----------------------------------------------------------------------
  
  # Real-time KPIs
  output$ridership_kpis <- renderUI({
    current_data <- live_info()
    req(current_data)
    
    kpis <- get_ridership_kpis(current_data)
    
    div(class = "kpi-grid",
      div(class = "kpi-card", 
          div(class = "kpi-label", "Total Passengers"), 
          div(class = "kpi-value", kpis$total_passengers)),
      div(class = "kpi-card", 
          div(class = "kpi-label", "Average Occupancy"), 
          div(class = "kpi-value", kpis$avg_occupancy)),
      div(class = "kpi-card", 
          div(class = "kpi-label", "Current Load Status"), 
          div(class = paste("kpi-value", kpis$status_class), kpis$status))
    )
  })
  
  # Plot 2: Trend Plot (Bar Chart)
  output$trendPlot <- renderPlot({ 
    req(live_info(), exists("routes"))
    create_ride_trend_bar(live_info(), routes)
  })
  
  # Plot 1: Daily Map Plot (Time Series)
  output$dailyMap <- renderPlotly({ 
    req(input$ride_date_select, live_info(), exists("routes"))
    p <- create_ride_plot1(live_info(), input$ride_date_select, routes)
    if(is.null(p)) return(NULL)
    ggplotly(p) %>% hide_legend() 
  })
  
  # -----------------------------------------------------------------------
  # CROWDING PAGE (Plots and KPIs)
  # -----------------------------------------------------------------------
  
  # Crowding Breakdown
  output$crowding_breakdown_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    
    # We must calculate crowding_level here to make it available for the API call
    dt_local <- as.data.table(current_data)
    dt_local[, crowding_level := fcase(
         occupancy_rate < 0.5, "Low",
         occupancy_rate < 0.85, "Medium",
         default = "High"
    )]
    
    kpis <- get_crowding_kpis(dt_local) 
    
    div(class = "breakdown-grid",
      div(class = "breakdown-item", 
          div(class = "breakdown-label", "Low Crowding Trips"), 
          div(class = "breakdown-value low", kpis$low_count)),
      div(class = "breakdown-item", 
          div(class = "breakdown-label", "Medium Crowding Trips"), 
          div(class = "breakdown-value medium", kpis$med_count)),
      div(class = "breakdown-item", 
          div(class = "breakdown-label", "High Crowding Trips"), 
          div(class = "breakdown-value high", kpis$high_count))
    )
  })
  
  # Crowding KPIs
  output$crowding_kpis <- renderUI({
    current_data <- live_info()
    req(current_data)

    # Note: crowding_level is calculated in the breakdown UI output above, but get_crowding_kpis recalculates
    kpis <- get_crowding_kpis(current_data)
    
    # Simple check for class styling based on average occupancy text from get_crowding_kpis
    avg_occ_text <- kpis$avg_occupancy
    occ_class <- if (grepl("^0\\.[0-4]", avg_occ_text)) "normal" else if (grepl("^0\\.[5-8]", avg_occ_text)) "busy" else "critical"
    
    div(class = "kpi-grid",
      div(class = "kpi-card", 
          div(class = "kpi-label", "Average Occupancy"), 
          div(class = paste("kpi-value", occ_class), kpis$avg_occupancy)),
      div(class = "kpi-card", 
          div(class = "kpi-label", "Overload Risk Zones (>85%)"), 
          div(class = "kpi-value critical", kpis$risk_zones))
    )
  })
  
  # 1. Map Plot
  output$crowd_ggplot1 <- renderPlotly({ 
    req(live_info())
    create_crowd_map(live_info()) 
  }) 
  
  # 2. Bar Chart
  output$crowd_ggplot2 <- renderPlot({ 
    req(live_info())
    create_crowd_bar(live_info()) 
  })
  
  # -----------------------------------------------------------------------
  # DELAY PAGE (Plots and KPIs)
  # -----------------------------------------------------------------------
  
  # Real-time KPIs
  output$realtime_kpis <- renderUI({
    current_data <- live_info()
    req(current_data)
    
    kpis <- get_realtime_kpis(current_data)
    
    # Determine class for styling 
    punctuality_class <- fcase(kpis$punctuality < 75, "bad", kpis$punctuality < 90, "warning", default = "good")
    delay_class <- fcase(kpis$avg_delay > 2, "bad", kpis$avg_delay > 1, "warning", default = "good")
    
    div(class = "kpi-grid",
      div(class = "kpi-card", 
          div(class = "kpi-label", "Punctuality Rate"), 
          div(class = paste("kpi-value", punctuality_class), paste0(kpis$punctuality, "%"))),
      div(class = "kpi-card", 
          div(class = "kpi-label", "Avg. Delay (min)"), 
          div(class = paste("kpi-value", delay_class), kpis$avg_delay)),
      div(class = "kpi-card", 
          div(class = "kpi-label", "Active Issues"), 
          div(class = "kpi-value warning", kpis$active_issues))
    )
  })

  # Worst Stops Display
  output$worst_stops_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    
    worst_stops_dt <- get_worst_stops(current_data)
    if (is.null(worst_stops_dt) || nrow(worst_stops_dt) == 0) return(NULL)
    
    div(class = "worst-stops",
      div(class = "worst-stops-title", "Top 5 Delay Hotspots"),
      lapply(1:nrow(worst_stops_dt), function(i) {
        row <- worst_stops_dt[i]
        div(class = "stop-item", 
            span(row$stop_name), 
            span(paste(round(row$avg_delay, 1), "min")))
      })
    )
  })
  
  output$hour_delay_plot1 <- renderPlot({ 
    req(live_info(), exists("routes"))
    create_hour_plot1(live_info(), routes)
  })
  
  output$hour_delay_plot2 <- renderPlot({ 
    req(live_info(), input$hourwhatRoute)
    create_hour_plot2(live_info(), input$hourwhatRoute)
  })
  
  # -----------------------------------------------------------------------
  # WEATHER PAGE (Plots and KPIs)
  # -----------------------------------------------------------------------
  
  # Current Weather KPIs
  output$weather_current_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    
    weather_data <- get_current_weather(current_data)
    current <- weather_data$current
    
    div(class = "weather-current-grid",
      div(class = "weather-kpi",
          div(class = "weather-kpi-label", "Current Condition"),
          div(class = "weather-kpi-value", current$condition)),
      div(class = "weather-kpi",
          div(class = "weather-kpi-label", "Temperature"),
          div(class = "weather-kpi-value", paste0(current$temperature, "°C"))),
      div(class = "weather-kpi",
          div(class = "weather-kpi-label", "Humidity"),
          div(class = "weather-kpi-value", paste0(current$humidity, "%"))),
      div(class = "weather-kpi",
          div(class = "weather-kpi-label", "Wind Speed"),
          div(class = "weather-kpi-value", paste0(current$wind_speed, " km/h")))
    )
  })

  # Weather Forecast Display
  output$weather_forecast_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    
    weather_data <- get_current_weather(current_data)
    forecast <- weather_data$forecast
    
    div(class = "forecast-grid",
      lapply(forecast, function(item) {
        div(class = "forecast-item",
            div(class = "forecast-time", item$time),
            div(class = "forecast-temp", paste0(item$temperature, "°C")),
            div(class = "forecast-condition", item$condition))
      })
    )
  })
  
  output$wea_hc_output <- renderHighchart({ 
    req(live_info())
    create_weather_polar_chart(live_info()) 
  })
  
  output$wea_gg_output1 <- renderPlot({ 
    req(live_info())
    create_weather_ridge_plot(live_info())
  })
  
  output$wea_gg_output2 <- renderPlot({ 
    req(live_info())
    create_weather_jitter_plot(live_info())
  })
  
  # -----------------------------------------------------------------------
  # LIVE MAP LOGIC (Corrected to use live_info() data source)
  # -----------------------------------------------------------------------
  
  observeEvent(c(input$refresh_live_map, input$map_route_select), {
    current_live_data <- live_info()
    req(current_live_data, exists("sf_stops"), input$map_route_select)
    
    target_route <- input$map_route_select
    
    # Pass live data stream to the utility function for simulation/location
    live_data <- get_live_location(current_live_data, target_route) 
    
    leafletProxy("mapPlotOut") %>%
      clearGroup("vehicles") 
    
    if (length(live_data$vehicles) > 0) {
      lats <- sapply(live_data$vehicles, function(x) x$lat)
      lons <- sapply(live_data$vehicles, function(x) x$lon)
      ids  <- sapply(live_data$vehicles, function(x) x$vehicle_id)
      status <- sapply(live_data$vehicles, function(x) x$status)
      
      leafletProxy("mapPlotOut") %>%
        addCircleMarkers(
          lng = lons,
          lat = lats,
          group = "vehicles",
          radius = 10,
          color = "black",
          weight = 2,
          opacity = 1,
          fillColor = ifelse(status == "Delayed", "#E74C3C", "#2ECC71"),
          fillOpacity = 1.0,
          popup = paste0("<b>Bus:</b> ", ids, "<br><b>Status:</b> ", status)
        )
      
      if(input$refresh_live_map > 0) {
         showNotification(paste("Synced", length(live_data$vehicles), "vehicles"), duration = 2)
      }
    }
  })

  # -----------------------------------------------------------------------
  # AI INSIGHT LOGIC
  # -----------------------------------------------------------------------
  
  # The AI functions use placeholder/simulated data and don't strictly require live_info() 
  # for the output text generation itself, so they remain as is.

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

# This 'ui' object must be available globally for Shiny to find the application.
ui <- page_fluid(
  style = "padding: 0; margin: 0;",
  
  # JavaScript for session restoration (handles auto-login)
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
  
  # The Root UI Router (will show login_ui() or dashboard_ui())
  uiOutput("root_ui")
)


message(sprintf("[SYSTEM] UI generation complete. Total startup time: %s seconds", round(difftime(Sys.time(), startup_start, units = "secs"), 2)))

# -------------------------------------------------------------------------
# 3. RUN APPLICATION
# -------------------------------------------------------------------------
shinyApp(ui, server)