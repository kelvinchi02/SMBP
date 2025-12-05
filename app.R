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
if (file.exists(".env")) load_dot_env(".env")
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Source Modules (UI and Function definitions)
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
source("styles.R") 

# Ensure image path exists and register it
if (dir.exists("www/index")) {
  addResourcePath("index", "www/index")
}

# -------------------------------------------------------------------------
# 2. MAIN UI SHELL DEFINITION
# -------------------------------------------------------------------------
ui <- page_fluid(
  style = "padding: 0; margin: 0;",
  
  # JavaScript for session restoration and error handling
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
       $(document).on('shiny:value', function(event) {
        if (event.name === 'login_error_trigger') {
          var msg = event.value;
          if (msg && msg.length > 0) {
            $('#error-msg').text(msg).addClass('show');
            setTimeout(function() { $('#error-msg').removeClass('show'); }, 3000);
          }
        }
      });
    "))
  ),
  
  uiOutput("root_ui")
)

message(sprintf("[SYSTEM] UI definition complete. Total setup time: %s seconds", round(difftime(Sys.time(), startup_start, units = "secs"), 2)))


# -------------------------------------------------------------------------
# 3. SERVER LOGIC (COMBINED)
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  message("[SERVER] Client connected. Session started.")
  
  # -----------------------------------------------------------------------
  # 0. AUTO-REFRESH LOGIC (Reactive Polling)
  # -----------------------------------------------------------------------
  
  live_info <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    
    checkFunc = function() {
      get_latest_timestamp("SmartTransit_Integrated")
    },
    
    valueFunc = function() {
      message("[SYSTEM] New data detected. Refreshing dataset...")
      
      df <- load_supabase_table("SmartTransit_Integrated")
      
      # Data Cleaning (as defined in server.R's valueFunc)
      if(inherits(df$scheduled_arrival, "character")) {
        df[, scheduled_arrival := lubridate::ymd_hms(scheduled_arrival)]
      }
      
      df[, hour := lubridate::hour(scheduled_arrival)]
      df[, hour := factor(hour, levels = sort(unique(hour)))]
      
      df[, delay_category := data.table::fcase(
        delay_min > 0, "Delayed",
        delay_min < 0, "Early",
        delay_min == 0, "On-time"
      )]
      
      # CRITICAL: Calculate crowding_level on the fly for use in KPI summarization
      df[, crowding_level := data.table::fcase(
         occupancy_rate < 0.5, "Low",
         occupancy_rate < 0.85, "Medium",
         default = "High"
      )]
      
      return(df)
    }
  )

  # --- 1. AUTHENTICATION & ROUTING ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")
  
  # --- 2. CONVERSATION HISTORY & STATE (NEW) ---
  
  # Max history: 1 system persona + N conversation pairs. We limit pairs to 10.
  MAX_HISTORY_PAIRS <- 10 
  
  # Initialize with the permanent system persona prompt
  chat_history <- reactiveVal(list(
      list(role = "system", content = paste0(
        "You are the Smart Transit AI Assistant. Your role is to analyze current bus operational data and provide concise, professional insights to transit managers. ",
        "Your answers must be based only on the data provided in the 'CURRENT LIVE SYSTEM STATUS' section below. ",
        "Be concise, analytical, and limit responses to 3-4 sentences. Do not mention the existence of your context window or data injection."
      ))
  ))

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
  
  # Chat (UPDATED TO MANAGE HISTORY AND CALL GROQ)
  observeEvent(input$chat_message, {
    req(input$chat_message$text, live_info())
    user_message <- input$chat_message$text
    
    # 1. Start with the current full history
    current_history <- chat_history()
    
    # 2. Apply Rolling Context Management (Truncation)
    if (length(current_history) > (MAX_HISTORY_PAIRS * 2) + 1) {
      # Keep the system persona (index 1) and the latest N*2 messages
      current_history <- c(current_history[1], tail(current_history, MAX_HISTORY_PAIRS * 2))
    }

    # 3. Generate Live Data Context Message (Part 2)
    live_data_summary <- get_live_kpi_summary(live_info())
    context_message <- list(role = "system", content = paste0("CURRENT LIVE SYSTEM STATUS: ", live_data_summary))
    
    # 4. Assemble the FINAL list to send to Groq
    messages_to_send <- c(
        current_history[1], # Persona
        list(context_message), # Live Data
        tail(current_history, -1), # Existing history (excluding persona)
        list(list(role = "user", content = user_message)) # Current Query
    )
    
    # 5. Call Groq with the full context window
    ai_response <- call_chatgpt(messages_to_send) 
    
    # 6. Update permanent history (Append only User Query and AI Response)
    if (startsWith(ai_response, "Error:") || startsWith(ai_response, "I was unable to connect")) {
        final_response <- ai_response
    } else {
        user_message_to_save <- list(role = "user", content = user_message)
        ai_message_to_save <- list(role = "assistant", content = ai_response)
        
        final_history <- c(current_history, user_message_to_save, ai_message_to_save)
        
        chat_history(final_history)
        final_response <- ai_response
    }
    
    # 7. Send final response to UI
    session$sendCustomMessage("chat_response", final_response)
  })
  
  # -----------------------------------------------------------------------
  # AI INSIGHT LOGIC (Existing Placeholders)
  # -----------------------------------------------------------------------
  
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
  
  # -----------------------------------------------------------------------
  # DASHBOARD PLOT RENDERING (Existing)
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

  output$mapPlotOut <- renderLeaflet({ 
    req(exists("sf_stops")) 
    makemap(sf_stops, input$whatMapalpha, input$map_route_select) 
  })
  
  output$ridership_kpis <- renderUI({
    current_data <- live_info()
    req(current_data)
    kpis <- get_ridership_kpis(current_data)
    div(class = "kpi-grid",
      div(class = "kpi-card", div(class = "kpi-label", "Total Passengers"), div(class = "kpi-value", kpis$total_passengers)),
      div(class = "kpi-card", div(class = "kpi-label", "Average Occupancy"), div(class = "kpi-value", kpis$avg_occupancy)),
      div(class = "kpi-card", div(class = "kpi-label", "Current Load Status"), div(class = paste("kpi-value", kpis$status_class), kpis$status))
    )
  })
  
  output$trendPlot <- renderPlot({ 
    req(live_info(), exists("routes"))
    create_ride_trend_bar(live_info(), routes)
  })
  
  output$dailyMap <- renderPlotly({ 
    req(input$ride_date_select, live_info(), exists("routes"))
    p <- create_ride_plot1(live_info(), input$ride_date_select, routes)
    if(is.null(p)) return(NULL)
    ggplotly(p) %>% hide_legend() 
  })
  
  output$crowding_breakdown_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    dt_local <- as.data.table(current_data)
    dt_local[, crowding_level := fcase(occupancy_rate < 0.5, "Low", occupancy_rate < 0.85, "Medium", default = "High")]
    kpis <- get_crowding_kpis(dt_local) 
    div(class = "breakdown-grid",
      div(class = "breakdown-item", div(class = "breakdown-label", "Low Crowding Trips"), div(class = "breakdown-value low", kpis$low_count)),
      div(class = "breakdown-item", div(class = "breakdown-label", "Medium Crowding Trips"), div(class = "breakdown-value medium", kpis$med_count)),
      div(class = "breakdown-item", div(class = "breakdown-label", "High Crowding Trips"), div(class = "breakdown-value high", kpis$high_count))
    )
  })
  
  output$crowding_kpis <- renderUI({
    current_data <- live_info()
    req(current_data)
    kpis <- get_crowding_kpis(current_data)
    avg_occ_text <- kpis$avg_occupancy
    occ_class <- if (grepl("^0\\.[0-4]", avg_occ_text)) "normal" else if (grepl("^0\\.[5-8]", avg_occ_text)) "busy" else "critical"
    div(class = "kpi-grid",
      div(class = "kpi-card", div(class = "kpi-label", "Average Occupancy"), div(class = paste("kpi-value", occ_class), kpis$avg_occupancy)),
      div(class = "kpi-card", div(class = "kpi-label", "Overload Risk Zones (>85%)"), div(class = "kpi-value critical", kpis$risk_zones))
    )
  })
  
  output$crowd_ggplot1 <- renderPlotly({ req(live_info()); create_crowd_map(live_info()) }) 
  output$crowd_ggplot2 <- renderPlot({ req(live_info()); create_crowd_bar(live_info()) }) 
  
  output$realtime_kpis <- renderUI({
    current_data <- live_info()
    req(current_data)
    kpis <- get_realtime_kpis(current_data)
    punctuality_class <- fcase(kpis$punctuality < 75, "bad", kpis$punctuality < 90, "warning", default = "good")
    delay_class <- fcase(kpis$avg_delay > 2, "bad", kpis$avg_delay > 1, "warning", default = "good")
    div(class = "kpi-grid",
      div(class = "kpi-card", div(class = "kpi-label", "Punctuality Rate"), div(class = paste("kpi-value", punctuality_class), paste0(kpis$punctuality, "%"))),
      div(class = "kpi-card", div(class = "kpi-label", "Avg. Delay (min)"), div(class = paste("kpi-value", delay_class), kpis$avg_delay)),
      div(class = "kpi-card", div(class = "kpi-label", "Active Issues"), div(class = "kpi-value warning", kpis$active_issues))
    )
  })

  output$worst_stops_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    worst_stops_dt <- get_worst_stops(current_data)
    if (is.null(worst_stops_dt) || nrow(worst_stops_dt) == 0) return(NULL)
    div(class = "worst-stops",
      div(class = "worst-stops-title", "Top 5 Delay Hotspots"),
      lapply(1:nrow(worst_stops_dt), function(i) {
        row <- worst_stops_dt[i]
        div(class = "stop-item", span(row$stop_name), span(paste(round(row$avg_delay, 1), "min")))
      })
    )
  })
  
  output$hour_delay_plot1 <- renderPlot({ req(live_info(), exists("routes")); create_hour_plot1(live_info(), routes) })
  output$hour_delay_plot2 <- renderPlot({ req(live_info(), input$hourwhatRoute); create_hour_plot2(live_info(), input$hourwhatRoute) })
  
  output$weather_current_display <- renderUI({
    current_data <- live_info()
    req(current_data)
    weather_data <- get_current_weather(current_data)
    current <- weather_data$current
    div(class = "weather-current-grid",
      div(class = "weather-kpi", div(class = "weather-kpi-label", "Current Condition"), div(class = "weather-kpi-value", current$condition)),
      div(class = "weather-kpi", div(class = "weather-kpi-label", "Temperature"), div(class = "weather-kpi-value", paste0(current$temperature, "°C"))),
      div(class = "weather-kpi", div(class = "weather-kpi-label", "Humidity"), div(class = "weather-kpi-value", paste0(current$humidity, "%"))),
      div(class = "weather-kpi", div(class = "weather-kpi-label", "Wind Speed"), div(class = "weather-kpi-value", paste0(current$wind_speed, " km/h")))
    )
  })

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
  
  output$wea_hc_output <- renderHighchart({ req(live_info()); create_weather_polar_chart(live_info()) })
  output$wea_gg_output1 <- renderPlot({ req(live_info()); create_weather_ridge_plot(live_info()) })
  output$wea_gg_output2 <- renderPlot({ req(live_info()); create_weather_jitter_plot(live_info()) })
  
  observeEvent(c(input$refresh_live_map, input$map_route_select), {
    current_live_data <- live_info()
    req(current_live_data, exists("sf_stops"), input$map_route_select)
    target_route <- input$map_route_select
    live_data <- get_live_location(current_live_data, target_route) 
    leafletProxy("mapPlotOut") %>% clearGroup("vehicles") 
    if (length(live_data$vehicles) > 0) {
      lats <- sapply(live_data$vehicles, function(x) x$lat)
      lons <- sapply(live_data$vehicles, function(x) x$lon)
      ids  <- sapply(live_data$vehicles, function(x) x$vehicle_id)
      status <- sapply(live_data$vehicles, function(x) x$status)
      leafletProxy("mapPlotOut") %>%
        addCircleMarkers(lng = lons, lat = lats, group = "vehicles", radius = 10, color = "black", weight = 2, opacity = 1,
          fillColor = ifelse(status == "Delayed", "#E74C3C", "#2ECC71"), fillOpacity = 1.0,
          popup = paste0("<b>Bus:</b> ", ids, "<br><b>Status:</b> ", status))
      if(input$refresh_live_map > 0) { showNotification(paste("Synced", length(live_data$vehicles), "vehicles"), duration = 2) }
    }
  })
}

# -------------------------------------------------------------------------
# 4. RUN APPLICATION (Execution)
# -------------------------------------------------------------------------
shinyApp(ui, server)