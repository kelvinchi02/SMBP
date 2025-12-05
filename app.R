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
# 3. SERVER LOGIC
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  message("[SERVER] Client connected. Session started.")
  
  # -----------------------------------------------------------------------
  # 0. AUTO-REFRESH LOGIC
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
      
      if (!"crowding_level" %in% names(df)) {
        df[, crowding_level := data.table::fcase(
           occupancy_rate < 0.5, "Low",
           occupancy_rate < 0.85, "Medium",
           default = "High"
        )]
      }
      
      return(df)
    }
  )

  # --- 1. AUTHENTICATION & ROUTING ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")
  
  # Note: chat_history reactiveVal is REMOVED for stateless operation

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

  # Auto-login & Logout
  observeEvent(input$restore_session, {
    tryCatch({
      user_data <- jsonlite::fromJSON(input$restore_session)
      if (!is.null(user_data$username)) {
        authenticated(TRUE)
        user_info(user_data)
      }
    }, error = function(e) { session$sendCustomMessage("clearStorage", list()) })
  })

  observeEvent(input$logout_btn, {
    authenticated(FALSE)
    user_info(NULL)
    current_view("dashboard")
    session$sendCustomMessage("clearStorage", list())
  })

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
  
  # Chat (UPDATED: STATELESS MODE)
  observeEvent(input$chat_message, {
    req(input$chat_message$text, live_info())
    user_message <- input$chat_message$text
    
    # 1. Define Persona (Standard System Prompt)
    system_persona <- list(role = "system", content = paste0(
      "You are the Smart Transit AI Assistant. Analyze the current live data provided below. ",
      "Provide concise, data-driven answers to transit managers. Do not mention that this data was injected."
    ))

    # 2. Generate Live Data Context (New System Message every time)
    live_data_summary <- get_live_kpi_summary(live_info())
    live_context <- list(role = "system", content = paste0("CURRENT LIVE SYSTEM STATUS:\n", live_data_summary))
    
    # 3. User Question
    user_msg_obj <- list(role = "user", content = user_message)
    
    # 4. Construct Stateless Message List
    # Only contains: [Persona] + [Live Data] + [Current Question]
    messages_to_send <- list(
        system_persona,
        live_context,
        user_msg_obj
    )
    
    # 5. Call Groq
    ai_response <- call_chatgpt(messages_to_send) 
    
    # 6. Send Response to UI (No History Saving)
    session$sendCustomMessage("chat_response", ai_response)
  })
  
  # --- OPERATIONAL ACTIONS (WRITE-BACK) ---
  observeEvent(input$add_ridership_trip_btn, {
    result <- add_ridership_trip("B", 10, 5000, 1) 
    if (is.null(result$error)) {
      showNotification(paste(result$message, "New Headway:", result$new_headway, "min"), type = "message")
    } else {
      showNotification(paste("Failed:", result$error), type = "error")
    }
  })
  
  observeEvent(input$add_crowding_trip_btn, {
    result <- add_crowding_trip("A", 12, 0.9, 1) 
    if (is.null(result$error)) {
      showNotification(paste(result$message, "Occupancy Reduction:", result$occupancy_reduction*100, "%"), type = "message")
    } else {
      showNotification(paste("Failed:", result$error), type = "error")
    }
  })
  
  # -----------------------------------------------------------------------
  # AI INSIGHT LOGIC
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
  


  # --- SCHEDULER MODULE LOGIC ---
  
  # Reactive values for the proposal state
  proposal_state <- reactiveValues(active = FALSE, route = NULL, time = NULL, reason = NULL)
  
  observeEvent(input$scan_system_btn, {
    # 1. Scan Routes A, B, C sequentially
    routes_to_scan <- c("A", "B", "C")
    found_issue <- FALSE
    
    for (r in routes_to_scan) {
      analysis <- get_route_crowding_profile(live_info(), r)
      if (is.null(analysis)) next
      
      # Ask AI
      prompt <- list(
        list(role="system", content="You are an Operations AI. Check the profile. If recommendation is YES, output 'DECISION: YES | REASON: [Brief reason]'. If NO, output 'DECISION: NO'."),
        list(role="user", content=analysis$profile_text)
      )
      response <- call_chatgpt(prompt)
      
      if (grepl("DECISION: YES", response)) {
        # Found an issue! Populate proposal and stop scanning.
        proposal_state$active <- TRUE
        proposal_state$route <- r
        proposal_state$time <- analysis$suggested_time
        proposal_state$reason <- sub(".*REASON:", "", response)
        
        output$scheduler_ai_message <- renderUI({ 
          div(strong(paste("Issue Detected on Route", r)), br(), trimws(proposal_state$reason)) 
        })
        found_issue <- TRUE
        break 
      }
    }
    
    if (!found_issue) {
      output$scheduler_ai_message <- renderUI({ div(class="status-optimal", icon("check"), " System Optimal. No additions needed.") })
      proposal_state$active <- FALSE
    }
  })
  
  # Render Proposal Card
  output$scheduler_proposal_card <- renderUI({
    if (proposal_state$active) {
      div(class="proposal-card",
        div(class="proposal-route", paste("Add Trip to Route", proposal_state$route)),
        div(class="proposal-time", proposal_state$time),
        div(class="proposal-impact", "Fills schedule gap to reduce crowding")
      )
    } else {
      div(class="proposal-card", style="opacity: 0.5;", h4("No Pending Proposals"))
    }
  })
  
  # Render Confirm Button (Active/Disabled)
  output$scheduler_confirm_btn_ui <- renderUI({
    if (proposal_state$active) {
      actionButton("confirm_schedule_btn", "Confirm & Add Schedule", class = "btn-confirm", icon = icon("plus"))
    } else {
      actionButton("disabled_btn", "Confirm & Add Schedule", class = "btn-confirm btn-disabled", disabled = TRUE)
    }
  })
  
  # Dismiss Action
  observeEvent(input$dismiss_proposal_btn, {
    proposal_state$active <- FALSE
    output$scheduler_ai_message <- renderUI({ "Suggestion dismissed." })
  })
  
  # Confirm Action
  observeEvent(input$confirm_schedule_btn, {
    req(proposal_state$active)
    # Write to DB
    # We need a slight mod to add_new_trip_to_db to accept a specific time if possible, 
    # otherwise it defaults to NOW. For MVP, we just trigger the function.
    add_new_trip_to_db(proposal_state$route) 
    
    showNotification(paste("✅ Added Trip to Route", proposal_state$route, "at", proposal_state$time), type="message")
    proposal_state$active <- FALSE
    output$scheduler_ai_message <- renderUI({ "Action completed successfully." })
  })


  # -----------------------------------------------------------------------
  # DASHBOARD PLOT RENDERING
  # -----------------------------------------------------------------------
  output$summaryOutputPlot1 <- renderPlot({ current_data <- live_info(); req(current_data, exists("routes")); create_occupancy_box(current_data, routes) })
  output$summaryOutputPlot2 <- renderPlot({ current_data <- live_info(); req(current_data, exists("routes")); create_delay_jitter(current_data, routes) })
  output$summaryOutputPlot3 <- renderPlotly({ req(input$summaryplot3whatRoute); current_data <- live_info(); req(current_data, exists("routes")); create_crowding_pie(current_data, routes, input$summaryplot3whatRoute) })
  output$mapPlotOut <- renderLeaflet({ req(exists("sf_stops")); makemap(sf_stops, input$whatMapalpha, input$map_route_select) })
  output$ridership_kpis <- renderUI({ current_data <- live_info(); req(current_data); kpis <- get_ridership_kpis(current_data); div(class = "kpi-grid", div(class = "kpi-card", div(class = "kpi-label", "Total Passengers"), div(class = "kpi-value", kpis$total_passengers)), div(class = "kpi-card", div(class = "kpi-label", "Average Occupancy"), div(class = "kpi-value", kpis$avg_occupancy)), div(class = "kpi-card", div(class = "kpi-label", "Current Load Status"), div(class = paste("kpi-value", kpis$status_class), kpis$status))) })
  output$trendPlot <- renderPlot({ req(live_info(), exists("routes")); create_ride_trend_bar(live_info(), routes) })
  output$dailyMap <- renderPlotly({ req(input$ride_date_select, live_info(), exists("routes")); p <- create_ride_plot1(live_info(), input$ride_date_select, routes); if(is.null(p)) return(NULL); ggplotly(p) %>% hide_legend() })
  output$crowding_breakdown_display <- renderUI({ current_data <- live_info(); req(current_data); dt_local <- as.data.table(current_data); dt_local[, crowding_level := fcase(occupancy_rate < 0.5, "Low", occupancy_rate < 0.85, "Medium", default = "High")]; kpis <- get_crowding_kpis(dt_local); div(class = "breakdown-grid", div(class = "breakdown-item", div(class = "breakdown-label", "Low Crowding Trips"), div(class = "breakdown-value low", kpis$low_count)), div(class = "breakdown-item", div(class = "breakdown-label", "Medium Crowding Trips"), div(class = "breakdown-value medium", kpis$med_count)), div(class = "breakdown-item", div(class = "breakdown-label", "High Crowding Trips"), div(class = "breakdown-value high", kpis$high_count))) })
  output$crowding_kpis <- renderUI({ current_data <- live_info(); req(current_data); kpis <- get_crowding_kpis(current_data); avg_occ_text <- kpis$avg_occupancy; occ_class <- if (grepl("^0\\.[0-4]", avg_occ_text)) "normal" else if (grepl("^0\\.[5-8]", avg_occ_text)) "busy" else "critical"; div(class = "kpi-grid", div(class = "kpi-card", div(class = "kpi-label", "Average Occupancy"), div(class = paste("kpi-value", occ_class), kpis$avg_occupancy)), div(class = "kpi-card", div(class = "kpi-label", "Overload Risk Zones (>85%)"), div(class = "kpi-value critical", kpis$risk_zones))
  ) })
  output$crowd_ggplot1 <- renderPlotly({ req(live_info()); create_crowd_map(live_info()) }) 
  output$crowd_ggplot2 <- renderPlot({ req(live_info()); create_crowd_bar(live_info()) }) 
  output$realtime_kpis <- renderUI({ current_data <- live_info(); req(current_data); kpis <- get_realtime_kpis(current_data); punctuality_class <- fcase(kpis$punctuality < 75, "bad", kpis$punctuality < 90, "warning", default = "good"); delay_class <- fcase(kpis$avg_delay > 2, "bad", kpis$avg_delay > 1, "warning", default = "good"); div(class = "kpi-grid", div(class = "kpi-card", div(class = "kpi-label", "Punctuality Rate"), div(class = paste("kpi-value", punctuality_class), paste0(kpis$punctuality, "%"))), div(class = "kpi-card", div(class = "kpi-label", "Avg. Delay (min)"), div(class = paste("kpi-value", delay_class), kpis$avg_delay)), div(class = "kpi-card", div(class = "kpi-label", "Active Issues"), div(class = "kpi-value warning", kpis$active_issues))) })
  output$worst_stops_display <- renderUI({ current_data <- live_info(); req(current_data); worst_stops_dt <- get_worst_stops(current_data); if (is.null(worst_stops_dt) || nrow(worst_stops_dt) == 0) return(NULL); div(class = "worst-stops", div(class = "worst-stops-title", "Top 5 Delay Hotspots"), lapply(1:nrow(worst_stops_dt), function(i) { row <- worst_stops_dt[i]; div(class = "stop-item", span(row$stop_name), span(paste(round(row$avg_delay, 1), "min"))) })) })
  output$hour_delay_plot1 <- renderPlot({ req(live_info(), exists("routes")); create_hour_plot1(live_info(), routes) })
  output$hour_delay_plot2 <- renderPlot({ req(live_info(), input$hourwhatRoute); create_hour_plot2(live_info(), input$hourwhatRoute) })
  output$weather_current_display <- renderUI({ current_data <- live_info(); req(current_data); weather_data <- get_current_weather(current_data); current <- weather_data$current; div(class = "weather-current-grid", div(class = "weather-kpi", div(class = "weather-kpi-label", "Current Condition"), div(class = "weather-kpi-value", current$condition)), div(class = "weather-kpi", div(class = "weather-kpi-label", "Temperature"), div(class = "weather-kpi-value", paste0(current$temperature, "°C"))), div(class = "weather-kpi", div(class = "weather-kpi-label", "Humidity"), div(class = "weather-kpi-value", paste0(current$humidity, "%"))), div(class = "weather-kpi", div(class = "weather-kpi-label", "Wind Speed"), div(class = "weather-kpi-value", paste0(current$wind_speed, " km/h"))) ) })
  output$weather_forecast_display <- renderUI({ current_data <- live_info(); req(current_data); weather_data <- get_current_weather(current_data); forecast <- weather_data$forecast; div(class = "forecast-grid", lapply(forecast, function(item) { div(class = "forecast-item", div(class = "forecast-time", item$time), div(class = "forecast-temp", paste0(item$temperature, "°C")), div(class = "forecast-condition", item$condition)) })) })
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
      if(input$refresh_live_map > 2) { showNotification(paste("Synced", length(live_data$vehicles), "vehicles"), duration = 2) }
    }
  })
}

# -------------------------------------------------------------------------
# 4. RUN APPLICATION (Execution)
# -------------------------------------------------------------------------
shinyApp(ui, server)