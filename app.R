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
library(DT)

# Initialize Startup Timer
startup_start <- Sys.time()
message(paste("[SYSTEM] Application startup initiated at:", startup_start))

# -------------------------------------------------------------------------
# 1. SETUP & MODULE LOADING
# -------------------------------------------------------------------------

# Load environment variables
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
source("scheduler.R") 

# Ensure image path exists
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
  
  # --- 0. LOCAL SCHEDULE STORAGE (SIMULATION MODE) ---
  # This var holds the schedule in memory. We load it once from DB, then modify locally.
  local_schedule <- reactiveVal(data.table())
  
  # Initialize schedule ONCE on startup
  observe({
    req(is.null(isolate(local_schedule())) || nrow(isolate(local_schedule())) == 0)
    
    message("[SYSTEM] Loading schedule from Supabase into local simulation memory...")
    initial_data <- load_supabase_table("bus_schedule")
    
    if (nrow(initial_data) > 0) {
      # Ensure time format is usable
      if(inherits(initial_data$scheduled_departure, "character")) {
         # Assuming format is HH:MM:SS, we prepend today's date for sorting
         initial_data[, full_time := as.POSIXct(paste(Sys.Date(), scheduled_departure))]
      } else {
         initial_data[, full_time := scheduled_departure]
      }
      local_schedule(initial_data)
    }
  })

  # --- 0.1 AUTO-REFRESH LOGIC (LIVE BUS DATA) ---
  live_info <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() { get_latest_timestamp("SmartTransit_Integrated") },
    valueFunc = function() {
      df <- load_supabase_table("SmartTransit_Integrated")
      if(inherits(df$scheduled_arrival, "character")) { df[, scheduled_arrival := lubridate::ymd_hms(scheduled_arrival)] }
      df[, hour := lubridate::hour(scheduled_arrival)]
      df[, hour := factor(hour, levels = sort(unique(hour)))]
      df[, delay_category := data.table::fcase(delay_min > 0, "Delayed", delay_min < 0, "Early", delay_min == 0, "On-time")]
      if (!"crowding_level" %in% names(df)) {
        df[, crowding_level := data.table::fcase(occupancy_rate < 0.5, "Low", occupancy_rate < 0.85, "Medium", default = "High")]
      }
      return(df)
    }
  )

  # --- 1. AUTHENTICATION & ROUTING ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")
  
  output$root_ui <- renderUI({
    if (!authenticated()) { login_ui() } else { tagList(dashboard_ui(user_name = user_info()$name), chat_ui()) }
  })

  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    auth_result <- authenticate_user(input$login_username, input$login_password)
    if (auth_result$success) {
      authenticated(TRUE); user_info(list(name = input$login_username))
      session$sendCustomMessage("saveUserInfo", list(username = input$login_username, name = input$login_username))
    } else {
      insertUI(selector = "#login_error", where = "afterBegin", ui = div(style = "color:red; margin-top:10px; font-weight:bold;", auth_result$message))
    }
  })
  
  observeEvent(input$restore_session, { tryCatch({ user_data <- jsonlite::fromJSON(input$restore_session); if (!is.null(user_data$username)) { authenticated(TRUE); user_info(user_data) } }, error = function(e) { session$sendCustomMessage("clearStorage", list()) }) })
  observeEvent(input$logout_btn, { authenticated(FALSE); user_info(NULL); current_view("dashboard"); session$sendCustomMessage("clearStorage", list()) })
  observeEvent(input$nav_selection, { current_view(input$nav_selection) })
  observeEvent(input$back_to_home, { current_view("dashboard") })

  # --- 3. DASHBOARD CONTENT ---
  output$topbar_title_dynamic <- renderUI({ req(authenticated()); div(class = "topbar-title", "Smart Bus Management Platform") })
  output$dashboard_content <- renderUI({
    req(authenticated())
    switch(current_view(),
      "dashboard" = dashboard_home_content(),
      "overview" = overview_ui(),
      "scheduler" = scheduler_ui(),
      "delay" = delay_ui(),
      "ridership" = rider_ui(),
      "crowding" = crowd_ui(),
      "weather" = weather_ui(),
      "map" = map_ui()
    )
  })

  # --- 4. MODULE SERVER LOGIC ---
  observeEvent(input$chat_message, {
    req(input$chat_message$text, live_info())
    user_message <- input$chat_message$text
    system_persona <- list(role = "system", content = "You are the Smart Transit AI Assistant. Analyze the current live data provided. Provide concise, data-driven answers.")
    live_context <- list(role = "system", content = paste0("CURRENT LIVE SYSTEM STATUS:\n", get_live_kpi_summary(live_info())))
    messages_to_send <- list(system_persona, live_context, list(role = "user", content = user_message))
    session$sendCustomMessage("chat_response", call_chatgpt(messages_to_send))
  })
  
  # --- SCHEDULER MODULE LOGIC (UPDATED) ---
  
  # 1. Render the 3-Column Schedule Table (Next 10 Departures)
  output$schedule_table <- DT::renderDataTable({
    req(local_schedule())
    
    sched <- local_schedule()
    
    # Filter for future trips only
    current_time <- Sys.time()
    future_sched <- sched[full_time > current_time][order(full_time)]
    
    if(nrow(future_sched) == 0) return(NULL)
    
    # Helper to get next 10 times for a route
    get_next_10 <- function(r_id) {
      times <- future_sched[route_id == r_id, head(format(full_time, "%H:%M"), 10)]
      # Pad with empty strings if less than 10
      length(times) <- 10
      return(times)
    }
    
    # Construct the display table
    display_df <- data.frame(
      "Route A" = get_next_10("A"),
      "Route B" = get_next_10("B"),
      "Route C" = get_next_10("C"),
      stringsAsFactors = FALSE
    )
    
    # Replace NAs with "--"
    display_df[is.na(display_df)] <- "--"
    
    DT::datatable(display_df, 
      options = list(dom = 't', pageLength = 10, ordering = FALSE),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    )
  })

  # 2. AI Proposal Logic
  proposal_state <- reactiveValues(active = FALSE, route = NULL, time = NULL, reason = NULL)
  
  observeEvent(input$scan_system_btn, {
    routes_to_scan <- c("A", "B", "C")
    found_issue <- FALSE
    
    for (r in routes_to_scan) {
      analysis <- get_route_crowding_profile(live_info(), r)
      if (is.null(analysis)) next
      
      prompt <- list(
        list(role="system", content="You are an Operations AI. Check the profile. If recommendation is YES, output 'DECISION: YES | REASON: [Brief reason]'. If NO, output 'DECISION: NO'."),
        list(role="user", content=analysis$profile_text)
      )
      response <- call_chatgpt(prompt)
      
      if (grepl("DECISION: YES", response)) {
        proposal_state$active <- TRUE
        proposal_state$route <- r
        proposal_state$time <- analysis$suggested_time
        proposal_state$reason <- sub(".*REASON:", "", response)
        output$scheduler_ai_message <- renderUI({ div(strong(paste("Issue Detected on Route", r)), br(), trimws(proposal_state$reason)) })
        found_issue <- TRUE
        break 
      }
    }
    
    if (!found_issue) {
      output$scheduler_ai_message <- renderUI({ div(class="status-optimal", icon("check"), " System Optimal. No additions needed.") })
      proposal_state$active <- FALSE
    }
  })
  
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
  
  output$scheduler_confirm_btn_ui <- renderUI({
    if (proposal_state$active) {
      actionButton("confirm_schedule_btn", "Confirm & Add Schedule", class = "btn-confirm", icon = icon("plus"))
    } else {
      actionButton("disabled_btn", "Confirm & Add Schedule", class = "btn-confirm btn-disabled", disabled = TRUE)
    }
  })
  
  observeEvent(input$dismiss_proposal_btn, {
    proposal_state$active <- FALSE
    output$scheduler_ai_message <- renderUI({ "Suggestion dismissed." })
  })
  
  # 3. Confirm Logic (UPDATED: Modifies Local Var Only)
  observeEvent(input$confirm_schedule_btn, {
    req(proposal_state$active, local_schedule())
    
    # Create the new row
    new_time_str <- proposal_state$time # Format HH:MM:SS
    new_datetime <- as.POSIXct(paste(Sys.Date(), new_time_str))
    
    new_row <- data.table(
      route_id = proposal_state$route,
      scheduled_departure = new_time_str,
      headway_min = 15, # Default value for inserted trip
      full_time = new_datetime
    )
    
    # Append to local schedule and re-sort
    current <- local_schedule()
    updated <- rbind(current, new_row, fill = TRUE)
    updated <- updated[order(full_time)]
    
    # Update the reactive variable
    local_schedule(updated)
    
    showNotification(paste("✅ [SIMULATION] Added Trip to Route", proposal_state$route, "at", proposal_state$time), type="message")
    proposal_state$active <- FALSE
    output$scheduler_ai_message <- renderUI({ "Action completed successfully (Local Update)." })
  })

  # --- PLOT RENDERING ---
  output$summaryOutputPlot1 <- renderPlot({ current_data <- live_info(); req(current_data, exists("routes")); create_occupancy_box(current_data, routes) })
  output$summaryOutputPlot2 <- renderPlot({ current_data <- live_info(); req(current_data, exists("routes")); create_delay_jitter(current_data, routes) })
  output$summaryOutputPlot3 <- renderPlotly({ req(input$summaryplot3whatRoute); current_data <- live_info(); req(current_data, exists("routes")); create_crowding_pie(current_data, routes, input$summaryplot3whatRoute) })
  output$mapPlotOut <- renderLeaflet({ req(exists("sf_stops")); makemap(sf_stops, input$whatMapalpha, input$map_route_select) })
  output$ridership_kpis <- renderUI({ current_data <- live_info(); req(current_data); kpis <- get_ridership_kpis(current_data); div(class = "kpi-grid", div(class = "kpi-card", div(class = "kpi-label", "Total Passengers"), div(class = "kpi-value", kpis$total_passengers)), div(class = "kpi-card", div(class = "kpi-label", "Average Occupancy"), div(class = "kpi-value", kpis$avg_occupancy)), div(class = "kpi-card", div(class = "kpi-label", "Current Load Status"), div(class = paste("kpi-value", kpis$status_class), kpis$status))) })
  output$trendPlot <- renderPlot({ req(live_info(), exists("routes")); create_ride_trend_bar(live_info(), routes) })
  output$dailyMap <- renderPlotly({ req(input$ride_date_select, live_info(), exists("routes")); p <- create_ride_plot1(live_info(), input$ride_date_select, routes); if(is.null(p)) return(NULL); ggplotly(p) %>% hide_legend() })
  output$crowding_breakdown_display <- renderUI({ current_data <- live_info(); req(current_data); dt_local <- as.data.table(current_data); dt_local[, crowding_level := fcase(occupancy_rate < 0.5, "Low", occupancy_rate < 0.85, "Medium", default = "High")]; kpis <- get_crowding_kpis(dt_local); div(class = "breakdown-grid", div(class = "breakdown-item", div(class = "breakdown-label", "Low Crowding Trips"), div(class = "breakdown-value low", kpis$low_count)), div(class = "breakdown-item", div(class = "breakdown-label", "Medium Crowding Trips"), div(class = "breakdown-value medium", kpis$med_count)), div(class = "breakdown-item", div(class = "breakdown-label", "High Crowding Trips"), div(class = "breakdown-value high", kpis$high_count))) })
  output$crowding_kpis <- renderUI({ current_data <- live_info(); req(current_data); kpis <- get_crowding_kpis(current_data); avg_occ_text <- kpis$avg_occupancy; occ_class <- if (grepl("^0\\.[0-4]", avg_occ_text)) "normal" else if (grepl("^0\\.[5-8]", avg_occ_text)) "busy" else "critical"; div(class = "kpi-grid", div(class = "kpi-card", div(class = "kpi-label", "Average Occupancy"), div(class = paste("kpi-value", occ_class), kpis$avg_occupancy)), div(class = "kpi-card", div(class = "kpi-label", "Overload Risk Zones (>85%)"), div(class = "kpi-value critical", kpis$risk_zones)) ) })
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

shinyApp(ui, server)