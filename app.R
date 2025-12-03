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

# -------------------------------------------------------------------------
# 1. SETUP & LOADING
# -------------------------------------------------------------------------

# Load Environment Variables (API Keys & DB Credentials)
if (file.exists(".env")) {
  load_dot_env(".env")
}
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Load Backend Logic
source("database_connection.R")
source("pre.R")         # Loads Supabase data
source("api_utils.R")   # Loads Real-time & AI logic

# Load UI Modules
source("dashboard.R")   # NEW Sidebar & Carousel UI
source("login.R")       # Glassmorphism Login UI
source("chat.R")        # Chat UI
source("overview.R")
source("map.R")
source("weather.R")
source("crowd.R")
source("ridership.R")
source("hour.R")

# -------------------------------------------------------------------------
# 2. MAIN UI SHELL
# -------------------------------------------------------------------------
# Add resource path for images in 'www' folder
addResourcePath("index", "www/index") 

ui <- page_fluid(
  style = "padding: 0; margin: 0;",
  
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
  uiOutput("app_content")
)

# -------------------------------------------------------------------------
# 3. SERVER LOGIC
# -------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- AUTHENTICATION STATE ---
  authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")

  # --- DATA STATE (Reactive Values) ---
  realtime_data <- reactiveVal(NULL)
  ai_insight_data <- reactiveVal(NULL)
  ridership_data <- reactiveVal(NULL)
  ai_ridership_data <- reactiveVal(NULL)
  crowding_data <- reactiveVal(NULL)
  ai_crowding_data <- reactiveVal(NULL)
  weather_data <- reactiveVal(NULL)
  weather_impact_data <- reactiveVal(NULL)
  ai_weather_data <- reactiveVal(NULL)
  live_vehicles_data <- reactiveVal(NULL)
  ai_stop_data <- reactiveVal(NULL)

  # --- MAIN ROUTER (Login vs Dashboard) ---
  output$app_content <- renderUI({
    if (!authenticated()) {
      login_ui() # From login.R
    } else {
      tagList(
        # Uses the Sidebar Layout from dashboard.R
        dashboard_ui(user_name = user_info()$name), 
        chat_ui()
      )
    }
  })

  # --- LOGIN HANDLERS ---
  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    
    # Use authenticate_user from login.R (checks .env)
    success <- authenticate_user(input$login_user, input$login_pass)

    if (success) {
      authenticated(TRUE)
      user_name_val <- input$login_user
      user_info(list(username = input$login_user, name = user_name_val))
      session$sendCustomMessage("saveUserInfo", list(username = input$login_user, name = user_name_val))
    } else {
      output$login_error <- renderUI({
        div(style = "color:red; margin-top:10px; font-weight:bold;", "Invalid username or password.")
      })
    }
  })

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

  # --- NAVIGATION HANDLERS ---
  # 'nav_selection' comes from the Sidebar clicks in dashboard.R
  observeEvent(input$nav_selection, { current_view(input$nav_selection) })
  observeEvent(input$back_to_home, { current_view("dashboard") })

  # --- TOPBAR TITLE UPDATE ---
  output$topbar_title_dynamic <- renderUI({
    req(authenticated())
    title_text <- switch(current_view(),
      "dashboard" = "Smart Bus Management Platform",
      "overview" = "Overview Dashboard",
      "delay" = "Delay & Punctuality Analysis",
      "ridership" = "Ridership Trends",
      "crowding" = "Crowding Analysis",
      "weather" = "Weather Impact",
      "map" = "Stops & Map",
      "Smart Bus Management Platform"
    )
    div(class = "topbar-title", title_text)
  })

  # --- CONTENT RENDERER ---
  output$dashboard_content <- renderUI({
    req(authenticated())
    switch(current_view(),
      "dashboard" = dashboard_home_content(), # From dashboard.R (Carousel)
      "overview" = overview_ui(),
      "delay" = delay_ui(),
      "ridership" = rider_ui(),
      "crowding" = crowd_ui(),
      "weather" = weather_ui(),
      "map" = map_ui()
    )
  })

  # --- CHATBOT LOGIC ---
  observeEvent(input$chat_message, {
    resp <- call_chatgpt(input$chat_message$text, OPENAI_API_KEY)
    session$sendCustomMessage("chat_response", resp)
  })

  # =========================================================================
  # MODULE BACKEND LOGIC (Connecting UI to api_utils.R)
  # =========================================================================

  # 1. OVERVIEW
  output$summaryOutputPlot1 <- renderPlot({ summary.plot1 })
  output$summaryOutputPlot2 <- renderPlot({ summary.plot2 })
  output$summaryOutputPlot3 <- renderPlotly({
    req(input$summaryplot3whatRoute)
    create_crowding_pie(info, routes, input$summaryplot3whatRoute)
  })

  # 2. DELAY
  observeEvent(input$refresh_realtime, { realtime_data(get_realtime_kpis()) })
  observe({ if(current_view() == "delay" && is.null(realtime_data())) realtime_data(get_realtime_kpis()) })
  
  output$realtime_kpis <- renderUI({
    d <- realtime_data()
    if(is.null(d)) return(NULL)
    div(class="kpi-grid",
        div(class="kpi-card", div(class="kpi-label", "Punctuality"), div(class="kpi-value good", paste0(d$punctuality, "%"))),
        div(class="kpi-card", div(class="kpi-label", "Avg Delay"), div(class="kpi-value warning", paste0(d$avg_delay, " min")))
    )
  })
  
  output$worst_stops_display <- renderUI({
    s <- get_worst_stops()
    div(class="worst-stops", 
      div(class="worst-stops-title", "Highest Delay Stops"),
      lapply(1:nrow(s), function(i) {
        div(class="stop-item", span(s[i]$stop_name), span(style="color:#E74C3C", paste0("+", round(s[i]$avg_delay, 1), " min")))
      })
    )
  })
  
  observeEvent(input$get_ai_insight, { ai_insight_data(generate_ai_delay_summary()) })
  output$ai_insight_display <- renderUI({
    if(is.null(ai_insight_data())) return(NULL)
    div(class="ai-suggestion", HTML(paste0("<strong>AI Analysis:</strong> ", ai_insight_data())))
  })
  
  output$hour_delay_plot1 <- renderPlot({ hour_plot1 })
  output$hour_delay_plot2 <- renderPlot({ hour_plot2(input$hourwhatRoute) })

  # 3. RIDERSHIP
  observeEvent(input$refresh_ridership, { ridership_data(get_ridership_kpis()) })
  observe({ if(current_view() == "ridership" && is.null(ridership_data())) ridership_data(get_ridership_kpis()) })
  
  output$ridership_kpis <- renderUI({
    d <- ridership_data()
    if(is.null(d)) return(NULL)
    div(class="kpi-grid",
        div(class="kpi-card", div(class="kpi-label", "Total Pax"), div(class="kpi-value", d$total_passengers)),
        div(class="kpi-card", div(class="kpi-label", "Status"), div(class=paste("kpi-value", d$status_class), d$status))
    )
  })
  
  observeEvent(input$get_ridership_insight, { ai_ridership_data(generate_ai_ridership_summary()) })
  output$ai_ridership_display <- renderUI({
    if(is.null(ai_ridership_data())) return(NULL)
    div(class="ai-suggestion", HTML(paste0("<strong>AI Insight:</strong> ", ai_ridership_data())))
  })
  
  output$trendPlot <- renderPlot({ ride.plot2 })
  output$mapTitle <- renderText({ paste("Passengers -", input$ride_date_select) })
  output$dailyMap <- renderPlotly({
    p <- ride.plot1(input$ride_date_select)
    if(is.null(p)) return(NULL)
    ggplotly(p) |> hide_legend()
  })

  # 4. CROWDING
  observeEvent(input$refresh_crowding, { crowding_data(get_crowding_kpis()) })
  observe({ if(current_view() == "crowding" && is.null(crowding_data())) crowding_data(get_crowding_kpis()) })
  
  output$crowding_kpis <- renderUI({
    d <- crowding_data()
    if(is.null(d)) return(NULL)
    div(class="kpi-grid",
        div(class="kpi-card", div(class="kpi-label", "Avg Occupancy"), div(class="kpi-value", d$avg_occupancy)),
        div(class="kpi-card", div(class="kpi-label", "High Risk"), div(class="kpi-value critical", d$risk_zones))
    )
  })
  
  output$crowding_breakdown_display <- renderUI({
    d <- crowding_data()
    if(is.null(d)) return(NULL)
    div(class="breakdown-grid",
        div(class="breakdown-item", div(class="breakdown-label", "Low"), div(class="breakdown-value low", d$low_count)),
        div(class="breakdown-item", div(class="breakdown-label", "Med"), div(class="breakdown-value medium", d$med_count)),
        div(class="breakdown-item", div(class="breakdown-label", "High"), div(class="breakdown-value high", d$high_count))
    )
  })
  
  observeEvent(input$get_crowding_insight, { ai_crowding_data(generate_ai_crowding_summary()) })
  output$ai_crowding_display <- renderUI({
    if(is.null(ai_crowding_data())) return(NULL)
    div(class="ai-suggestion", HTML(paste0("<strong>AI Insight:</strong> ", ai_crowding_data())))
  })
  
  output$crowd_ggplot1 <- renderPlotly({ create_weather_polar_chart() }) 
  output$crowd_ggplot2 <- renderPlot({ 
    ggplot(info[, .N, .(route_id, stop_id, crowding_level)], aes(x = stop_id, y = N, fill = crowding_level)) +
      geom_col(position = "fill") + theme_minimal() + 
      scale_fill_manual(values = c('Low'='#2ca02c', 'Medium'='#ff7f0e', 'High'='#d62728'))
  })

  # 5. WEATHER
  observeEvent(input$refresh_weather, { weather_data(get_weather_kpis()) })
  observe({ if(current_view() == "weather" && is.null(weather_data())) weather_data(get_weather_kpis()) })
  
  output$weather_current_display <- renderUI({
    d <- weather_data()
    if(is.null(d)) return(NULL)
    div(class="weather-current-grid",
        div(class="weather-kpi", div(class="weather-kpi-label", "Condition"), div(class="weather-kpi-value", d$condition)),
        div(class="weather-kpi", div(class="weather-kpi-label", "Temp"), div(class="weather-kpi-value", d$temperature))
    )
  })
  
  observeEvent(input$get_weather_insight, { ai_weather_data(generate_ai_weather_summary()) })
  output$ai_weather_display <- renderUI({
    if(is.null(ai_weather_data())) return(NULL)
    div(class="ai-suggestion", HTML(paste0("<strong>AI Insight:</strong> ", ai_weather_data())))
  })
  
  output$wea_hc_output <- renderHighchart({ create_weather_polar_chart() })
  output$wea_gg_output1 <- renderPlot({ create_weather_ridge_plot() })
  output$wea_gg_output2 <- renderPlot({ create_weather_jitter_plot() })

  # 6. MAP
  observeEvent(input$refresh_live_map, {
    showNotification("Live vehicle locations refreshed (Simulated)", type="message")
  })
  
  output$mapPlotOut <- renderLeaflet({
    makemap(input$whatMapalpha)
  })
}

# Run the application
runApp(shinyApp(ui, server), port = 6120, host = "0.0.0.0")