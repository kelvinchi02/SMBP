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


source("pre.R")
source("overview.R")
source("map.R")
source("weather.R")
source("crowd.R")
source("ridership.R")
source("hour.R")
source("chat.R")

if (file.exists(".env")) {
  load_dot_env(".env")
}
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
valid_user <- Sys.getenv("LOGIN_USER")
valid_pass <- Sys.getenv("LOGIN_PASS")


nav_card <- function(id, icon_name, title, subtitle) {
  div(
    class = "nav-card",
    onclick = sprintf("Shiny.setInputValue('view_selection', '%s', {priority: 'event'})", id),
    bs_icon(icon_name, size = "2.5rem"),
    h5(title, class = "mt-3"),
    p(subtitle, class = "text-muted")
  )
}



login_ui <- function() {
  div(
    class = "login-container",
    style = "
      background-image: url('bk.webp');
      background-size: cover;
      background-position: center;
      min-height: 100vh;
      display: flex;
      justify-content: center;
      align-items: center;
      position: relative;
      padding: 2rem;
    ",

    div(
      class = "login-box",
      style = "
        background: rgba(255, 255, 255, 0.12);
        backdrop-filter: blur(12px);
        padding: 3rem 2.5rem;
        border-radius: 18px;
        width: 420px;
        text-align: center;
        position: relative;
        z-index: 1;
      ",

      tags$h1("SMART", class = "login-title"),
      tags$h1("BUS", class = "login-title", style = "margin-bottom: 1rem;"),

      tags$h3("Management Platform Login",
              style = "color: white; font-weight: 300; margin-bottom: 2rem;"),

      div(class = "login-label", "Username"),
      textInput("login_user", NULL, value = "admin1",
                placeholder = "Enter username", class = "login-input"),

      div(class = "login-label", "Password"),
      passwordInput("login_pass", NULL, placeholder = "Enter password",
                    class = "login-input"),

      actionButton("login_btn", "Login", class = "btn btn-primary",
                   style = "width: 100%; margin-top: 1.2rem;"),
      uiOutput("login_error")
    )
  )
}

# Home page UI
home_ui <- function() {
  tagList(
    div(
      class = "home-container",
      
      div(
        class = "title-section",
        h1("SMART BUS", class = "main-title"),
        h2("MANAGEMENT PLATFORM", class = "sub-title"),
        p("Select a module to explore detailed insights", class = "title-description")
      ),

      layout_column_wrap(
        width = 1 / 3,
        heights_equal = "row",
        nav_card("overview", "clipboard-data-fill", "Overview", "Key system metrics"),
        nav_card("delay", "clock-history", "Delay & Punctuality", "On-time performance analysis"),
        nav_card("ridership", "graph-up-arrow", "Ridership Trends", "Passenger volume patterns"),
        nav_card("crowding", "people-fill", "Crowding Analysis", "Vehicle load factors"),
        nav_card("weather", "cloud-sun-fill", "Weather Impact", "Correlation with performance"),
        nav_card("map", "geo-alt-fill", "Stops & Map", "Geospatial visualizations")
      ),

      div(
        class = "about-footer",
        h4("About This Platform", class = "about-title"),
        div(
          class = "about-content",
          p("This Smart Bus Management Platform is designed for bus management teams and local transit agencies, not for everyday passengers. It helps operators see how the whole bus network is running in real time, so they can make better decisions about routes, schedules, and capacity."),
          p("Unlike tools like Google Maps that focus on helping riders plan a single trip, our dashboard focuses on system-level operations. It shows wait times, crowding, delays, and line performance across the whole network, so agencies can quickly spot problems and respond."),
          p("Behind the scenes, the website connects to an API that provides real-time (or simulated) bus data, including locations, occupancy, and service reliability. AI features on the dashboard can automatically summarize route performance, suggest where to add or reduce buses, and explain charts in plain language, making data easier to use for non-technical staff.")
        )
      )
    )
  )
}

# Main UI definition
ui <- page_fluid(
  style = "padding: 0;",

  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800&display=swap');

      body {
        font-family: 'Poppins', sans-serif;
        background-color: #f8f9fa;
      }

      /* LOGIN PAGE */
      .login-container::before {
        content: '';
        position: absolute;
        top: 0; right: 0; bottom: 0; left: 0;
        background-color: rgba(0,0,0,0.55);
        z-index: 0;
      }

      .login-title {
        font-size: 4rem;
        font-weight: 800;
        letter-spacing: 0.1em;
        text-transform: uppercase;
        background: linear-gradient(135deg, #ffffff 0%, #d0ecff 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        text-shadow: 0 4px 25px rgba(0, 0, 0, 0.5);
      }

      .login-input {
        background-color: rgba(255, 255, 255, 0.92);
        color: #222 !important;
        border: 1px solid rgba(255, 255, 255, 0.7);
        border-radius: 10px;
        padding: 0.9rem;
        font-size: 1rem;
        margin-bottom: 1rem;
      }

      .login-label {
        color: rgba(255, 255, 255, 0.95);
        font-weight: 600;
        font-size: 1rem;
        margin-bottom: 0.3rem;
        text-align: left;
        width: 100%;
      }

      /* HOME PAGE */
      .home-container {
        background-image: url('bk.webp');
        background-size: cover;
        background-position: center;
        background-attachment: fixed;
        padding: 3rem 2rem 2rem 2rem;
        min-height: 100vh;
        position: relative;
        color: white;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
      }

      .home-container::before {
        content: '';
        position: absolute;
        top: 0; right: 0; bottom: 0; left: 0;
        background-color: rgba(0, 0, 0, 0.5);
        z-index: 0;
      }

      .home-container > * { position: relative; z-index: 1; }

      .title-section {
        text-align: center;
        margin-bottom: 3rem;
        animation: fadeInDown 0.8s ease-out;
      }

      @keyframes fadeInDown {
        from { opacity: 0; transform: translateY(-30px); }
        to { opacity: 1; transform: translateY(0); }
      }

      .main-title {
        font-size: 4rem;
        font-weight: 800;
        letter-spacing: 0.1em;
        margin: 0;
        text-transform: uppercase;
        background: linear-gradient(135deg, #ffffff 0%, #e0f7ff 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }

      .sub-title {
        font-size: 1.8rem;
        font-weight: 300;
        color: rgba(255, 255, 255, 0.95);
      }

      .title-description {
        font-size: 1.1rem;
        font-weight: 400;
        color: rgba(255, 255, 255, 0.85);
      }

      .nav-card {
        padding: 1.5rem;
        text-align: center;
        border-radius: 12px;
        background-color: rgba(255, 255, 255, 0.7);
        cursor: pointer;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
      }

      .nav-card:hover {
        transform: translateY(-8px);
      }

      .about-footer {
        margin-top: 4rem;
        padding-top: 2rem;
        text-align: center;
      }

      .about-content p:last-child { margin-bottom: 0; }

      .custom-card {
        border: 1px solid #e9ecef;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.05);
      }

      .custom-card .card-header {
        background-color: #f8f9fa;
        border-bottom: 1px solid #e9ecef;
      }

      .footer {
        text-align: center;
        padding-top: 1.5rem;
        font-size: 0.9rem;
        color: #6c757d;
      }
    "))
  ),

  uiOutput("gate")
)



# Server logic
server <- function(input, output, session) {

  # ---------------- LOGIN SYSTEM ----------------
  logged_in <- reactiveVal(FALSE)

  observeEvent(input$login_btn, {
    ok_user <- tolower(input$login_user) == tolower(valid_user)
    ok_pass <- input$login_pass == valid_pass

    if (ok_user && ok_pass) {
      logged_in(TRUE)
    } else {
      output$login_error <- renderUI({
        div(style = "color:red; margin-top:10px;", "Invalid username or password.")
      })
    }
  })

  # Gate: show login page first, then app
  output$gate <- renderUI({
    if (!logged_in()) {
      login_ui()
    } else {
      tagList(
        uiOutput("main_content"),
        chat_ui()
      )
    }
  })

 
  current_view <- reactiveVal("home")

  observeEvent(input$view_selection, {
    current_view(input$view_selection)
  })

  observeEvent(input$back_to_home, {
    current_view("home")
  })

  output$main_content <- renderUI({
    switch(current_view(),
      "home" = home_ui(),
      "overview" = overview_ui(),
      "delay" = delay_ui(),
      "ridership" = rider_ui(),
      "crowding" = crowd_ui(),
      "weather" = weather_ui(),
      "map" = map_ui()
    )
  })

  # Chat handler
  observeEvent(input$chat_message, {
    msg <- input$chat_message$text
    resp <- call_chatgpt(msg, OPENAI_API_KEY)
    session$sendCustomMessage("chat_response", resp)
  })

  # Overview outputs
  output$summaryOutputPlot1 <- renderPlot({ summary.plot1 })
  output$summaryOutputPlot2 <- renderPlot({ summary.plot2 })

  output$summaryOutputPlot3 <- renderPlotly({
    crowding_data <- info[, .N, .(route_id, crowding_level)]
    create_crowding_pie(crowding_data, routes, input$summaryplot3whatRoute)
  })

  # Map outputs
  output$mapPlotOut <- renderLeaflet({
    makemap(input$whatMapalpha)
  })

  # Weather
  output$wea_hc_output <- renderHighchart({ table.wea })
  output$wea_gg_output1 <- renderPlot({ plot.wea1 })
  output$wea_gg_output2 <- renderPlot({ plot.wea2 })

  # Crowding
  output$crowd_ggplot1 <- renderPlotly({ crowd.plot2 })
  output$crowd_ggplot2 <- renderPlot({ crowd.plot1 })

  # Ridership
  output$trendPlot <- renderPlot({ ride.plot2 })

  output$mapTitle <- renderText({
    paste("Passengers onboard -", input$ride_date_select)
  })

  output$dailyMap <- renderPlotly({
    inter.p <- ride.plot1(input$ride_date_select)
    ggplotly(inter.p) |> hide_legend()
  })

  # Hourly delay
  output$hour_delay_plot1 <- renderPlot({ hour_plot1 })
  output$hour_delay_plot2 <- renderPlot({
    hour_plot2(input$hourwhatRoute)
  })
}


shinyApp(ui, server)