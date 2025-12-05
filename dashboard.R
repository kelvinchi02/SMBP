library(shiny)
library(bsicons)

# -------------------------------------------------------------------------
# DASHBOARD HOME (Landing Page)
# -------------------------------------------------------------------------

dashboard_home_content <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .welcome-banner {
          background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
          color: white; padding: 3rem 2rem; border-radius: 12px; margin-bottom: 2rem;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        .welcome-title { font-size: 2.5rem; font-weight: 700; margin-bottom: 0.5rem; }
        .welcome-subtitle { font-size: 1.1rem; opacity: 0.9; font-weight: 300; }
        
        .quick-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 2rem; }
        .quick-card { 
          background: white; padding: 2rem; border-radius: 12px; 
          border: 1px solid #e9ecef; text-align: center;
          transition: transform 0.2s, box-shadow 0.2s;
          cursor: pointer;
        }
        .quick-card:hover { transform: translateY(-5px); box-shadow: 0 10px 20px rgba(0,0,0,0.08); }
        .quick-icon { font-size: 2.5rem; margin-bottom: 1rem; color: #2c3e50; }
        .quick-title { font-size: 1.1rem; font-weight: 600; color: #2c3e50; margin-bottom: 0.5rem; }
        .quick-desc { font-size: 0.85rem; color: #6c757d; line-height: 1.5; }
      "))
    ),
    
    div(class = "page-wrapper",
      div(class = "page-content",
        
        # 1. Welcome Banner
        div(class = "welcome-banner",
          div(class = "welcome-title", "Smart Transit Operations Center"),
          div(class = "welcome-subtitle", "Real-time AI monitoring and schedule optimization platform")
        ),
        
        # 2. Quick Access Grid
        div(class = "quick-grid",
          # Card 1: Map
          div(class = "quick-card", onclick = "Shiny.setInputValue('nav_selection', 'map')",
            div(class = "quick-icon", "🗺️"),
            div(class = "quick-title", "Live Map"),
            div(class = "quick-desc", "Track vehicle locations and route status in real-time.")
          ),
          
          # Card 2: Delays
          div(class = "quick-card", onclick = "Shiny.setInputValue('nav_selection', 'delay')",
            div(class = "quick-icon", "⏱️"),
            div(class = "quick-title", "Delay Analysis"),
            div(class = "quick-desc", "Identify bottlenecks and monitor schedule adherence.")
          ),
          
          # Card 3: Scheduler
          div(class = "quick-card", onclick = "Shiny.setInputValue('nav_selection', 'scheduler')",
            div(class = "quick-icon", "📅"),
            div(class = "quick-title", "AI Scheduler"),
            div(class = "quick-desc", "Review and approve AI-suggested schedule optimizations.")
          )
        )
      )
    )
  )
}

# -------------------------------------------------------------------------
# UI DEFINITION (Dashboard UI Shell)
# -------------------------------------------------------------------------

dashboard_ui <- function(user_name = "Admin") {
  tagList(
    tags$head(
      tags$style(HTML("
        :root { --sidebar-width: 260px; --topbar-height: 70px; }
        body { background-color: #f8f9fa; font-family: 'Segoe UI', system-ui, sans-serif; }
        
        /* Sidebar */
        .sidebar {
          position: fixed; top: 0; left: 0; height: 100vh; width: var(--sidebar-width);
          background-color: #2c3e50; color: white; padding-top: 0; z-index: 100;
          display: flex; flex-direction: column;
        }
        .sidebar-brand {
          height: var(--topbar-height); display: flex; align-items: center; padding: 0 1.5rem;
          font-size: 1.25rem; font-weight: 700; background-color: #1a252f; border-bottom: 1px solid #34495e;
        }
        .nav-menu { flex: 1; padding: 1.5rem 0; overflow-y: auto; }
        .nav-item {
          padding: 0.8rem 1.5rem; color: rgba(255,255,255,0.7); cursor: pointer;
          display: flex; align-items: center; transition: all 0.2s; font-size: 0.95rem; text-decoration: none;
        }
        .nav-item:hover, .nav-item.active { background-color: #34495e; color: white; border-left: 4px solid #3498db; }
        .nav-icon { width: 24px; margin-right: 12px; text-align: center; }
        
        .user-profile {
          padding: 1.5rem; background-color: #1a252f; border-top: 1px solid #34495e;
          display: flex; align-items: center; gap: 10px;
        }
        .user-avatar {
          width: 36px; height: 36px; background-color: #3498db; border-radius: 50%;
          display: flex; align-items: center; justify-content: center; font-weight: 600;
        }
        .user-info { font-size: 0.85rem; }
        .user-name { font-weight: 600; color: white; }
        .user-role { color: rgba(255,255,255,0.5); font-size: 0.75rem; }
        
        /* Main Content */
        .main-content { margin-left: var(--sidebar-width); min-height: 100vh; }
        .topbar {
          height: var(--topbar-height); background: white; border-bottom: 1px solid #e9ecef;
          display: flex; align-items: center; justify-content: space-between; padding: 0 2rem;
          position: sticky; top: 0; z-index: 99;
        }
      "))
    ),
    
    # JavaScript for Navigation
    tags$script(HTML("
      function updateNav(id) {
        Shiny.setInputValue('nav_selection', id);
        document.querySelectorAll('.nav-item').forEach(el => el.classList.remove('active'));
        document.getElementById('nav-' + id).classList.add('active');
      }
      $(document).on('shiny:sessioninitialized', function() {
        updateNav('dashboard');
      });
    ")),
    
    div(
      class = "sidebar",
      div(class = "sidebar-brand", icon("bus"), span(style="margin-left: 10px;", "SmartTransit")),
      div(
        class = "nav-menu",
        tags$a(id="nav-dashboard", class="nav-item", onclick="updateNav('dashboard')", icon("home", class="nav-icon"), "Dashboard"),
        tags$a(id="nav-overview", class="nav-item", onclick="updateNav('overview')", icon("chart-pie", class="nav-icon"), "Overview"),
        tags$a(id="nav-map", class="nav-item", onclick="updateNav('map')", icon("map-marked-alt", class="nav-icon"), "Live Map"),
        tags$a(id="nav-delay", class="nav-item", onclick="updateNav('delay')", icon("clock", class="nav-icon"), "Delay Analysis"),
        tags$a(id="nav-crowding", class="nav-item", onclick="updateNav('crowding')", icon("users", class="nav-icon"), "Crowding"),
        tags$a(id="nav-ridership", class="nav-item", onclick="updateNav('ridership')", icon("chart-line", class="nav-icon"), "Ridership"),
        tags$a(id="nav-weather", class="nav-item", onclick="updateNav('weather')", icon("cloud-sun", class="nav-icon"), "Weather Impact"),
        tags$a(id="nav-scheduler", class="nav-item", onclick="updateNav('scheduler')", icon("calendar-check", class="nav-icon"), "AI Scheduler")
      ),
      div(
        class = "user-profile",
        div(class = "user-avatar", substr(user_name, 1, 1)),
        div(class = "user-info",
            div(class = "user-name", user_name),
            div(class = "user-role", "Operations Manager")
        ),
        actionButton("logout_btn", "", icon = icon("sign-out-alt"), style="background:none; border:none; color:#e74c3c; margin-left:auto;")
      )
    ),
    
    div(
      class = "main-content",
      div(class = "topbar", uiOutput("topbar_title_dynamic")),
      uiOutput("dashboard_content")
    )
  )
}