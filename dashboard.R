# Dashboard UI Structure (Sidebar + Topbar)
dashboard_ui <- function(user_name = "Administrator") {
  tagList(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap');
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: 'Poppins', sans-serif; background-color: #f8f9fa; overflow-x: hidden; }
        .dashboard-container { display: flex; min-height: 100vh; background-color: #f8f9fa; }
        
        /* Sidebar */
        .sidebar { width: 260px; background-color: #ffffff; border-right: 1px solid #e9ecef; position: fixed; height: 100vh; left: 0; top: 0; z-index: 1000; }
        .sidebar-header { padding: 30px 25px; border-bottom: 1px solid #e9ecef; }
        .sidebar-logo { font-size: 1.4rem; font-weight: 700; color: #2c3e50; text-align: center; }
        .sidebar-menu { padding: 20px 0; }
        .menu-item { display: flex; align-items: center; padding: 14px 25px; color: #6c757d; cursor: pointer; transition: all 0.3s; border-left: 3px solid transparent; }
        .menu-item:hover, .menu-item.active { background-color: #f8f9fa; color: #2c3e50; border-left-color: #2c3e50; font-weight: 500; }
        .menu-item i { font-size: 1.2rem; width: 24px; margin-right: 12px; }
        
        /* Topbar */
        .topbar { position: fixed; top: 0; left: 260px; right: 0; height: 70px; background-color: #ffffff; border-bottom: 1px solid #e9ecef; display: flex; align-items: center; justify-content: space-between; padding: 0 35px; z-index: 999; }
        .topbar-title { font-size: 1.5rem; font-weight: 600; color: #2c3e50; }
        .topbar-user { display: flex; align-items: center; gap: 15px; }
        .user-name { font-size: 0.9rem; font-weight: 500; color: #2c3e50; }
        .user-role { font-size: 0.75rem; color: #6c757d; }
        .logout-btn { padding: 8px 18px; font-size: 0.85rem; color: #6c757d; border: 1px solid #dee2e6; background: transparent; border-radius: 8px; cursor: pointer; transition: all 0.3s; }
        .logout-btn:hover { color: #e74c3c; border-color: #e74c3c; background-color: #fff5f5; }
        
        /* Main Content Area */
        .main-content { margin-left: 260px; margin-top: 70px; padding: 0; flex: 1; min-height: calc(100vh - 70px); }
        
        /* Carousel Styles */
        .carousel-container { position: relative; height: calc(100vh - 70px); overflow: hidden; background: #fff; }
        .carousel-slide { display: none; width: 100%; height: 100%; }
        .carousel-slide.active { display: block; animation: fadeIn 0.5s ease-in-out; }
        .carousel-slide img { width: 100%; height: 100%; object-fit: cover; }
        
        /* Placeholder for missing images */
        .slide-placeholder { width: 100%; height: 100%; background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); display: flex; align-items: center; justify-content: center; flex-direction: column; color: #5f6c7b; }
        .slide-placeholder i { font-size: 4rem; margin-bottom: 1rem; opacity: 0.5; }
        
        .carousel-overlay { position: absolute; top: 15%; right: 5%; width: 55%; z-index: 10; pointer-events: none; }
        .feature-item { display: none; flex-direction: row-reverse; align-items: center; color: #fff; font-size: 3.5rem; font-weight: 700; padding: 10px; text-shadow: 2px 2px 8px rgba(0,0,0,0.6); text-align: right; }
        .feature-item.active { display: flex; animation: fadeInRight 0.8s ease-out; }
        .feature-icon { width: 80px; height: 80px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 20px; display: flex; align-items: center; justify-content: center; font-size: 2.5rem; margin-left: 25px; box-shadow: 0 8px 20px rgba(102,126,234,0.5); }
        
        @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }
        @keyframes fadeInRight { from { opacity: 0; transform: translateX(100px); } to { opacity: 1; transform: translateX(0); } }
      "))
    ),
    div(
      class = "dashboard-container",
      
      # Sidebar
      div(class = "sidebar",
        div(class = "sidebar-header", div(class = "sidebar-logo", "Smart Bus Platform")),
        div(class = "sidebar-menu", id = "sidebar-menu",
          div(class = "menu-item active", id = "menu-dashboard", onclick = "Shiny.setInputValue('nav_selection', 'dashboard', {priority: 'event'}); updateActiveMenu('dashboard');", bs_icon("house-fill"), span(class="menu-item-text", "Dashboard")),
          div(class = "menu-item", id = "menu-overview", onclick = "Shiny.setInputValue('nav_selection', 'overview', {priority: 'event'}); updateActiveMenu('overview');", bs_icon("clipboard-data-fill"), span(class="menu-item-text", "Overview")),
          div(class = "menu-item", id = "menu-delay", onclick = "Shiny.setInputValue('nav_selection', 'delay', {priority: 'event'}); updateActiveMenu('delay');", bs_icon("clock-history"), span(class="menu-item-text", "Delay & Punctuality")),
          div(class = "menu-item", id = "menu-ridership", onclick = "Shiny.setInputValue('nav_selection', 'ridership', {priority: 'event'}); updateActiveMenu('ridership');", bs_icon("graph-up-arrow"), span(class="menu-item-text", "Ridership Trends")),
          div(class = "menu-item", id = "menu-crowding", onclick = "Shiny.setInputValue('nav_selection', 'crowding', {priority: 'event'}); updateActiveMenu('crowding');", bs_icon("people-fill"), span(class="menu-item-text", "Crowding Analysis")),
          div(class = "menu-item", id = "menu-weather", onclick = "Shiny.setInputValue('nav_selection', 'weather', {priority: 'event'}); updateActiveMenu('weather');", bs_icon("cloud-sun-fill"), span(class="menu-item-text", "Weather Impact")),
          div(class = "menu-item", id = "menu-map", onclick = "Shiny.setInputValue('nav_selection', 'map', {priority: 'event'}); updateActiveMenu('map');", bs_icon("geo-alt-fill"), span(class="menu-item-text", "Stops & Map"))
        ),
        tags$script(HTML("
          function updateActiveMenu(page) {
            $('.menu-item').removeClass('active');
            $('#menu-' + page).addClass('active');
          }
        "))
      ),
      
      # Main Content Area
      div(style = "flex: 1;",
        # Top Bar
        div(class = "topbar",
          uiOutput("topbar_title_dynamic"),
          div(class = "topbar-user",
            div(class = "user-info", div(class = "user-name", user_name), div(class = "user-role", "System Administrator")),
            actionButton("logout_btn", "Logout", class = "logout-btn")
          )
        ),
        # Content
        div(class = "main-content", uiOutput("dashboard_content"))
      )
    )
  )
}

# -------------------------------------------------------------------------
# CAROUSEL CONTENT (With Image Existence Check)
# -------------------------------------------------------------------------

dashboard_home_content <- function() {
  
  # Helper to check image existence
  render_slide_image <- function(filename) {
    # Check physical path
    file_path <- file.path("www", "index", filename)
    if (file.exists(file_path)) {
      return(tags$img(src = paste0("index/", filename)))
    } else {
      # Fallback UI if image is missing
      return(div(class = "slide-placeholder", 
                 bs_icon("image", size = "3rem"), 
                 h4("Image not found"),
                 p(paste("Expected:", filename))
      ))
    }
  }

  tagList(
    div(class = "carousel-container",
      div(id = "carousel",
        div(class = "carousel-slide active", `data-slide`="1", render_slide_image("1.png")), 
        div(class = "carousel-slide", `data-slide`="2", render_slide_image("2.png")),
        div(class = "carousel-slide", `data-slide`="3", render_slide_image("3.png"))
      ),
      div(class = "carousel-overlay",
        div(class = "carousel-features",
          div(class = "feature-list",
            div(class = "feature-item active", `data-slide`="1", tags$span(class="feature-icon", HTML("✨")), tags$span("Provides a clear overview of bus operations")),
            div(class = "feature-item", `data-slide`="2", tags$span(class="feature-icon", HTML("⚡")), tags$span("Enables faster decision-making via AI insights")),
            div(class = "feature-item", `data-slide`="3", tags$span(class="feature-icon", HTML("🚀")), tags$span("Real-time passenger flow & vehicle status"))
          )
        )
      )
    ),
    tags$script(HTML("
      $(document).ready(function() {
        var currentSlide = 1;
        function showSlide(n) {
          $('.carousel-slide, .feature-item').removeClass('active');
          $('[data-slide=\"' + n + '\"]').addClass('active');
          currentSlide = n;
        }
        setInterval(function() {
          currentSlide = currentSlide >= 3 ? 1 : currentSlide + 1;
          showSlide(currentSlide);
        }, 5000);
      });
    "))
  )
}