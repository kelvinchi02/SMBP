source("styles.R")

# Overview page UI
overview_ui <- function() {
  
  # --- PRE-CALCULATIONS FOR UI ---
  # Calculate dynamic metrics so UI updates automatically with DB changes
  total_records <- format(nrow(info), big.mark = ",")
  
  # Get time range (e.g., "6 AM - 4 PM")
  time_range <- paste(
    format(min(info$datetime), "%I %p"), "-", 
    format(max(info$datetime), "%I %p")
  )
  
  # Get date range (e.g., "Oct 20-26, 2025")
  date_range <- paste(
    format(min(info$service_date), "%b %d"), "-",
    format(max(info$service_date), "%b %d, %Y")
  )
  
  # -------------------------------

  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        /* Route items */
        .route-grid {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 3rem;
          margin-bottom: 0;
        }
        
        .route-item {
          display: flex;
          align-items: center;
          gap: 1.2rem;
          padding-bottom: 1.5rem;
          border-bottom: 1px solid #f0f0f0;
        }
        
        .route-item .route-icon {
          font-size: 2rem;
          width: 50px;
          text-align: center;
          flex-shrink: 0;
        }
        
        .route-item h5 {
          font-size: 1rem;
          font-weight: 600;
          color: #2c3e50;
          margin: 0 0 0.3rem 0;
        }
        
        .route-item p {
          font-size: 0.85rem;
          color: #6c757d;
          margin: 0;
        }
        
        /* Metrics */
        .metrics-grid {
          display: grid;
          grid-template-columns: repeat(4, 1fr);
          gap: 3rem;
          margin-bottom: 0;
        }
        
        .metric-item {
          text-align: center;
          padding-bottom: 1.5rem;
          border-bottom: 1px solid #f0f0f0;
        }
        
        .metric-item .metric-icon {
          font-size: 2rem;
          color: #6c757d;
          margin-bottom: 0.8rem;
        }
        
        .metric-item .metric-value {
          font-size: 1.5rem;
          font-weight: 700;
          color: #2c3e50;
          margin-bottom: 0.3rem;
        }
        
        .metric-item .metric-label {
          font-size: 0.85rem;
          font-weight: 600;
          color: #495057;
          margin-bottom: 0.3rem;
        }
        
        .metric-item .metric-desc {
          font-size: 0.75rem;
          color: #6c757d;
        }
        
        /* KPI section */
        .kpi-grid {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 3rem;
        }
        
        .kpi-item {
          padding-bottom: 2rem;
          border-bottom: 1px solid #f0f0f0;
        }
      "))
    ),
    
    div(
      class = "page-wrapper",
      
      div(
        class = "page-header",
        div(
          class = "container",
          h2("Overview Dashboard"),
          back_button()
        )
      ),
      
      div(
        class = "page-content",
        
        div(
          class = "page-section",
          div(class = "section-title", "Route Details"),
          div(
            class = "route-grid",
            
            # --- DYNAMIC ROUTE GENERATION ---
            # Using 'routes' object from pre.R to fill details
            
            # Route A
            div(
              class = "route-item",
              div(class = "route-icon", style = paste0("color: ", routes[1, route_color]), fa("city")),
              div(
                class = "route-details",
                h5(paste("Route", routes[1, route_id], ":", routes[1, route_name])),
                p(sprintf("%s km | %s stops | %s daily trips", 
                          routes[1, route_length_km], 
                          10, # If you have stop counts in pre.R, replace this
                          6   # If you have trip counts in pre.R, replace this
                ))
              )
            ),
            
            # Route B
            div(
              class = "route-item",
              div(class = "route-icon", style = paste0("color: ", routes[2, route_color]), fa("graduation-cap")),
              div(
                class = "route-details",
                h5(paste("Route", routes[2, route_id], ":", routes[2, route_name])),
                p(sprintf("%s km | %s stops | %s daily trips", 
                          routes[2, route_length_km], 10, 6))
              )
            ),
            
            # Route C
            div(
              class = "route-item",
              div(class = "route-icon", style = paste0("color: ", routes[3, route_color]), fa("plane-departure")),
              div(
                class = "route-details",
                h5(paste("Route", routes[3, route_id], ":", routes[3, route_name])),
                p(sprintf("%s km | %s stops | %s daily trips", 
                          routes[3, route_length_km], 10, 6))
              )
            )
          )
        ),
        
        div(
          class = "page-section",
          div(class = "section-title", "System Operations"),
          div(
            class = "metrics-grid",
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("stack")),
              div(class = "metric-value", total_records), # Dynamic
              div(class = "metric-label", "Total Records"),
              div(class = "metric-desc", "Comprehensive trip data")
            ),
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("clock-fill")),
              div(class = "metric-value", time_range), # Dynamic
              div(class = "metric-label", "Service Hours"),
              div(class = "metric-desc", "Daily operation window")
            ),
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("hourglass-split")),
              div(class = "metric-value", "1 hour"), # Keep hardcoded if constant
              div(class = "metric-label", "Trip Duration"),
              div(class = "metric-desc", "Average one-way time")
            ),
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("calendar-week-fill")),
              div(class = "metric-value", paste(uniqueN(info$service_date), "days")), # Dynamic
              div(class = "metric-label", "Operating Period"),
              div(class = "metric-desc", date_range) # Dynamic
            )
          )
        ),
        
        div(
          class = "page-section",
          div(class = "section-title", "KPI Evaluation"),
          div(
            class = "kpi-grid",
            div(
              class = "kpi-item",
              plotOutput("summaryOutputPlot1", height = "500px")
            ),
            div(
              class = "kpi-item",
              plotOutput("summaryOutputPlot2", height = "500px")
            ),
            div(
              class = "kpi-item",
              pickerInput(
                inputId = "summaryplot3whatRoute",
                label = "Please select a route:",
                choices = final_choices, # From pre.R
                choicesOpt = list(style = final_styles) # From pre.R
              ),
              plotlyOutput("summaryOutputPlot3", height = "450px")
            )
          )
        )
      ),
      
      div(
        class = "page-footer",
        div(
          class = "container",
          p("SmartTransit Analytics Dashboard © 2025")
        )
      )
    )
  )
}