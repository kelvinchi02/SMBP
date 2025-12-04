library(shiny)
library(plotly)
library(data.table)
library(bsicons) # Required for the metric icons

source("styles.R")

# -------------------------------------------------------------------------
# UI COMPONENT
# -------------------------------------------------------------------------

overview_ui <- function() {
  
  # --- PRE-CALCULATIONS FOR UI ---
  # Safety check: ensure 'info' exists before trying to calculate metrics
  if (!exists("info") || nrow(info) == 0) {
    total_records <- "0"
    time_range <- "N/A"
    date_range <- "N/A"
    days_count <- "0"
  } else {
    total_records <- format(nrow(info), big.mark = ",")
    
    time_range <- paste(
      format(min(info$datetime), "%I %p"), "-", 
      format(max(info$datetime), "%I %p")
    )
    
    date_range <- paste(
      format(min(info$service_date), "%b %d"), "-",
      format(max(info$service_date), "%b %d, %Y")
    )
    
    days_count <- paste(uniqueN(info$service_date), "days")
  }
  # -------------------------------

  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        /* Route items */
        .route-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 3rem; margin-bottom: 0; }
        .route-item { display: flex; align-items: center; gap: 1.2rem; padding-bottom: 1.5rem; border-bottom: 1px solid #f0f0f0; }
        .route-item .route-icon { font-size: 2rem; width: 50px; text-align: center; flex-shrink: 0; }
        .route-item h5 { font-size: 1rem; font-weight: 600; color: #2c3e50; margin: 0 0 0.3rem 0; }
        .route-item p { font-size: 0.85rem; color: #6c757d; margin: 0; }
        
        /* Metrics */
        .metrics-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 3rem; margin-bottom: 0; }
        .metric-item { text-align: center; padding-bottom: 1.5rem; border-bottom: 1px solid #f0f0f0; }
        .metric-item .metric-icon { font-size: 2rem; color: #6c757d; margin-bottom: 0.8rem; }
        .metric-item .metric-value { font-size: 1.5rem; font-weight: 700; color: #2c3e50; margin-bottom: 0.3rem; }
        .metric-item .metric-label { font-size: 0.85rem; font-weight: 600; color: #495057; margin-bottom: 0.3rem; }
        .metric-item .metric-desc { font-size: 0.75rem; color: #6c757d; }
        
        /* KPI section */
        .kpi-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 3rem; }
        .kpi-item { padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
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
        
        # --- SECTION 1: ROUTE DETAILS ---
        div(
          class = "page-section",
          div(class = "section-title", "Route Details"),
          div(
            class = "route-grid",
            
            # Using loop or manual extraction safely
            lapply(1:min(3, nrow(routes)), function(i) {
              div(
                class = "route-item",
                div(class = "route-icon", style = paste0("color: ", routes[i, route_color]), 
                    if(i==1) fa("city") else if(i==2) fa("graduation-cap") else fa("plane-departure")),
                div(
                  class = "route-details",
                  h5(paste("Route", routes[i, route_id], ":", routes[i, route_name])),
                  p(sprintf("%s km | %s stops | %s daily trips", 
                            routes[i, route_length_km], 
                            10, # Placeholder
                            6   # Placeholder
                  ))
                )
              )
            })
          )
        ),
        
        # --- SECTION 2: METRICS ---
        div(
          class = "page-section",
          div(class = "section-title", "System Operations"),
          div(
            class = "metrics-grid",
            div(class = "metric-item", div(class = "metric-icon", bs_icon("stack")), div(class = "metric-value", total_records), div(class = "metric-label", "Total Records"), div(class = "metric-desc", "Comprehensive trip data")),
            div(class = "metric-item", div(class = "metric-icon", bs_icon("clock-fill")), div(class = "metric-value", time_range), div(class = "metric-label", "Service Hours"), div(class = "metric-desc", "Daily operation window")),
            div(class = "metric-item", div(class = "metric-icon", bs_icon("hourglass-split")), div(class = "metric-value", "1 hour"), div(class = "metric-label", "Trip Duration"), div(class = "metric-desc", "Average one-way time")),
            div(class = "metric-item", div(class = "metric-icon", bs_icon("calendar-week-fill")), div(class = "metric-value", days_count), div(class = "metric-label", "Operating Period"), div(class = "metric-desc", date_range))
          )
        ),
        
        # --- SECTION 3: KPI & PLOTS ---
        div(
          class = "page-section",
          div(class = "section-title", "KPI Evaluation"),
          div(
            class = "kpi-grid",
            div(class = "kpi-item", plotOutput("summaryOutputPlot1", height = "500px")),
            div(class = "kpi-item", plotOutput("summaryOutputPlot2", height = "500px")),
            div(
              class = "kpi-item",
              pickerInput(
                inputId = "summaryplot3whatRoute",
                label = "Please select a route:",
                choices = if(exists("final_choices")) final_choices else c("Loading..."),
                choicesOpt = if(exists("final_styles")) list(style = final_styles) else NULL
              ),
              plotlyOutput("summaryOutputPlot3", height = "450px")
            )
          )
        )
      ),
      
      div(class = "page-footer", div(class = "container", p("SmartTransit Analytics Dashboard © 2025")))
    )
  )
}


# -------------------------------------------------------------------------
# SERVER LOGIC (FIXED FUNCTION)
# -------------------------------------------------------------------------

create_crowding_pie <- function(info_df, routes_df, selected_route) {
  
  # 1. Safety Check: If data is missing
  if (is.null(info_df) || nrow(info_df) == 0) {
    return(plotly_empty(type = "pie") %>% layout(title = "No Data Available"))
  }
  
  # 2. Local conversion to data.table
  dt <- as.data.table(info_df)
  
  # 3. DEFENSIVE CODING: Ensure 'crowding_level' exists
  # If Supabase/CSV didn't provide it, we calculate it now to prevent crash
  if (!"crowding_level" %in% names(dt)) {
    dt[, crowding_level := fcase(
      occupancy_rate < 0.5, "Low",
      occupancy_rate < 0.85, "Medium",
      default = "High"
    )]
  }
  
  # 4. Filter Data by Route
  if (!is.null(selected_route) && selected_route != "" && selected_route != "ALL") {
    match_data <- dt[route_id == selected_route]
  } else {
    match_data <- dt
  }
  
  # 5. THE FIX FOR "object 'N' not found":
  # explicitly name the count column 'Count'
  pie_data <- match_data[, .(Count = .N), by = .(crowding_level)]
  
  # 6. Check if filtering resulted in empty data
  if (nrow(pie_data) == 0) {
    return(plotly_empty(type = "pie") %>% layout(title = "No Data for this Route"))
  }
  
  # 7. Define Colors
  colors <- c('Low' = '#2ECC71', 'Medium' = '#F1C40F', 'High' = '#E74C3C')
  
  # 8. Render Plot
  plot_ly(
    data = pie_data, 
    labels = ~crowding_level, 
    values = ~Count,  # Referencing the explicit column 'Count'
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',
    text = ~paste(crowding_level, ": ", Count, " trips"),
    marker = list(colors = colors[pie_data$crowding_level], line = list(color = '#FFFFFF', width = 1))
  ) %>%
    layout(
      title = list(text = paste("Crowding Levels"), font = list(size = 14)),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.1, y = -0.1),
      margin = list(t = 40, b = 40, l = 20, r = 20)
    )
}