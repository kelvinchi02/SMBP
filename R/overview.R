library(shiny)
library(plotly)
library(data.table)
library(bsicons)
library(scales) # Added for percent_format



# -------------------------------------------------------------------------
# UI COMPONENT
# -------------------------------------------------------------------------

overview_ui <- function() {
  
  # Note: Top-level metrics here (Total Records etc.) remain static based on initial load
  # to allow auto-refresh for these text values, they would need to be converted to uiOutputs.
  # For now, we focus on the plots updating.
  if (!exists("info") || nrow(info) == 0) {
    total_records <- "0"
    time_range <- "N/A"
    date_range <- "N/A"
    days_count <- "0"
  } else {
    total_records <- format(nrow(info), big.mark = ",")
    time_range <- paste(format(min(info$datetime), "%I %p"), "-", format(max(info$datetime), "%I %p"))
    date_range <- paste(format(min(info$service_date), "%b %d"), "-", format(max(info$service_date), "%b %d, %Y"))
    days_count <- paste(uniqueN(info$service_date), "days")
  }

  tagList(
    tags$head(
      tags$style(common_styles),
      tags$style(HTML("
        .route-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 3rem; margin-bottom: 0; }
        .route-item { display: flex; align-items: center; gap: 1.2rem; padding-bottom: 1.5rem; border-bottom: 1px solid #f0f0f0; }
        .route-item .route-icon { font-size: 2rem; width: 50px; text-align: center; flex-shrink: 0; }
        .route-item h5 { font-size: 1rem; font-weight: 600; color: #2c3e50; margin: 0 0 0.3rem 0; }
        .route-item p { font-size: 0.85rem; color: #6c757d; margin: 0; }
        .metrics-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 3rem; margin-bottom: 0; }
        .metric-item { text-align: center; padding-bottom: 1.5rem; border-bottom: 1px solid #f0f0f0; }
        .metric-item .metric-icon { font-size: 2rem; color: #6c757d; margin-bottom: 0.8rem; }
        .metric-item .metric-value { font-size: 1.5rem; font-weight: 700; color: #2c3e50; margin-bottom: 0.3rem; }
        .metric-item .metric-label { font-size: 0.85rem; font-weight: 600; color: #495057; margin-bottom: 0.3rem; }
        .metric-item .metric-desc { font-size: 0.75rem; color: #6c757d; }
        .kpi-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 3rem; }
        .kpi-item { padding-bottom: 2rem; border-bottom: 1px solid #f0f0f0; }
      "))
    ),
    
    div(
      class = "page-wrapper",
      div(class = "page-header", div(class = "container", h2("Overview Dashboard"), back_button())),
      div(
        class = "page-content",
        div(
          class = "page-section",
          div(class = "section-title", "Route Details"),
          div(class = "route-grid",
            lapply(1:min(3, nrow(routes)), function(i) {
              div(class = "route-item",
                div(class = "route-icon", style = paste0("color: ", routes[i, route_color]), 
                    if(i==1) fa("city") else if(i==2) fa("graduation-cap") else fa("plane-departure")),
                div(class = "route-details",
                  h5(paste("Route", routes[i, route_id], ":", routes[i, route_name])),
                  p(sprintf("%s km | %s stops | %s daily trips", routes[i, route_length_km], 10, 6))
                )
              )
            })
          )
        ),
        div(
          class = "page-section",
          div(class = "section-title", "System Operations"),
          div(class = "metrics-grid",
            div(class = "metric-item", div(class = "metric-icon", bs_icon("stack")), div(class = "metric-value", total_records), div(class = "metric-label", "Total Records"), div(class = "metric-desc", "Comprehensive trip data")),
            div(class = "metric-item", div(class = "metric-icon", bs_icon("clock-fill")), div(class = "metric-value", time_range), div(class = "metric-label", "Service Hours"), div(class = "metric-desc", "Daily operation window")),
            div(class = "metric-item", div(class = "metric-icon", bs_icon("hourglass-split")), div(class = "metric-value", "1 hour"), div(class = "metric-label", "Trip Duration"), div(class = "metric-desc", "Average one-way time")),
            div(class = "metric-item", div(class = "metric-icon", bs_icon("calendar-week-fill")), div(class = "metric-value", days_count), div(class = "metric-label", "Operating Period"), div(class = "metric-desc", date_range))
          )
        ),
        div(
          class = "page-section",
          div(class = "section-title", "KPI Evaluation"),
          div(class = "kpi-grid",
            div(class = "kpi-item", plotOutput("summaryOutputPlot1", height = "500px")),
            div(class = "kpi-item", plotOutput("summaryOutputPlot2", height = "500px")),
            div(class = "kpi-item",
              pickerInput(inputId = "summaryplot3whatRoute", label = "Please select a route:",
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
# SERVER LOGIC (DYNAMIC FUNCTIONS)
# -------------------------------------------------------------------------

# 1. Occupancy Boxplot (Replaces summary.plot1)
create_occupancy_box <- function(data, routes_ref) {
  ggplot(data, aes(y = factor(route_id, levels = routes_ref$route_id), x = occupancy_rate, color = route_id)) +
    geom_boxplot(aes(fill = after_scale(alpha(color, 0.3))), outlier.shape = 21, outlier.size = 2, width = 0.6) +
    scale_color_manual(values = setNames(routes_ref[, route_color], routes_ref[, route_id]), labels = setNames(routes_ref[, route_name], routes_ref[, route_id])) +
    scale_x_continuous(name = "Occupancy Rate", labels = scales::percent_format()) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(title = "Occupancy Rate Distribution", subtitle = "Comparing passenger load across routes (Live)", x = "Occupancy %") +
    theme_bw() + 
    theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
}

# 2. Delay Jitter Plot (Replaces summary.plot2)
create_delay_jitter <- function(data, routes_ref) {
  ggplot(data, aes(x = route_id, y = delay_min)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_jitter(aes(color = delay_category), width = 0.2, alpha = 0.4, size = 2) +
    scale_color_manual(values = c("Delayed" = "#E74C3C", "Early" = "#2ECC71", "On-time" = "#3498DB"), name = "Status") +
    scale_x_discrete(name = "Transit Route", labels = setNames(routes_ref$route_name, routes_ref$route_id)) +
    labs(title = "Transit Delays Overview", subtitle = "Real-time delay distribution", y = "Delay (min)") +
    theme_bw() + 
    theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
}

# 3. Crowding Pie Chart (Refined for safety)
create_crowding_pie <- function(info_df, routes_df, selected_route) {
  
  if (is.null(info_df) || nrow(info_df) == 0) return(plotly_empty(type = "pie") %>% layout(title = "No Data Available"))
  
  dt <- as.data.table(info_df)
  
  # Ensure crowding_level exists (since this is now calculated in server.R)
  if (!"crowding_level" %in% names(dt)) {
    dt[, crowding_level := data.table::fcase(
      occupancy_rate < 0.5, "Low",
      occupancy_rate < 0.85, "Medium",
      default = "High"
    )]
  }
  
  # Filter logic
  if (!is.null(selected_route) && selected_route != "" && selected_route != "ALL") {
    match_data <- dt[route_id == selected_route]
    main_title <- paste("Route", selected_route)
  } else {
    match_data <- dt
    main_title <- "All Routes"
  }
  
  # Aggregation with explicit column name 'Count'
  pie_data <- match_data[, .(Count = .N), by = .(crowding_level)]
  
  if (nrow(pie_data) == 0) return(plotly_empty(type = "pie") %>% layout(title = "No Data"))
  
  # Plot
  plot_ly(pie_data, labels = ~crowding_level, values = ~Count, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',
          text = ~paste(crowding_level, ": ", Count, " trips"),
          marker = list(colors = c('Low' = '#2ECC71', 'Medium' = '#F1C40F', 'High' = '#E74C3C')[pie_data$crowding_level])) %>%
    layout(title = list(text = main_title, font = list(size = 14)))
}