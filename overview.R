source("styles.R")

# Overview page UI
overview_ui <- function() {
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
        
        .route-item .route-icon.route-a { color: #1f77b4; }
        .route-item .route-icon.route-b { color: #ff7f0e; }
        .route-item .route-icon.route-c { color: #2ca02c; }
        
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
            div(
              class = "route-item",
              div(class = "route-icon route-a", fa("city")),
              div(
                class = "route-details",
                h5("Route A: Downtown Loop"),
                p("12.3 km | 10 stops | 6 daily trips")
              )
            ),
            div(
              class = "route-item",
              div(class = "route-icon route-b", fa("graduation-cap")),
              div(
                class = "route-details",
                h5("Route B: University Express"),
                p("18.7 km | 10 stops | 6 daily trips")
              )
            ),
            div(
              class = "route-item",
              div(class = "route-icon route-c", fa("plane-departure")),
              div(
                class = "route-details",
                h5("Route C: Airport Connector"),
                p("25.4 km | 10 stops | 6 daily trips")
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
              div(class = "metric-value", "2,520"),
              div(class = "metric-label", "Total Records"),
              div(class = "metric-desc", "Comprehensive trip data")
            ),
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("clock-fill")),
              div(class = "metric-value", "6 AM - 4 PM"),
              div(class = "metric-label", "Service Hours"),
              div(class = "metric-desc", "2-hour frequency")
            ),
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("hourglass-split")),
              div(class = "metric-value", "1 hour"),
              div(class = "metric-label", "Trip Duration"),
              div(class = "metric-desc", "One-way journey time")
            ),
            div(
              class = "metric-item",
              div(class = "metric-icon", bs_icon("calendar-week-fill")),
              div(class = "metric-value", "7 days"),
              div(class = "metric-label", "Operating Period"),
              div(class = "metric-desc", "Oct 20-26, 2025")
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
                choices = final_choices,
                choicesOpt = list(style = final_styles)
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