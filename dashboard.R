library(shiny)
library(bsicons)

# -------------------------------------------------------------------------
# UI DEFINITION
# -------------------------------------------------------------------------

scheduler_ui <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .schedule-board { background: white; padding: 20px; border-radius: 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 30px; }
        .board-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; border-bottom: 1px solid #eee; padding-bottom: 10px; }
        .board-title { font-size: 1.1rem; font-weight: 700; color: #2c3e50; }
        
        .command-center { display: grid; grid-template-columns: 1.2fr 1fr 0.8fr; gap: 25px; }
        .command-col { background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); height: 100%; display: flex; flex-direction: column; }
        .col-header { font-size: 0.95rem; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 15px; display: flex; align-items: center; gap: 10px; }
        
        .ai-insight-box { flex: 1; background: #f8f9fa; border-radius: 8px; padding: 15px; font-size: 0.95rem; line-height: 1.6; color: #2c3e50; border-left: 4px solid #3498db; }
        .proposal-card { flex: 1; background: #fff; border: 1px solid #e9ecef; border-radius: 8px; padding: 20px; text-align: center; display: flex; flex-direction: column; justify-content: center; }
        .proposal-time { font-size: 2.5rem; font-weight: 700; color: #27ae60; margin: 10px 0; }
        .proposal-route { font-size: 1.2rem; font-weight: 600; color: #2c3e50; }
        .proposal-impact { font-size: 0.85rem; color: #7f8c8d; margin-top: 5px; }
        
        .action-area { flex: 1; display: flex; flex-direction: column; justify-content: center; align-items: center; gap: 15px; }
        .btn-scan { width: 100%; background-color: #34495e; color: white; font-weight: 600; padding: 12px; border: none; }
        .btn-confirm { width: 100%; background-color: #27ae60; color: white; font-weight: 700; font-size: 1.1rem; padding: 15px; border: none; box-shadow: 0 4px 6px rgba(39, 174, 96, 0.2); }
        .btn-dismiss { width: 100%; background-color: white; color: #e74c3c; border: 1px solid #e74c3c; font-weight: 600; }
        .btn-disabled { opacity: 0.6; cursor: not-allowed; background-color: #bdc3c7 !important; box-shadow: none !important; }
        
        .status-badge { padding: 4px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 700; }
        .status-optimal { background: #e8f5e9; color: #27ae60; }
        .status-warning { background: #fff3cd; color: #856404; }
      "))
    ),
    
    div(class = "page-wrapper",
      div(class = "page-header", div(class = "container", h2("AI Schedule Optimizer"), back_button())),
      
      div(class = "page-content",
        
        # 1. Top Section: Schedule Board
        div(class = "schedule-board",
          div(class = "board-header",
            div(class = "board-title", icon("calendar-alt"), " Upcoming Scheduled Departures"),
            div(class = "board-status", uiOutput("schedule_last_updated"))
          ),
          DT::dataTableOutput("schedule_table")
        ),
        
        # 2. Bottom Section: Command Center
        div(class = "command-center",
          
          # Column 1: AI Insight
          div(class = "command-col",
            div(class = "col-header", icon("robot"), "AI Analysis Log"),
            div(class = "ai-insight-box", uiOutput("scheduler_ai_message")),
            div(style = "margin-top: 15px;",
              actionButton("scan_system_btn", "Scan System for Issues", icon = icon("search"), class = "btn-scan")
            )
          ),
          
          # Column 2: Proposal
          div(class = "command-col",
            div(class = "col-header", icon("clipboard-list"), "Optimization Proposal"),
            uiOutput("scheduler_proposal_card")
          ),
          
          # Column 3: Action
          div(class = "command-col",
            div(class = "col-header", icon("check-circle"), "Authorization"),
            div(class = "action-area",
              uiOutput("scheduler_confirm_btn_ui"),
              actionButton("dismiss_proposal_btn", "Dismiss Suggestion", icon = icon("times"), class = "btn-dismiss")
            )
          )
        )
      )
    )
  )
}