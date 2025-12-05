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
        
        /* New Decision Table Layout */
        .decision-table-container { background: white; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.08); overflow: hidden; border: 1px solid #e9ecef; }
        
        /* Table Header */
        .decision-header { display: grid; grid-template-columns: 2fr 1fr 1fr; background-color: #f8f9fa; border-bottom: 2px solid #e9ecef; padding: 15px 25px; }
        .d-head-item { font-size: 0.85rem; font-weight: 700; color: #495057; text-transform: uppercase; letter-spacing: 0.05em; display: flex; align-items: center; gap: 8px; }
        
        /* Table Body */
        .decision-body { display: grid; grid-template-columns: 2fr 1fr 1fr; padding: 0; min-height: 180px; }
        
        /* Column 1: AI Insight */
        .ai-insight-panel { padding: 25px; border-right: 1px solid #eee; display: flex; flex-direction: column; justify-content: space-between; }
        .ai-text-box { background: #f1f8ff; border-left: 4px solid #007bff; padding: 15px; border-radius: 6px; color: #2c3e50; font-size: 0.95rem; line-height: 1.5; margin-bottom: 15px; flex-grow: 1; }
        .scan-controls { margin-top: 10px; }
        
        /* Column 2: Proposal Details */
        .proposal-panel { padding: 25px; border-right: 1px solid #eee; text-align: center; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #fff; }
        .prop-time-large { font-size: 2rem; font-weight: 800; color: #2c3e50; margin: 5px 0; letter-spacing: -0.5px; }
        .prop-route-badge { display: inline-block; background: #2c3e50; color: white; padding: 4px 12px; border-radius: 20px; font-weight: 600; font-size: 0.85rem; margin-bottom: 8px; }
        .prop-impact-text { font-size: 0.8rem; color: #6c757d; font-style: italic; }
        
        /* Column 3: Actions */
        .action-panel { padding: 25px; display: flex; flex-direction: column; gap: 12px; justify-content: center; align-items: center; background-color: #fff; }
        .btn-confirm-lg { width: 100%; background-color: #28a745; color: white; font-weight: 700; padding: 10px; border: none; border-radius: 6px; transition: all 0.2s; display: flex; align-items: center; justify-content: center; gap: 8px; font-size: 0.9rem; }
        .btn-confirm-lg:hover { background-color: #218838; transform: translateY(-1px); box-shadow: 0 4px 8px rgba(40, 167, 69, 0.2); }
        .btn-dismiss-lg { width: 100%; background-color: transparent; color: #dc3545; border: 1px solid #dc3545; font-weight: 600; padding: 8px; border-radius: 6px; transition: all 0.2s; font-size: 0.9rem; }
        .btn-dismiss-lg:hover { background-color: #fff5f5; }
        .btn-disabled-lg { background-color: #e9ecef !important; color: #adb5bd !important; border: 1px solid #dee2e6 !important; cursor: not-allowed; box-shadow: none; }
        
        .status-optimal { color: #28a745; font-weight: 600; padding: 10px; border-radius: 6px; background: #e8f5e9; border: 1px solid #c3e6cb; display: inline-block; }

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
        
        # 2. Bottom Section: Decision Table Layout
        div(class = "decision-table-container",
            
            # Table Header
            div(class = "decision-header",
                div(class = "d-head-item", icon("robot"), " AI Strategic Insight"),
                div(class = "d-head-item", style="justify-content: center;", icon("clock"), " Proposed Action"),
                div(class = "d-head-item", style="justify-content: center;", icon("user-check"), " Authorization")
            ),
            
            # Table Body
            div(class = "decision-body",
                
                # Column 1: AI Suggestion
                div(class = "ai-insight-panel",
                    uiOutput("scheduler_ai_message"),
                    div(class = "scan-controls",
                        actionButton("scan_system_btn", "Run System Scan", icon = icon("search"), class = "btn-scan", style="width: auto; padding: 8px 20px; font-size: 0.9rem; background-color: #34495e; color: white; border:none; border-radius: 4px;")
                    )
                ),
                
                # Column 2: What/When
                div(class = "proposal-panel",
                    uiOutput("scheduler_proposal_card")
                ),
                
                # Column 3: Confirm/Dismiss
                div(class = "action-panel",
                    uiOutput("scheduler_confirm_btn_ui"),
                    actionButton("dismiss_proposal_btn", "Dismiss", class = "btn-dismiss-lg", icon = icon("times"))
                )
            )
        )
      )
    )
  )
}