# Smart Bus Platform - Technical Manual

## 1. System Architecture

### 1.1 High-Level Design
The application follows a modular Monolith architecture using R Shiny. It is designed around a reactive core that polls for data updates and propagates changes to independent UI modules.

* **Frontend**: `bslib` and `shiny` provide a responsive, dashboard-style interface.
* **Backend Logic**: R functions serve as the controller layer, managing state via `reactiveVal` and `reactivePoll`.
* **Data Persistence**: A hybrid model using remote Supabase (PostgreSQL) for live operations and local CSVs for failover/simulation.
* **Intelligence Layer**: External API calls to Groq (Llama 3.1) provide natural language processing and decision support.

### 1.2 Module Map
The codebase is split into functional domains to ensure maintainability. The file structure is organized by responsibility:

#### Core Infrastructure
| File | Role | Key Dependencies |
| :--- | :--- | :--- |
| **`app.R`** | **Entry Point:** Orchestrates module loading, global server logic, and auto-refresh timers. | `shiny`, `bslib` |
| **`pre.R`** | **Preprocessing:** Handles data ingestion, type conversion, and `sf` geospatial object creation. | `data.table`, `sf`, `lubridate` |
| **`database_connection.R`** | **DAL:** Manages secure connections to Supabase and fallback logic. | `httr2`, `dotenv` |
| **`api_utils.R`** | **Logic Engine:** Contains all KPI calculations (delays, crowding) and simulation algorithms. | `data.table`, `scales` |
| **`styles.R`** | **Styling:** Centralized CSS definitions for the application's look and feel. | `htmltools` |
| **`create_manifest.R`** | **Deployment:** Generates `manifest.json` for publishing to Posit Connect. | `rsconnect` |

#### UI Modules & Features
| File | Role | Key Dependencies |
| :--- | :--- | :--- |
| **`dashboard.R`** | **Shell:** Defines the main sidebar, top navigation bar, and landing page carousel. | `bsicons`, `htmltools` |
| **`login.R`** | **Auth:** Handles the login screen UI and credential validation logic. | `shiny` |
| **`overview.R`** | **Summary View:** High-level metrics and system-wide KPI evaluation. | `plotly` |
| **`scheduler.R`** | **AI Agent:** The autonomous scheduling interface and decision board. | `DT` (DataTables) |
| **`chat.R`** | **Assistant:** The LLM interface for the "Bus System Assistant" chat panel. | `httr2` (Groq API) |
| **`map.R`** | **Live Map:** Leaflet visualization for vehicle tracking. | `leaflet`, `shinyWidgets` |
| **`hour.R`** | **Delay Analysis:** Visualizes hourly schedule adherence and delay distributions. | `ggplot2` |
| **`crowd.R`** | **Crowding:** Visualizes occupancy rates, risk zones, and scatter maps. | `plotly` |
| **`ridership.R`** | **Trends:** visualizes passenger counts and daily demand curves. | `ggplot2`, `plotly` |
| **`weather.R`** | **Impact:** Visualizes weather correlation with delays (Polar/Ridge plots). | `highcharter`, `ggridges` |

---

## 2. Data Flow & Reactivity

### 2.1 The Auto-Refresh Cycle
The application does not rely on manual refreshes. It utilizes `reactivePoll` in `app.R` to check the database timestamp every 15 seconds.

1.  **Check Function**: Queries the latest timestamp from the `SmartTransit_Integrated` table.
2.  **Value Function**: If the timestamp has changed, the full dataset is downloaded, processed (type conversion in `pre.R`), and stored in a reactive object `live_info()`.
3.  **Propagation**: Any UI element dependent on `live_info()` (e.g., the Map, KPI cards) automatically invalidates and redraws.

### 2.2 Offline/Failover Mode
To ensure system stability during network outages or API rate limiting, `database_connection.R` implements a `tryCatch` block.
* **Primary Path**: Attempt HTTP GET request to Supabase REST API.
* **Fallback Path**: If HTTP 500/404/Timeout occurs, the system reads `SmartTransit_Integrated.csv` from the local directory.
* **User Notification**: The system logs a `[SYSTEM] Switching to Offline Mode` message to the R console.

---

## 3. AI & Logic Integration

### 3.1 The Scheduler Logic (`scheduler.R`)
The AI Scheduler is not a "black box." It uses a hybrid approach combining deterministic rules with probabilistic LLM generation.

* **Trigger**: A `reactiveTimer` fires every 60 seconds.
* **Deterministic Filter**: The system calculates metrics for routes A, B, and C. It looks for:
    * Occupancy > 85%
    * Average Delay > 10 minutes
    * Passengers Waiting > 5
* **LLM Prompting**: If a threshold is breached, a structured prompt is sent to Groq. The prompt explicitly restricts the AI to return a specific string format (`DECISION: YES | REASON: ...`).
* **Action**: If the AI returns "YES," the UI updates to show a "Proposed Action" card.

### 3.2 Chat Assistant (`chat.R`)
The chat module uses a stateless request model.
* **Context Injection**: Every user message is wrapped with a "System Persona" and a summary of the current system KPIs (`get_live_kpi_summary` from `api_utils.R`).
* **Privacy**: No user-identifiable data is sent to the LLM, only aggregated operational metrics.

---

## 4. Visualization & Geocomputation

### 4.1 Geospatial Processing (`pre.R` & `map.R`)
Map data is handled using the `sf` (Simple Features) library.
* **Ingestion**: Raw Lat/Lon columns are converted to `sf` POINT geometries in `pre.R` immediately upon data load.
* **Rendering**: `leaflet` renders these points. To optimize performance, the map uses `leafletProxy` to update only the markers (bus locations) rather than redrawing the entire base map tiles.

### 4.2 Charting Libraries
* **`highcharter`**: Used in `weather.R` for the Weather Polar Chart due to its superior handling of radial data.
* **`ggplot2` / `ggridges`**: Used in `weather.R` and `hour.R` for statistical distributions (delay density) where static analysis is preferred.
* **`plotly`**: Used in `overview.R`, `crowd.R`, and `ridership.R` for interactive pie charts and trend lines to allow user zooming.

---

## 5. Deployment & Security

### 5.1 Environment Variables
Security is managed via `.env` files (locally) and Environment Variables (on Posit Cloud). The following keys are required:

```bash
SUPABASE_URL=...      # Database Endpoint
SUPABASE_KEY=...      # Anon Key for Read Access
GROQ_API_KEY=...      # LLM Inference Key
LOGIN_USER=...        # Admin Username
LOGIN_PASS=...        # Admin Password