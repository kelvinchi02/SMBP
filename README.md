# Smart Bus Management Platform

![Status](https://img.shields.io/badge/Status-Live-success) ![R](https://img.shields.io/badge/Language-R-blue) ![Shiny](https://img.shields.io/badge/Framework-Shiny-blueviolet)


![SmartTransit Poster](poster.png)


## Overview
The Smart Bus Management Platform is an enterprise-grade analytics dashboard designed for transit operators. It unifies real-time fleet telemetry, weather data, and passenger demand metrics into a single decision-support system.

Built with **R Shiny**, the platform leverages **Supabase** for real-time data persistence and **Groq (Llama 3.1)** for generative AI insights, enabling operators to predict delays, visualize overcrowding, and optimize schedules dynamically.

## 🚀 Live Deployment
The application is currently deployed and accessible on Posit Cloud:
**[Launch Smart Bus Platform](https://kelvinchi02-sbmp.share.connect.posit.cloud)**

### 🔐 Demo Credentials
To access the full dashboard functionalities, use the following administrative credentials:
* **Username:** `admin1`
* **Password:** `admin123`

---

## Key Features

### 1. 🌍 Live Network Map
* **Real-Time Tracking:** Visualizes vehicle locations using `leaflet` with auto-refresh capabilities.
* **Route Filtering:** Dynamic route selection to isolate specific transit lines.
* **Layer Control:** Adjustable stop visibility and transparency for clearer map overlay.

### 2. 🤖 AI-Driven Scheduling
* **Autonomous Monitoring:** The `scheduler.R` module runs a background "Auto-Scan" every 60 seconds to detect bottlenecks.
* **Smart Proposals:** If occupancy exceeds 85% or delays spike, the AI suggests specific schedule injections (e.g., "Add Trip to Route A at 08:15").
* **Decision Board:** Interactive "Next 10 Departures" board powered by `DT` tables.

### 3. ☁️ Weather Impact Analysis
* **Correlated Metrics:** Analyzes how rain, snow, or temperature shifts affect delay minutes.
* **Visualizations:** Features `ggridges` (ridge plots) for delay density and `highcharter` polar charts for commute time deviations.
* **Forecast Integration:** Displays current conditions and hourly forecasts to help dispatchers prepare for weather events.

### 4. 👥 Ridership & Crowding
* **Demand Trends:** Tracks daily passenger counts and peak usage hours using `plotly` and `ggplot2`.
* **Heatmaps:** Geospatial scatter plots identifying "Risk Zones" where overcrowding frequently occurs.
* **Capacity Alerts:** Real-time KPI cards for "Normal", "Busy", and "Critical" load states.

---

## 🔌 API & Data Architecture

### Supabase Integration (PostgreSQL)
The application uses the `httr2` library to interface with a Supabase backend, ensuring lightweight and secure data retrieval.
* **Connection Logic:** Managed in `database_connection.R` and `pre.R`.
* **Resiliency:** Implements a "Safe Mode" that automatically falls back to local CSV files (`SmartTransit_Integrated.csv`) if the API connection times out or fails.
* **Spatial Data:** The `pre.R` pipeline converts raw coordinate data into Simple Features (`sf`) objects for mapping.

### Groq AI Integration
Generative AI features are powered by the Groq API, utilizing the `llama-3.1-8b-instant` model for sub-second inference speeds.
* **Chat Assistant:** The `chat.R` module creates a conversational interface ("Bus System Assistant") that allows operators to ask natural language questions about system status.
* **Logic Engine:** The AI doesn't just chat; it evaluates boolean logic in `scheduler.R` (e.g., `Occupancy > 85% AND Delay > 10m`) to generate operational proposals.

---

## Technical Architecture

The project is modularized for maintainability:

| Module | Description | Key Libraries |
| :--- | :--- | :--- |
| **`app.R`** | Main entry point; handles auth and navigation. | `shiny`, `bslib` |
| **`pre.R`** | Data preprocessing pipeline and SF conversion. | `data.table`, `sf`, `lubridate` |
| **`map.R`** | Interactive map visualization. | `leaflet`, `shinyWidgets` |
| **`weather.R`** | Weather metrics and ridge plots. | `ggridges`, `highcharter` |
| **`scheduler.R`** | AI proposal system and schedule board. | `DT`, `bsicons` |
| **`ridership.R`** | Passenger analytics and trends. | `ggplot2`, `plotly` |
| **`styles.R`** | Centralized CSS styling. | `htmltools` |

---

## Installation & Local Setup

1.  **Clone the Repository**
    ```bash
    git clone [https://github.com/your-username/smart-bus-platform.git](https://github.com/your-username/smart-bus-platform.git)
    ```

2.  **Install R Dependencies**
    Run the following in your R console:
    ```r
    install.packages(c(
      "shiny", "bslib", "bsicons", "fontawesome", "htmltools", 
      "shinyWidgets", "leaflet", "sf", "dplyr", "dotenv", 
      "data.table", "plotly", "highcharter", "ggridges", 
      "DT", "httr2", "jsonlite", "lubridate", "scales"
    ))
    ```

3.  **Environment Configuration**
    Create a `.env` file in the root directory:
    ```env
    # App Auth
    LOGIN_USER=admin1
    LOGIN_PASS=admin123

    # Supabase (Database)
    SUPABASE_URL=your_supabase_url
    SUPABASE_KEY=your_supabase_anon_key

    # Groq (AI)
    GROQ_API_KEY=your_groq_api_key
    ```

4.  **Run the App**
    ```r
    shiny::runApp("app.R")
    ```

## License
This project is proprietary software designed for transit management analysis.