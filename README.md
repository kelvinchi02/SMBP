# Smart Bus Management Platform

![Status](https://img.shields.io/badge/Status-Live-success) ![R](https://img.shields.io/badge/Language-R-blue) ![Shiny](https://img.shields.io/badge/Framework-Shiny-blueviolet)


![SmartTransit Poster](poster1.png)


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
This directory contains the core datasets used to power the Smart Bus Management Platform. The data is divided into two primary files:
1. **Operational Telemetry (`SmartTransit_Integrated.csv`)**: A comprehensive record of real-time bus operations, including location, passenger counts, delays, and environmental conditions.
2. **Static Schedule (`schedule.csv`)**: The baseline timetable used for route planning and AI-driven optimization.

---

## Datasets
**File:** `SmartTransit_Integrated.csv`

This dataset serves as the primary source for the analytics dashboard. It contains granular records of individual bus trips, capturing metrics at every stop.

### Key Column Groups

#### A. Route & Trip Identification
* **`datetime`**: Timestamp of the recorded observation.
* **`service_date`**: The operating date of service.
* **`route_id`**: Identifier for the transit line (e.g., "A", "B", "C").
* **`route_name` / `route_long_name`**: Descriptive names of the route (e.g., "Downtown Loop").
* **`trip_id`**: Unique identifier for a specific trip sequence.
* **`direction_id` / `direction_name`**: Direction of travel (0/Outbound or 1/Inbound).

#### B. Stop & Location Metrics
* **`stop_id`**: Unique code for the specific bus stop.
* **`stop_name`**: Common name of the stop.
* **`stop_sequence`**: The order of the stop within the trip.
* **`lat` / `lon`**: Geospatial coordinates (Latitude/Longitude) of the bus at the time of recording.
* **`stop_zone`**: Classification of the stop area (e.g., Urban, Suburban).

#### C. Operational Performance
* **`scheduled_arrival`**: The planned arrival time per the timetable.
* **`actual_arrival`**: The recorded arrival time.
* **`delay_min`**: The deviation from the schedule in minutes (Positive = Late, Negative = Early).
* **`headway_min`**: The time interval between the current bus and the preceding one.
* **`dwell_time_sec`**: Time spent stationary at the stop for boarding/alighting.

#### D. Passenger & Capacity Metrics
* **`passengers_boarded` / `alighted`**: Counts of passengers entering and exiting.
* **`passengers_onboard`**: Total current load on the vehicle.
* **`occupancy_rate`**: The ratio of passengers onboard to total capacity.
* **`crowding_level`**: Categorical classification of load (Low, Medium, High).
* **`capacity`**: Maximum passenger limit for the specific vehicle.

#### E. Environmental & Contextual Factors
* **`weather_conditions`**: General weather state (e.g., cloudy, rain).
* **`weather_temp_c`**: Ambient temperature in degrees Celsius.
* **`weather_precip_mm`**: Precipitation volume in millimeters.
* **`peak`**: Indicator if the trip occurs during peak rush hours.

---

**File:** `schedule.csv`

This file defines the planned service structure. It is utilized by the `scheduler.R` module to compare actual performance against targets and by the AI agent to propose schedule injections.

### Column Schema
| Column | Data Type | Description |
| :--- | :--- | :--- |
| **`route_id`** | String | The identifier of the route (Links to `SmartTransit_Integrated.csv`). |
| **`scheduled_departure`** | Time | The planned departure time from the terminal or key timing points. |
| **`headway_min`** | Integer | The planned time interval (frequency) between trips in minutes. |

---

## Usage Notes

* **Data linkage:** The `route_id` and `scheduled_departure` field acts as the primary key connecting the static schedule with the operational data.
* **Fallback Mechanism:** In the R application, `database_connection.R` is configured to load these CSV files automatically if the connection to the live Supabase database fails.
* **AI Analysis:** The generative AI module uses the `occupancy_rate` and `delay_min` fields from the integrated dataset to identify bottlenecks and references `schedule.csv` to propose valid time slots for new trips.

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