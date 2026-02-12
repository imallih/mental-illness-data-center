# Mental Illness Data Dashboard

**Interactive Shiny app exploring mental health trends across US states.**

## Overview
This dashboard allows users to visualize and analyze mental health survey data and state-level indicators.  
Users can:
- Filter by state and year  
- Explore interactive maps of mental health metrics  
- Compare trends across states using charts  

## Live App
Try the app here: [Shiny App](https://allih-final-project.share.connect.posit.cloud/)

## Data
- `MH_clean.parquet.zip` – cleaned survey data  
- `MH_states.parquet` – aggregated state-level data  
- `states_simplified.rds` – shapefile for mapping  
- `cb_2016_us_state_500k` – base shapefile for US states  

## Libraries / Skills Used
- **R / Shiny** – interactive web app  
- **Data manipulation & cleaning:** dplyr, tidyr, haven, arrow  
- **Visualization:** ggplot2, leaflet, leaflet.extras, sf, raster  
- **UI / themes:** shinythemes  
- **Deployment:** rsconnect  

## How to Run Locally
1. Install R and RStudio  
2. Install required packages:
```R
install.packages(c("shiny","shinythemes","dplyr","tidyr","ggplot2","leaflet","leaflet.extras","sf","raster","haven","arrow","rsconnect"))

