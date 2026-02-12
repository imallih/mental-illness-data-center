# Mental Illness Data Dashboard

**Shiny app analyzing mental health data across US states.**

## Overview
This interactive dashboard allows users to explore mental health trends across different US states using survey and state-level data. Users can filter by year and state, and view interactive maps and plots.

## Data
- `MH_clean.parquet.zip` – cleaned survey data  
- `MH_states.parquet` – state-level aggregated data  
- `states_simplified.rds` – shapefile for mapping  

## Skills Used
- R, Shiny  
- dplyr, ggplot2, leaflet  
- Data cleaning, visualization, dashboard design  

## How to Run
1. Install R and RStudio  
2. Install required packages:  
```R
install.packages(c("shiny","dplyr","ggplot2","leaflet","readr"))
