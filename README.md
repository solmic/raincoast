# Fraser River Estuary Fish Explorer

An interactive Shiny dashboard presenting long-term fish monitoring data collected for Raincoast's Wild Salmon Program in the Fraser River Estuary (2016–2025).

## About

Surveys are conducted annually across multiple habitat types including eelgrass beds, sandflats, and marsh environments. The dataset includes catch records, species identifications, and biological measurements.

## Dashboard Tabs

-   **Welcome** — Dataset overview, summary statistics, and monitoring site map
-   **General Fish Population** — Trends across all fish functional groups and families
-   **Salmon** — Detailed analysis of salmon species including size and seasonal patterns
-   **Forage Fish** — Forage fish trends and co-occurrence analysis with salmon
-   **Water Properties** — Water chemistry trends across sites and years

## Data

| File | Description |
|-----------------------|-------------------------------------------------|
| `data/FraserEstuaryFishData_2016_2025.xlsx` | Raw fish catch and water chemistry data |
| `data/raincoast_sites.xlsx` | Monitoring site coordinates and metadata |

## Project Structure

Raincoast_shinyapp/ 
├── data/ \# Raw data files 

├── R/ 

│ ├── utils.R \# Shared helper functions 

│ ├── mod_intro.R \# Welcome tab module 

│ ├── mod_general_population.R \# General population tab module 

│ ├── mod_salmon.R \# Salmon tab module 

│ ├── mod_forage_fish.R \# Forage fish tab module 

│ └── mod_water_properties.R \# Water properties tab module 

├── global.R \# Packages, data loading, and cleaning 

├── server.R \# Shiny server 

└── ui.R \# Shiny UI

## Running the App

Open `Raincoast_shinyapp.Rproj` in RStudio and run:

``` r
shiny::runApp()
```

## Requirements

Install required packages with:

``` r
install.packages(c(
  "shiny", "bslib", "fontawesome", "tidyverse", "lubridate",
  "leaflet", "Polychrome", "htmlwidgets", "here", "scales",
  "janitor", "readxl", "DT", "RColorBrewer", "colorspace"
))
```
