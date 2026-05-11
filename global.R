# global.R
# Runs once at app startup — packages, data loading, and cleaning

# Packages 
library(shiny)
library(bslib)
library(fontawesome)
library(tidyverse)
library(lubridate)
library(leaflet)
library(Polychrome)
library(htmlwidgets)
library(here)
library(scales)
library(janitor)
library(readxl)
library(DT)

# Source modules 
source(here("R", "utils.R"))                        
source(here("R", "mod_intro.R"))
source(here("R", "mod_general_population.R"))
source(here("R", "mod_salmon.R"))
source(here("R", "mod_forage_fish.R"))
source(here("R", "mod_water_properties.R"))


# Load raw data
raw_fish  <- read_excel(here("data", "FraserEstuaryFishData_2016_2025.xlsx"),
                        sheet = "FraserEstuaryFishData_2016_2025")

raw_water <- read_excel(here("data", "FraserEstuaryFishData_2016_2025.xlsx"),
                        sheet = "Water Chemistry")

raw_sites <- read_excel(here("data", "raincoast_sites.xlsx"))

# Sites reference table
sites_clean <- raw_sites %>%
  clean_names() %>%                    
  select(site_id, site, lat, lon) %>%
  filter(!is.na(site_id), site_id != "") %>%
  group_by(site_id) %>%
  slice(1) %>%
  ungroup()

# Helper: standardize method names 
clean_method <- function(x) {
  case_when(
    tolower(x) == "beach seine"       ~ "Beach seine",
    tolower(x) == "fixed beach seine" ~ "Fixed beach seine",
    tolower(x) == "fyke net"          ~ "Fyke net",
    tolower(x) == "fixed fyke net"    ~ "Fixed fyke net",
    tolower(x) == "high fyke"         ~ "High fyke",
    tolower(x) == "purse seine"       ~ "Purse seine",
    tolower(x) == "frytrap"           ~ "Frytrap",
    tolower(x) == "dip net"           ~ "Dip net",
    tolower(x) == "n/a"               ~ NA_character_,
    TRUE                              ~ x
  )
}

# Clean fish dataset
fraser_clean <- raw_fish %>%
  clean_names() %>%
  # Replace "N/A" strings with proper NA
  mutate(across(where(is.character), ~ na_if(trimws(.), "N/A"))) %>%
  # Dates
  mutate(
    date  = as.Date(date),
    year  = year(date),
    month = month(date, label = TRUE, abbr = FALSE)
  ) %>%
  # Numeric conversions
  mutate(
    fork   = suppressWarnings(as.numeric(fork)),
    mass_g = suppressWarnings(as.numeric(mass_g))
  ) %>%
  # Standardize methods
  mutate(method = clean_method(method)) %>%
  # Functional groups
  mutate(
    functional_group = case_when(
      family %in% c("Ammodytidae", "Clupeidae",
                    "Engraulidae", "Osmeridae")              ~ "Forage fish",
      family %in% c("Bothidae", "Pleuronectidae")            ~ "Benthic flatfishes",
      family %in% c("Cottidae", "Agonidae",
                    "Pholidae", "Stichaeidae")               ~ "Benthic sculpins & gunnels",
      family %in% c("Gadidae", "Gasterosteidae", "Gobiidae") ~ "Pelagic / semi-pelagic non-forage fishes",
      family %in% c("Catostomidae", "Centrarchidae",
                    "Cobitidae", "Cyprinidae",
                    "Leuciscidae")                           ~ "Freshwater fishes",
      family == "Rajidae"                                    ~ "Elasmobranchs",
      family == "Petromyzontidae"                            ~ "Lampreys",
      family == "Acipenseridae"                              ~ "Sturgeon",
      family %in% c("Scorpaenidae", "Hexagrammidae")         ~ "Rockfishes & greenlings",
      species == "Forage fish"                               ~ "Forage fish",
      species %in% c("Chinook", "Chum", "Coho",
                     "Pink", "Sockeye")                      ~ "Salmon",
      species %in% c("Bull trout", "Dolly varden",
                     "Cutthroat trout", "Rainbow trout",
                     "Steelhead trout")                      ~ "Other Salmonoids",
      TRUE                                                   ~ "Unclassified / coarse ID"
    )
  ) %>%
  # Fill missing family names
  mutate(
    family = case_when(
      is.na(family) & species == "Forage fish"                            ~ "Unknown forage fish",
      is.na(family) & species %in% c("Flounder", "Flatfish")             ~ "Pleuronectidae",
      is.na(family) & species %in% c("Unidentified fish", "Coarse fish") ~ "Unidentified",
      is.na(family) & is.na(species)                                      ~ "Unidentified",
      TRUE                                                                 ~ family
    )
  )

# Clean water dataset 
water_clean <- raw_water %>%
  clean_names() %>%
  rename(pH = p_h) %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
  mutate(
    date         = as.Date(date),
    year         = year(date),
    month        = month(date, label = TRUE, abbr = FALSE),
    temp         = suppressWarnings(as.numeric(temp)),
    do_percent   = suppressWarnings(as.numeric(do_percent)),
    do_mg_l      = suppressWarnings(as.numeric(do_mg_l)),
    pH           = suppressWarnings(as.numeric(pH)),
    salinity_ppt = suppressWarnings(as.numeric(salinity_ppt)),
    secchi_m     = suppressWarnings(as.numeric(secchi_m)),
    turbid = case_when(
      tolower(as.character(turbid)) %in% c("y", "yes", "true",  "1") ~ TRUE,
      tolower(as.character(turbid)) %in% c("n", "no",  "false", "0") ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  select(date, year, month, site, site_id, type,
         temp, do_percent, do_mg_l, pH, salinity_ppt, secchi_m, turbid)

# Subsets
salmon_species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye")

salmon_clean <- fraser_clean %>%
  filter(species %in% salmon_species) %>%
  left_join(sites_clean %>% select(site_id, lat, lon), by = "site_id")

forage_clean <- fraser_clean %>%
  filter(functional_group == "Forage fish") %>%
  left_join(sites_clean %>% select(site_id, lat, lon), by = "site_id")

# Column name reference
# clean_names() converts all column names to snake_case on load.
# Key changes from the raw Excel file:
#
#   Raw name          → New name
#   ─────────────────────────────
#   SiteID            → site_id
#   Site              → site
#   LifeStage         → life_stage
#   Fork              → fork
#   Depth             → depth
#   Hatch?            → hatch
#   Tanya             → tanya
#   Mass(g)           → mass_g
#   Time              → time
#   Set_Depth_Start   → set_depth_start
#   Set_Depth_End     → set_depth_end
#   Notes             → notes
#   Method            → method
#   Date              → date