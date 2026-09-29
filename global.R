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

raw_sites   <- read_excel(here("data", "raincoast_sites.xlsx"), sheet = "raincoast_sites")
raw_renames <- read_excel(here("data", "raincoast_sites.xlsx"), sheet = "renames")

# Sites reference table (used by all maps)
sites_clean <- raw_sites %>%
  clean_names() %>%
  select(site_id, site_location, site_info, type, lat, lon) %>%
  filter(!is.na(site_id), site_id != "") %>%
  group_by(site_id) %>%
  slice(1) %>%
  ungroup()

# Old site names -> current SiteID (from the "renames" sheet)
# A code that is already a current SiteID is left as it is.
site_renames <- raw_renames %>%
  clean_names() %>%
  pivot_longer(-site_id, values_to = "old_name", values_drop_na = TRUE) %>%
  select(old_name, site_id) %>%
  filter(!old_name %in% sites_clean$site_id) %>%
  distinct(old_name, .keep_all = TRUE)

update_site_id <- function(x) {
  coalesce(site_renames$site_id[match(x, site_renames$old_name)], x)
}

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
  # Convert old site names to current SiteIDs
  mutate(site_id = update_site_id(site_id)) %>%
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
  # Fix data-entry errors in species names
  mutate(
    species  = case_when(
      species == "Sandab"      ~ "Sanddab",
      species == "Pike minnow" ~ "Northern pikeminnow",
      TRUE                     ~ species
    ),
    family   = if_else(species == "Sanddab", "Bothidae", family, missing = family),
    sci_name = if_else(species == "Prickly sculpin", "Cottus asper",
                       str_replace(sci_name, "Oligocuttus", "Oligocottus"),
                       missing = sci_name)
  ) %>%
  # Fill missing family names (before functional groups, so they get grouped)
  mutate(
    family = case_when(
      is.na(family) & species == "Forage fish"                            ~ "Unknown forage fish",
      is.na(family) & species %in% c("Flounder", "Flatfish")             ~ "Pleuronectidae",
      is.na(family) & species %in% c("Unidentified fish", "Coarse fish") ~ "Unidentified",
      is.na(family) & is.na(species)                                      ~ "Unidentified",
      TRUE                                                                 ~ family
    )
  ) %>%
  # Functional groups
  mutate(
    functional_group = case_when(
      family %in% c("Ammodytidae", "Clupeidae",
                    "Engraulidae", "Osmeridae",
                    "Embiotocidae", "Gasterosteidae",
                    "Aulorhynchidae")                        ~ "Forage fish",
      family %in% c("Bothidae", "Pleuronectidae")            ~ "Benthic flatfishes",
      family %in% c("Cottidae", "Agonidae",
                    "Pholidae", "Stichaeidae")               ~ "Benthic sculpins & gunnels",
      family %in% c("Gadidae", "Gobiidae", "Syngnathidae")   ~ "Pelagic / semi-pelagic non-forage fishes",
      family %in% c("Catostomidae", "Centrarchidae",
                    "Cobitidae", "Cyprinidae",
                    "Leuciscidae")                           ~ "Freshwater fishes",
      family == "Rajidae"                                    ~ "Elasmobranchs",
      family == "Petromyzontidae"                            ~ "Lampreys",
      family == "Acipenseridae"                              ~ "Sturgeon",
      family %in% c("Scorpaenidae", "Hexagrammidae")         ~ "Rockfishes & greenlings",
      species == "Forage fish"                               ~ "Forage fish",
      species %in% c("Chinook", "Chum", "Coho",
                     "Pink", "Sockeye", "Salmon")            ~ "Salmon",
      species %in% c("Bull trout", "Dolly varden",
                     "Cutthroat trout", "Rainbow trout",
                     "Steelhead trout", "Mountain whitefish") ~ "Other Salmonoids",
      TRUE                                                   ~ "Unclassified / coarse ID"
    )
  )

# Site checks (printed to the R console at startup)
# Re-run the app after editing raincoast_sites.xlsx to re-check.
site_check <- function(title, x) {
  if (length(x) > 0) {
    message("⚠ ", title, " (", length(x), "): ", paste(x, collapse = ", "))
  }
  length(x)
}

sites_all <- raw_sites %>% clean_names() %>% filter(!is.na(site_id))
old_names <- raw_renames %>%
  clean_names() %>%
  pivot_longer(-site_id, values_to = "old_name", values_drop_na = TRUE)

n_problems <- sum(
  site_check(
    "Site codes in the fish data not found in raincoast_sites (current or old names)",
    fraser_clean %>%
      filter(!is.na(site_id), !site_id %in% sites_clean$site_id) %>%
      count(site_id) %>%
      mutate(label = paste0(site_id, " [", n, " records]")) %>%
      pull(label)
  ),
  site_check(
    "Old names that are also a current SiteID (kept as the current site)",
    old_names %>%
      filter(old_name %in% sites_clean$site_id) %>%
      mutate(label = paste0(old_name, " (old name for ", site_id, ")")) %>%
      pull(label)
  ),
  site_check(
    "Old names listed under more than one site",
    old_names %>% count(old_name) %>% filter(n > 1) %>% pull(old_name)
  ),
  site_check(
    "SiteIDs in the renames sheet missing from the raincoast_sites sheet",
    setdiff(na.omit(raw_renames$SiteID), sites_clean$site_id)
  ),
  site_check(
    "SiteIDs listed more than once with different type or coordinates (first row used)",
    sites_all %>%
      distinct(site_id, type, lat, lon) %>%
      count(site_id) %>%
      filter(n > 1) %>%
      pull(site_id)
  ),
  site_check(
    "Sites with no coordinates (not shown on maps)",
    sites_clean %>% filter(is.na(lat) | is.na(lon)) %>% pull(site_id)
  ),
  site_check(
    "Sites with no type",
    sites_clean %>% filter(is.na(type)) %>% pull(site_id)
  )
)

if (n_problems == 0) message("✓ Site checks: no problems found")

rm(sites_all, old_names, n_problems)

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
  filter(species %in% salmon_species)

forage_clean <- fraser_clean %>%
  filter(functional_group == "Forage fish")

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