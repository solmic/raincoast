# R/mod_intro.R

mod_intro_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 10,
        offset = 1,
        
        # Header
        div(
          style = "text-align: center; padding: 40px 0 20px 0;",
          h1("Fraser River Fish Monitoring Dataset",
             style = "font-size: 2.5em; font-weight: 700; color: #1e3a5f;"),
          p("A long-term monitoring dataset of fish populations in the Fraser River estuary",
            style = "font-size: 1.2em; color: #6b7280;")
        ),
        
        hr(),
        
        # Overview cards
        bslib::layout_columns(
          bslib::value_box(
            title = "Years of monitoring",
            value = textOutput(ns("intro_n_years")),
            theme = bslib::value_box_theme(bg = "#1e3a5f", fg = "white")
          ),
          bslib::value_box(
            title = "Total records",
            value = textOutput(ns("intro_total_records")),
            theme = bslib::value_box_theme(bg = "#3b82f6", fg = "white")
          ),
          bslib::value_box(
            title = "Species recorded",
            value = textOutput(ns("intro_n_species")),
            theme = bslib::value_box_theme(bg = "#10b981", fg = "white")
          ),
          bslib::value_box(
            title = "Monitoring sites",
            value = textOutput(ns("intro_n_sites")),
            theme = bslib::value_box_theme(bg = "#f59e0b", fg = "white")
          ),
          col_widths = c(3, 3, 3, 3)
        ),
        
        br(),
        
        # About section
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h3("About this dataset", style = "color: #1e3a5f;"),
              p("This dashboard presents long-term fish monitoring data collected for Raincoast's Wild
                Salmon Program in the Fraser River Estuary. Surveys are conducted annually across multiple
                habitat types including eelgrass beds, sandflats, and marsh environments. The dataset
                includes catch records, species identifications, and biological measurements."),
              p("Use the tabs above to explore:"),
              tags$ul(
                tags$li(tags$b("General Population:"), " Trends across all fish functional groups and families."),
                tags$li(tags$b("Salmon:"), " Detailed analysis of salmon species including size, condition, and yearly patterns."),
                tags$li(tags$b("Forage Fish:"), " Detailed analysis of forage fish species including yearly patterns and co-occurrence data with salmon."),
                tags$li(tags$b("Water Properties:"), " Detailed analysis of water properties through yearly patterns."),
                tags$li(tags$b("Site Type:"), " Detailed analysis of sampling sites and their related gear types.")
              )
            )
          )
        ),
        
        # Charts row
        fluidRow(
          column(
            width = 6,
            wellPanel(
              h4("Records by year", style = "color: #1e3a5f;"),
              plotOutput(ns("intro_year_bar"), height = "280px")
            )
          ),
          column(
            width = 6,
            wellPanel(
              h4("Records by habitat type", style = "color: #1e3a5f;"),
              plotOutput(ns("intro_type_bar"), height = "280px")
            )
          )
        ),
        
        br(),
        
        # Map
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Monitoring sites", style = "color: #1e3a5f;"),
              leaflet::leafletOutput(ns("intro_sites_map"), height = "400px")
            )
          )
        ),
        
        br(),
        
        # Data dictionary
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h3("Data dictionary", style = "color: #1e3a5f;"),
              tableOutput(ns("data_dict_tbl"))
            )
          )
        )
      )
    )
  )
}

mod_intro_server <- function(id, fraser_data, sites_data) {
  moduleServer(id, function(input, output, session) {
    
    output$intro_n_years <- renderText({
      dat <- fraser_data()
      req(dat)
      n_distinct(dat$year, na.rm = TRUE)
    })
    
    output$intro_total_records <- renderText({
      dat <- fraser_data()
      req(dat)
      format(nrow(dat), big.mark = ",")
    })
    
    output$intro_n_species <- renderText({
      dat <- fraser_data()
      req(dat)
      n_distinct(dat$species, na.rm = TRUE)
    })
    
    output$intro_n_sites <- renderText({
      dat <- fraser_data()
      req(dat)
      n_distinct(dat$site_id, na.rm = TRUE)
    })
    
    # Records by year
    output$intro_year_bar <- renderPlot({
      dat <- fraser_data()
      req(dat)
      dat %>%
        filter(!is.na(year)) %>%
        count(year, name = "n_records") %>%
        mutate(prop = n_records / sum(n_records)) %>%
        ggplot(aes(x = factor(year), y = prop)) +
        geom_col(fill = "#3b82f6", alpha = 0.85) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
        labs(x = "Year", y = "Proportion of total records") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Records by habitat type
    output$intro_type_bar <- renderPlot({
      dat <- fraser_data()
      req(dat)
      dat %>%
        filter(!is.na(type)) %>%
        count(type, name = "n_records") %>%
        arrange(desc(n_records)) %>%
        ggplot(aes(x = reorder(type, n_records), y = n_records)) +
        geom_col(fill = "#10b981", alpha = 0.85) +
        coord_flip() +
        labs(x = NULL, y = "Number of records") +
        theme_minimal()
    })
    
    # Monitoring sites map
    output$intro_sites_map <- leaflet::renderLeaflet({
      req(!is.null(sites_data))
      
      map_sites <- sites_data %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        mutate(label = if_else(is.na(site), site_id, site))
      
      sites_unique <- unique(map_sites$label)
      clrs <- Polychrome::createPalette(
        max(length(sites_unique), 3),
        seedcolors = c("#A9A9A9", "#FF0000", "#0000FF")
      )
      names(clrs) <- NULL
      site_pal <- leaflet::colorFactor(palette = clrs, domain = sites_unique)
      
      leaflet::leaflet(map_sites) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addCircleMarkers(
          lng         = ~lon,
          lat         = ~lat,
          color       = ~site_pal(label),
          fillColor   = ~site_pal(label),
          fillOpacity = 0.9,
          radius      = 5,
          weight      = 2,
          opacity     = 1,
          label       = ~paste0(site_id, " \u2014 ", label),
          popup       = ~paste0(
            "<b>Site ID: </b>", site_id, "<br>",
            "<b>Site: </b>", label
          )
        ) %>%
        leaflet::fitBounds(
          lng1 = min(map_sites$lon) - 0.05,
          lat1 = min(map_sites$lat) - 0.05,
          lng2 = max(map_sites$lon) + 0.05,
          lat2 = max(map_sites$lat) + 0.05
        )
    })
    
    # Data dictionary
    output$data_dict_tbl <- renderTable({
      tibble::tibble(
        Column      = c("year", "month", "site_id", "type",
                        "species", "sci_name", "functional_group", "family",
                        "fork", "mass_g", "lat", "lon"),
        Description = c(
          "Year of survey",
          "Month of survey",
          "Unique site identifier",
          "Site type classification (e.g. Eelgrass, Sandflat, Marsh)",
          "Common name of the species",
          "Scientific name of the species",
          "Functional group classification",
          "Taxonomic family",
          "Fork length (mm)",
          "Body mass (g)",
          "Latitude of site",
          "Longitude of site"
        )
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
  })
}