# R/mod_salmon.R

mod_salmon_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          style = "position: sticky; top: 20px;",
          radioButtons(
            ns("view_by"), "View by",
            choices  = c("Species" = "species", "Site" = "site"),
            selected = "species",
            inline   = TRUE
          ),
          h4("Filters"),
          sliderInput(
            ns("year_range"), "Year range",
            min = 2016, max = 2025,
            value = c(2016, 2025), sep = ""
          ),
          selectInput(ns("site_id"), "Site ID",
                      choices = NULL, selected = "All", multiple = TRUE),
          selectInput(ns("species"), "Species",
                      choices = NULL, selected = "All", multiple = TRUE),
          checkboxInput(
            ns("complete_years"),
            "Show all years (fill missing with 0)",
            value = TRUE
          )
        )
      ),
      
      column(
        width = 9,
        h3("Salmon"),
        uiOutput(ns("kpi_row")),
        br(),
        plotOutput(ns("salmon_time_plot"),        height = "420px"),
        hr(),
        plotOutput(ns("salmon_time_month_plot"),  height = "1000px"),
        hr(),
        plotOutput(ns("salmon_fork_year_plot"),   height = "500px"),
        hr(),
        plotOutput(ns("salmon_fork_month_plot"),  height = "500px"),
        hr(),
        h4("Site Map"),
        leafletOutput(ns("salmon_map"), height = "500px"),
        hr(),
        br(),
        h4("Species present"),
        DTOutput(ns("species_tbl"))
      )
    )
  )
}

mod_salmon_server <- function(id, salmon_data, sites_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    palettes <- reactiveValues(species = NULL, site = NULL)
    
    observe({
      dat <- salmon_data()
      req(dat)
      
      update_choices(session, "site_id", dat$site_id)
      update_choices(session, "species", dat$species)
      
      palettes$species <- paired_map(sort(unique(na.omit(dat$species))))
      palettes$site    <- paired_map(sort(unique(na.omit(dat$site))))
    })
    
    # KPI row
    output$kpi_row <- renderUI({
      bslib::layout_columns(
        bslib::value_box(
          title = "Total records",
          value = textOutput(ns("kpi_total_records")),
          theme = bslib::value_box_theme(bg = "#3b82f6", fg = "white")
        ),
        bslib::value_box(
          title = "Species present",
          value = textOutput(ns("kpi_n_species")),
          theme = bslib::value_box_theme(bg = "white", fg = "#111827")
        ),
        bslib::value_box(
          title = "Total years",
          value = textOutput(ns("kpi_n_years")),
          theme = bslib::value_box_theme(bg = "#10b981", fg = "#111827")
        ),
        if (input$view_by != "species") {
          bslib::value_box(
            title = "Sites shown",
            value = textOutput(ns("kpi_n_groups")),
            theme = bslib::value_box_theme(bg = "#f59e0b", fg = "white")
          )
        },
        col_widths = if (input$view_by != "species") c(3, 3, 3, 3) else c(4, 4, 4)
      )
    })
    
    filtered_dat <- reactive({
      dat <- salmon_data()
      req(dat)
      
      dat <- dat %>%
        filter(!is.na(year),
               year >= input$year_range[1],
               year <= input$year_range[2])
      
      site_keep <- drop_all(input$site_id)
      sp_keep   <- drop_all(input$species)
      
      if (length(site_keep) > 0) dat <- dat %>% filter(site_id %in% site_keep)
      if (length(sp_keep)   > 0) dat <- dat %>% filter(species  %in% sp_keep)
      
      dat %>%
        mutate(group = .data[[input$view_by]]) %>%
        filter(!is.na(group), group != "")
    })
    
    output$kpi_total_records <- renderText({ format(nrow(filtered_dat()), big.mark = ",") })
    output$kpi_n_species     <- renderText({ n_distinct(filtered_dat()$species, na.rm = TRUE) })
    output$kpi_n_groups      <- renderText({ n_distinct(filtered_dat()$group,   na.rm = TRUE) })
    output$kpi_n_years       <- renderText({ n_distinct(filtered_dat()$year,    na.rm = TRUE) })
    
    counts_year_group <- reactive({
      ts <- filtered_dat() %>%
        count(year, group, name = "n_records")
      
      if (isTRUE(input$complete_years)) {
        ts <- ts %>%
          complete(
            year = seq(input$year_range[1], input$year_range[2]),
            group,
            fill = list(n_records = NA)
          )
      }
      
      ts %>%
        group_by(year) %>%
        mutate(pct = n_records / sum(n_records, na.rm = TRUE)) %>%
        ungroup()
    })
    
    output$salmon_time_plot <- renderPlot({
      ts <- counts_year_group()
      req(nrow(ts) > 0, !is.null(palettes$species), !is.null(palettes$site))
      
      pal <- if (input$view_by == "species") palettes$species else palettes$site
      
      ggplot(ts, aes(year, pct, color = group, group = group)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2.5) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2])) +
        scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 1)) +
        scale_color_manual(values = pal, drop = FALSE) +
        labs(
          x     = "Year",
          y     = "Proportion of annual records (%)",
          title = "Trends through time",
          color = if (input$view_by == "species") "Species" else "Site"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$salmon_time_month_plot <- renderPlot({
      dat <- filtered_dat()
      req(nrow(dat) > 0)
      
      year_lvls <- sort(unique(na.omit(dat$year)))
      poly_pal  <- setNames(
        as.vector(Polychrome::createPalette(
          max(length(year_lvls), 3),
          seedcolors = c("#A9A9A9", "#FF0000", "#0000FF")
        )),
        as.character(year_lvls)
      )
      
      dat %>%
        filter(!is.na(month), !is.na(year)) %>%
        count(month, year, group, name = "n_records") %>%
        group_by(year, month) %>%
        mutate(pct = n_records / sum(n_records)) %>%
        ungroup() %>%
        mutate(month = factor(month, levels = month.name),
               year  = factor(year)) %>%
        ggplot(aes(month, pct, color = year, group = year)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2.5) +
        facet_wrap(~ group, ncol = 2) +
        scale_color_manual(values = poly_pal, drop = FALSE) +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        labs(
          x     = "Month",
          y     = "Proportion of monthly records (%)",
          title = "Monthly trends by species",
          color = "Year"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text  = element_text(face = "bold"))
    })
    
    output$salmon_fork_year_plot <- renderPlot({
      dat <- filtered_dat()
      req(nrow(dat) > 0, any(!is.na(dat$fork)))
      
      dat %>%
        filter(!is.na(year), !is.na(fork), !is.na(species)) %>%
        ggplot(aes(x = factor(year), y = fork, fill = species)) +
        geom_boxplot(outlier.alpha = 0.3) +
        facet_wrap(~ species, scales = "free_y") +
        scale_fill_manual(values = palettes$species, drop = FALSE) +
        labs(x = "Year", y = "Fork length (mm)",
             title = "Salmon fork length by year and species") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$salmon_fork_month_plot <- renderPlot({
      dat <- filtered_dat()
      req(nrow(dat) > 0, any(!is.na(dat$fork)))
      
      dat %>%
        filter(!is.na(month), !is.na(fork), !is.na(species)) %>%
        mutate(month = factor(month, levels = month.name)) %>%
        ggplot(aes(x = month, y = fork, fill = species)) +
        geom_boxplot(outlier.alpha = 0.3) +
        facet_wrap(~ species, scales = "free_y") +
        scale_fill_manual(values = palettes$species, drop = FALSE) +
        labs(x = "Month", y = "Fork length (mm)",
             title = "Salmon fork length by month and species") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$salmon_map <- renderLeaflet({
      dat <- filtered_dat()
      req(nrow(dat) > 0)
      
      site_info <- dat %>%
        select(-type) %>%
        left_join(sites_data, by = "site_id") %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        group_by(site_id, site, type, lat, lon) %>%
        summarise(
          n_records    = n(),
          species_list = paste(sort(unique(species)), collapse = ", "),
          n_species    = n_distinct(species),
          .groups      = "drop"
        )
      
      req(nrow(site_info) > 0)
      
      type_pal <- colorFactor(palette = "Set2", domain = site_info$type)
      
      leaflet(site_info) %>%
        addTiles() %>%
        addCircleMarkers(
          lng         = ~lon,
          lat         = ~lat,
          radius      = ~sqrt(n_records) + 4,
          color       = ~type_pal(type),
          fillOpacity = 0.7,
          stroke      = TRUE,
          weight      = 1,
          popup       = ~paste0(
            "<strong>Site ID: </strong>", site_id, "<br>",
            "<strong>Site: </strong>", site, "<br>",
            "<strong>Type: </strong>", type, "<br>",
            "<strong>Records: </strong>", n_records, "<br>",
            "<strong>Species (", n_species, "): </strong><br>",
            gsub(", ", "<br>", species_list)
          )
        ) %>%
        addLegend(position = "bottomright", pal = type_pal,
                  values = ~type, title = "Site type")
    })
    
    output$species_tbl <- DT::renderDT({
      filtered_dat() %>%
        filter(!is.na(species), species != "",
               !is.na(sci_name), sci_name != "") %>%
        group_by(species, sci_name, site_id, site) %>%
        summarise(`Total count` = n(), .groups = "drop") %>%
        arrange(desc(`Total count`), species) %>%
        rename(`Common name`     = species,
               `Scientific name` = sci_name,
               `Site ID`         = site_id,
               `Site`            = site) %>%
        DT::datatable(
          rownames = FALSE,
          filter   = "top",
          options  = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE)
        )
    })
    
  })
}