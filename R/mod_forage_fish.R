# R/mod_forage_fish.R

mod_forage_fish_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          style = "position: sticky; top: 20px;",
          radioButtons(
            ns("view_by"), "View by",
            choices  = c("Species" = "species", "Family" = "family", "Site" = "site"),
            selected = "species",
            inline   = TRUE
          ),
          h4("Filters"),
          sliderInput(
            ns("year_range"), "Year range",
            min = 2016, max = 2025,
            value = c(2016, 2025), sep = ""
          ),
          selectInput(ns("type"),    "Habitat",  choices = NULL, selected = "All"),
          selectInput(ns("family"),  "Family",   choices = NULL, selected = "All", multiple = TRUE),
          selectInput(ns("site_id"), "Site ID",  choices = NULL, selected = "All", multiple = TRUE),
          selectInput(ns("species"), "Species",  choices = NULL, selected = "All", multiple = TRUE),
          checkboxInput(
            ns("complete_years"),
            "Show all years (fill missing with 0)",
            value = TRUE
          )
        )
      ),
      
      column(
        width = 9,
        h3("Forage Fish"),
        uiOutput(ns("kpi_row")),
        br(),
        plotOutput(ns("forage_time_plot"),       height = "420px"),
        hr(),
        plotOutput(ns("forage_time_month_plot"), height = "1000px"),
        hr(),
        h4("Site Map"),
        leafletOutput(ns("forage_map"), height = "500px"),
        hr(),
        h4("Co-occurrence with Salmon"),
        plotOutput(ns("cooc_heatmap"), height = "500px"),
        hr(),
        h4("Species present"),
        DTOutput(ns("species_tbl")),
        br()
      )
    )
  )
}

mod_forage_fish_server <- function(id, forage_data, salmon_data, sites_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    palettes <- reactiveValues(species = NULL, family = NULL, site = NULL)
    
    observe({
      dat <- forage_data()
      req(dat)
      
      update_choices(session, "type",    dat$type)
      update_choices(session, "family",  dat$family)
      update_choices(session, "site_id", dat$site_id)
      update_choices(session, "species", dat$species)
      
      yrs <- sort(unique(na.omit(dat$year)))
      if (length(yrs) > 1)
        updateSliderInput(session, "year_range",
                          min = min(yrs), max = max(yrs),
                          value = c(min(yrs), max(yrs)))
      
      palettes$species <- paired_map(sort(unique(na.omit(dat$species))))
      palettes$family  <- paired_map(sort(unique(na.omit(dat$family))))
      palettes$site    <- paired_map(sort(unique(na.omit(dat$site))))
    })
    
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
          title = "Families present",
          value = textOutput(ns("kpi_n_families")),
          theme = bslib::value_box_theme(bg = "#10b981", fg = "white")
        ),
        if (input$view_by == "site") {
          bslib::value_box(
            title = "Sites shown",
            value = textOutput(ns("kpi_n_groups")),
            theme = bslib::value_box_theme(bg = "#f59e0b", fg = "white")
          )
        },
        col_widths = if (input$view_by == "site") c(3, 3, 3, 3) else c(4, 4, 4)
      )
    })
    
    filtered_dat <- reactive({
      dat <- forage_data()
      req(dat)
      
      dat <- dat %>%
        filter(!is.na(year),
               year >= input$year_range[1],
               year <= input$year_range[2])
      
      if (!is.null(input$type) && input$type != "All")
        dat <- dat %>% filter(type == input$type)
      
      fam_keep  <- drop_all(input$family)
      site_keep <- drop_all(input$site_id)
      sp_keep   <- drop_all(input$species)
      
      if (length(fam_keep)  > 0) dat <- dat %>% filter(family  %in% fam_keep)
      if (length(site_keep) > 0) dat <- dat %>% filter(site_id %in% site_keep)
      if (length(sp_keep)   > 0) dat <- dat %>% filter(species %in% sp_keep)
      
      dat %>%
        mutate(group = .data[[input$view_by]]) %>%
        filter(!is.na(group), group != "")
    })
    
    output$kpi_total_records <- renderText({ format(nrow(filtered_dat()), big.mark = ",") })
    output$kpi_n_species     <- renderText({ n_distinct(filtered_dat()$species, na.rm = TRUE) })
    output$kpi_n_families    <- renderText({ n_distinct(filtered_dat()$family,  na.rm = TRUE) })
    output$kpi_n_groups      <- renderText({ n_distinct(filtered_dat()$site_id, na.rm = TRUE) })
    
    counts_year_group <- reactive({
      ts <- filtered_dat() %>%
        count(year, group, name = "n_records") %>%
        group_by(year) %>%
        mutate(pct = n_records / sum(n_records)) %>%
        ungroup()
      
      if (isTRUE(input$complete_years)) {
        ts <- ts %>%
          complete(
            year = seq(input$year_range[1], input$year_range[2]),
            group,
            fill = list(n_records = NA, pct = NA)
          )
      }
      ts
    })
    
    output$forage_time_plot <- renderPlot({
      ts <- counts_year_group()
      req(nrow(ts) > 0, !is.null(palettes$species))
      
      pal <- switch(input$view_by,
                    species = palettes$species,
                    family  = palettes$family,
                    site    = palettes$site)
      
      ggplot(ts, aes(year, pct, color = group, group = group)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2.5) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2])) +
        scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 1)) +
        scale_color_manual(values = pal, drop = FALSE) +
        labs(
          x     = "Year",
          y     = "Proportion of annual records (%)",
          title = "Forage fish trends through time",
          color = switch(input$view_by, species = "Species", family = "Family", site = "Site")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$forage_time_month_plot <- renderPlot({
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
        # Species not shown as monthly panels
        filter(!(input$view_by == "species" &
                   tolower(group) %in% c("alewife", "american shad",
                                         "forage fish", "longfin smelt",
                                         "kelp perch", "perch"))) %>%
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
          title = "Monthly trends by group",
          color = "Year"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text  = element_text(face = "bold"))
    })
    
    output$forage_map <- renderLeaflet({
      dat <- filtered_dat()
      req(nrow(dat) > 0)
      
      site_info <- dat %>%
        select(-type) %>%
        left_join(sites_data, by = "site_id") %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        group_by(site_id, type, lat, lon) %>%
        summarise(
          n_records    = n(),
          species_list = paste(sort(unique(species)), collapse = ", "),
          family_list  = paste(sort(unique(family)),  collapse = ", "),
          n_species    = n_distinct(species),
          n_families   = n_distinct(family),
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
            "<strong>Site: </strong>", site_id, "<br>",
            "<strong>Habitat: </strong>", type, "<br>",
            "<strong>Records: </strong>", n_records, "<br>",
            "<strong>Families (", n_families, "): </strong>", family_list, "<br>",
            "<strong>Species (", n_species, "): </strong><br>",
            gsub(", ", "<br>", species_list)
          )
        ) %>%
        addLegend(position = "bottomright", pal = type_pal,
                  values = ~type, title = "Habitat")
    })
    
    cooc_dat <- reactive({
      ff  <- filtered_dat()
      sal <- salmon_data()
      req(nrow(ff) > 0, nrow(sal) > 0)
      
      ff_presence <- ff %>%
        filter(!is.na(species), !is.na(site_id), !is.na(year), !is.na(month)) %>%
        distinct(site_id, year, month, forage_species = species)
      
      sal_presence <- sal %>%
        filter(!is.na(species), !is.na(site_id), !is.na(year), !is.na(month)) %>%
        distinct(site_id, year, month, salmon_species = species)
      
      inner_join(ff_presence, sal_presence,
                 by           = c("site_id", "year", "month"),
                 relationship = "many-to-many")
    })
    
    output$cooc_heatmap <- renderPlot({
      dat <- cooc_dat()
      req(nrow(dat) > 0)
      
      dat %>%
        count(forage_species, salmon_species, name = "n_cooc") %>%
        ggplot(aes(x = salmon_species, y = forage_species, fill = n_cooc)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = n_cooc), size = 3.5, color = "white") +
        scale_fill_viridis_c(option = "mako", direction = -1) +
        labs(
          x     = "Salmon species",
          y     = "Forage fish species",
          fill  = "Co-occurrence events",
          title = "Co-occurrence heatmap: forage fish \u00d7 salmon species"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$species_tbl <- DT::renderDT({
      filtered_dat() %>%
        filter(!is.na(species), species != "",
               !is.na(sci_name), sci_name != "") %>%
        group_by(species, sci_name, family) %>%
        summarise(`Total count` = n(), .groups = "drop") %>%
        arrange(desc(`Total count`), species) %>%
        rename(`Common name`     = species,
               `Scientific name` = sci_name,
               `Family`          = family) %>%
        DT::datatable(
          rownames = FALSE,
          filter   = "top",
          options  = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE)
        )
    })
    
  })
}