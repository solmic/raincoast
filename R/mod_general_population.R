# R/mod_general_population.R

mod_general_population_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          radioButtons(
            ns("view_by"), "View by",
            choices  = c("Functional group" = "functional_group", "Family" = "family"),
            selected = "functional_group",
            inline   = TRUE
          ),
          h4("Filters"),
          sliderInput(
            ns("year_range"), "Year range",
            min = 2016, max = 2025,
            value = c(2016, 2025), sep = ""
          ),
          selectInput(ns("type"), "Site type", choices = NULL, selected = "All"),
          uiOutput(ns("group_filter_ui")),
          checkboxInput(
            ns("complete_years"),
            "Show all years (fill missing with 0)",
            value = TRUE
          )
        )
      ),

      column(
        width = 9,
        h3("General Fish Population"),

        bslib::layout_columns(
          bslib::value_box(
            title = "Total records",
            value = textOutput(ns("kpi_total_records")),
            theme = bslib::value_box_theme(bg = "#3b82f6", fg = "white")
          ),
          bslib::value_box(
            title = "Species",
            value = textOutput(ns("kpi_n_species")),
            theme = bslib::value_box_theme(bg = "white", fg = "#111827")
          ),
          bslib::value_box(
            title = "Years",
            value = textOutput(ns("kpi_n_years")),
            theme = bslib::value_box_theme(bg = "#10b981", fg = "#111827")
          ),
          bslib::value_box(
            title = uiOutput(ns("kpi_group_label")),
            value = textOutput(ns("kpi_n_groups")),
            theme = bslib::value_box_theme(bg = "#f59e0b", fg = "white")
          ),
          col_widths = c(3, 3, 3, 3)
        ),

        br(),
        plotOutput(ns("fg_time_plot"), height = "420px"),
        hr(),
        h4("Site Map"),
        leafletOutput(ns("general_map"), height = "500px"),
        hr(),
        br(),
        h4("Species List"),
        DTOutput(ns("species_tbl"))
      )
    )
  )
}

mod_general_population_server <- function(id, fraser_data, sites_data) {
  moduleServer(id, function(input, output, session) {

    palettes <- reactiveValues(fg = NULL, fam = NULL)

    observe({
      dat <- fraser_data()
      req(dat)

      update_choices(session, "type", dat$type)

      fg_lvls  <- sort(unique(na.omit(dat$functional_group)))
      fam_lvls <- sort(unique(na.omit(dat$family)))

      palettes$fg  <- paired_map(fg_lvls)
      clrs <- as.vector(Polychrome::createPalette(
        max(length(fam_lvls), 3),
        seedcolors = c("#A9A9A9", "#FF0000", "#0000FF")
      ))
      palettes$fam <- setNames(clrs, fam_lvls)
    })

    output$kpi_group_label <- renderUI({
      if (input$view_by == "family") "Families present" else "Functional groups present"
    })

    output$group_filter_ui <- renderUI({
      ns <- session$ns
      dat <- fraser_data()
      req(dat)

      if (input$view_by == "functional_group") {
        fg_choices <- c("All", sort(unique(na.omit(dat$functional_group))))
        selectizeInput(
          ns("functional_group"), "Functional group(s)",
          choices  = fg_choices,
          selected = intersect(isolate(input$functional_group), fg_choices) %||% "All",
          multiple = TRUE,
          options  = list(plugins = list("remove_button"), placeholder = "Select one or more…")
        )
      } else {
        fam_choices <- c("All", sort(unique(na.omit(dat$family))))
        selectizeInput(
          ns("family"), "Family",
          choices  = fam_choices,
          selected = intersect(isolate(input$family), fam_choices) %||% "All",
          multiple = TRUE,
          options  = list(plugins = list("remove_button"), placeholder = "Select one or more…")
        )
      }
    })

    filtered_dat <- reactive({
      dat <- fraser_data()
      req(dat)

      dat <- dat %>%
        filter(!is.na(year),
               year >= input$year_range[1],
               year <= input$year_range[2])

      if (!is.null(input$type) && input$type != "All")
        dat <- dat %>% filter(type == input$type)

      fg_keep  <- drop_all(input$functional_group %||% "All")
      fam_keep <- drop_all(input$family %||% "All")

      if (length(fg_keep)  > 0) dat <- dat %>% filter(functional_group %in% fg_keep)
      if (length(fam_keep) > 0) dat <- dat %>% filter(family %in% fam_keep)

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
            fill = list(n_records = 0)
          )
      }

      ts %>%
        group_by(year) %>%
        mutate(pct = n_records / sum(n_records)) %>%
        ungroup()
    })

    output$fg_time_plot <- renderPlot({
      ts <- counts_year_group()
      req(nrow(ts) > 0, !is.null(palettes$fg), !is.null(palettes$fam))

      p <- ggplot(ts, aes(year, pct, fill = group, group = group)) +
        geom_area() +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2])) +
        scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 1)) +
        labs(
          x    = "Year",
          y    = "Proportion of annual records (%)",
          title = "Trends through time",
          fill  = ifelse(input$view_by == "family", "Family", "Functional group")
        ) +
        theme_minimal()

      if (input$view_by == "functional_group") {
        p <- p + scale_fill_manual(values = palettes$fg, drop = FALSE)
      } else {
        p <- p + scale_fill_manual(values = palettes$fam, drop = FALSE)
      }
      p
    })

    output$general_map <- renderLeaflet({
      dat <- filtered_dat()
      req(nrow(dat) > 0)

      site_info <- dat %>%
        left_join(sites_data, by = "site_id") %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        group_by(site_id, type, lat, lon) %>%
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
            "<strong>Site: </strong>", site_id, "<br>",
            "<strong>Habitat: </strong>", type, "<br>",
            "<strong>Records: </strong>", n_records, "<br>",
            "<strong>Species (", n_species, "): </strong><br>",
            gsub(", ", "<br>", species_list)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = type_pal,
          values   = ~type,
          title    = "Habitat"
        )
    })

    output$species_tbl <- DT::renderDT({
      dat <- filtered_dat()
      req(dat)

      dat %>%
        filter(!is.na(species), species != "",
               !is.na(sci_name), sci_name != "") %>%
        group_by(species, sci_name, family, site_id) %>%
        summarise(`Total count` = n(), .groups = "drop") %>%
        arrange(desc(`Total count`), species) %>%
        rename(
          `Common name`     = species,
          `Scientific name` = sci_name,
          `Family`          = family,
          `Site ID`         = site_id
        ) %>%
        DT::datatable(
          rownames = FALSE,
          filter   = "top",
          options  = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE)
        )
    })

  })
}