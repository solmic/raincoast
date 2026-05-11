# R/mod_water_properties.R

mod_water_properties_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          radioButtons(
            ns("view_by"), "View by",
            choices  = c("Year" = "year", "Month" = "month"),
            selected = "year",
            inline   = TRUE
          ),
          h4("Filters"),
          sliderInput(
            ns("year_range"), "Year range",
            min = 2016, max = 2025,
            value = c(2016, 2025), sep = ""
          ),
          selectInput(
            ns("metric"), "Metric",
            choices = c(
              "Temperature (°C)" = "temp",
              "DO (%)"           = "do_percent",
              "DO (mg/L)"        = "do_mg_l",
              "pH"               = "pH",
              "Salinity (ppt)"   = "salinity_ppt",
              "Secchi (m)"       = "secchi_m"
            ),
            selected = "temp"
          ),
          selectInput(ns("type"), "Site type",
                      choices = NULL, selected = "All", multiple = TRUE),
          uiOutput(ns("group_filter_ui")),
          uiOutput(ns("extra_filter_ui")),
          checkboxInput(
            ns("complete_x"),
            "Show all (fill missing with NA)",
            value = TRUE
          )
        )
      ),
      
      column(
        width = 9,
        h3("Water properties"),
        plotOutput(ns("ts_plot"), height = "420px"),
        hr(),
        h4("Summary statistics"),
        tableOutput(ns("summary_tbl"))
      )
    )
  )
}

mod_water_properties_server <- function(id, chem_data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      dat <- chem_data()
      req(dat)
      
      validate(
        need(all(c("date", "year", "month", "type") %in% names(dat)),
             "chem_data must include columns: date, year, month, type")
      )
      
      update_choices(session, "type", dat$type,
                     selected = isolate(input$type) %||% "All")
      
      yrs <- sort(unique(na.omit(dat$year)))
      if (length(yrs) > 1)
        updateSliderInput(session, "year_range",
                          min = min(yrs), max = max(yrs),
                          value = c(min(yrs), max(yrs)))
      
      update_choices(session, "year_pick", dat$year,
                     selected = isolate(input$year_pick) %||% "All")
      
      months_present <- month.name[month.name %in% unique(na.omit(dat$month))]
      updateSelectInput(session, "month_pick",
                        choices  = c("All", months_present),
                        selected = isolate(input$month_pick) %||% "All")
    })
    
    output$group_filter_ui <- renderUI({
      ns <- session$ns
      if (input$view_by == "year") {
        selectInput(ns("year_pick"),  "Year(s)",  choices = NULL, selected = "All", multiple = TRUE)
      } else {
        selectInput(ns("month_pick"), "Month(s)", choices = NULL, selected = "All", multiple = TRUE)
      }
    })
    
    output$extra_filter_ui <- renderUI({
      ns <- session$ns
      if (input$view_by == "year") {
        selectInput(ns("month_pick"), "Month(s) (optional)", choices = NULL, selected = "All", multiple = TRUE)
      } else {
        selectInput(ns("year_pick"),  "Year(s) (optional)",  choices = NULL, selected = "All", multiple = TRUE)
      }
    })
    
    filtered_dat <- reactive({
      dat <- chem_data()
      req(dat)
      
      dat <- dat %>%
        filter(!is.na(year),
               year >= input$year_range[1],
               year <= input$year_range[2])
      
      type_keep  <- drop_all(input$type       %||% "All")
      year_keep  <- drop_all(input$year_pick  %||% "All")
      month_keep <- drop_all(input$month_pick %||% "All")
      
      if (length(type_keep)  > 0) dat <- dat %>% filter(type  %in% type_keep)
      if (length(year_keep)  > 0) dat <- dat %>% filter(year  %in% as.integer(year_keep))
      if (input$view_by == "year" && length(month_keep) > 0)
        dat <- dat %>% filter(month %in% month_keep)
      
      dat
    })
    
    output$summary_tbl <- renderTable({
      dat    <- filtered_dat()
      metric <- input$metric
      req(dat)
      validate(need(metric %in% names(dat), paste("Column not found:", metric)))
      
      dat %>%
        filter(!is.na(.data[[metric]])) %>%
        group_by(type) %>%
        summarise(
          `N samples` = n(),
          Mean        = mean(.data[[metric]], na.rm = TRUE),
          SD          = sd(.data[[metric]],   na.rm = TRUE),
          Min         = min(.data[[metric]],   na.rm = TRUE),
          Max         = max(.data[[metric]],   na.rm = TRUE),
          .groups     = "drop"
        )
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    output$ts_plot <- renderPlot({
      dat    <- filtered_dat()
      metric <- input$metric
      req(nrow(dat) > 0)
      validate(need(metric %in% names(dat), paste("Column not found:", metric)))
      
      dat <- dat %>% filter(!is.na(.data[[metric]]))
      
      if (input$view_by == "year") {
        ts <- dat %>%
          group_by(type, year) %>%
          summarise(value = mean(.data[[metric]], na.rm = TRUE),
                    n = n(), .groups = "drop")
        
        if (isTRUE(input$complete_x))
          ts <- ts %>%
            complete(year = seq(input$year_range[1], input$year_range[2]),
                     type,
                     fill = list(value = NA_real_, n = 0))
        
        ggplot(ts, aes(x = year, y = value, color = type, group = type)) +
          geom_line(linewidth = 0.9, alpha = 0.9, na.rm = TRUE) +
          geom_point(size = 2, alpha = 0.9, na.rm = TRUE) +
          labs(x = "Year", y = metric,
               title = "Trends through time", color = "Site type") +
          theme_minimal()
        
      } else {
        month_keep     <- drop_all(input$month_pick %||% "All")
        months_to_show <- if (length(month_keep) > 0) {
          month.name[month.name %in% month_keep]
        } else {
          month.name
        }
        
        ts <- dat %>%
          mutate(month = factor(month, levels = month.name)) %>%
          filter(month %in% months_to_show) %>%
          group_by(type, month) %>%
          summarise(value = mean(.data[[metric]], na.rm = TRUE),
                    n = n(), .groups = "drop")
        
        if (isTRUE(input$complete_x))
          ts <- ts %>%
          complete(month = factor(months_to_show, levels = month.name),
                   type,
                   fill = list(value = NA_real_, n = 0))
        
        ggplot(ts, aes(x = month, y = value, color = type, group = type)) +
          geom_line(linewidth = 0.9, alpha = 0.9, na.rm = TRUE) +
          geom_point(size = 2, alpha = 0.9, na.rm = TRUE) +
          labs(x = "Month", y = metric,
               title = "Seasonality (across selected years)", color = "Site type") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
    
  })
}