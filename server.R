# server.R

server <- function(input, output, session) {
  
  fraser_data <- reactive(fraser_clean)
  water_data  <- reactive(water_clean)
  salmon_data <- reactive(salmon_clean)
  forage_data <- reactive(forage_clean)
  
  mod_intro_server("intro",               fraser_data, sites_clean)
  mod_general_population_server("general", fraser_data, sites_clean)
  mod_water_properties_server("water",     water_data)
  mod_salmon_server("salmon",              salmon_data)
  mod_forage_fish_server("forage_fish",    forage_data, salmon_data)
}