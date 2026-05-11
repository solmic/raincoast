# ui.R

ui <- fluidPage(
  theme = bs_theme(version = 5),
  
  titlePanel("Fraser River Estuary Fish Explorer"),
  br(),
  
  tabsetPanel(
    tabPanel(
      "Welcome to the Dataset",
      mod_intro_ui("intro")
    ),
    tabPanel(
      "General Fish Population",
      mod_general_population_ui("general")
    ),
    tabPanel(
      "Salmon",
      mod_salmon_ui("salmon")
    ),
    tabPanel(
      "Forage Fish",
      mod_forage_fish_ui("forage_fish")
    ),
    tabPanel(
      "Water Properties",
      mod_water_properties_ui("water")
    )
  )
)