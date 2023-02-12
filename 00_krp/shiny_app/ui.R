fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  # Application Title
  titlePanel(
    h1("PEI Budget", align = 'center'),
    windowTitle = "PEI Budget"
  ),
  cars_table_module_ui("cars_table")
)

