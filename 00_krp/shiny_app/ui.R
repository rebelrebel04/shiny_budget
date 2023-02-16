fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  # Application Title
  titlePanel(
    h1("PEI Budget", align = 'left'),
    windowTitle = "PEI Budget"
  ),
  rules_table_module_ui("rules_table")
)

