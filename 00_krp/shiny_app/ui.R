fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  # Application Title
  titlePanel(
    h1("PEI Budget", align = 'left'),
    windowTitle = "PEI Budget"
  ),
  # /// pretty sure this is where I can implement the tabbed nav
  #     and then call the UI functions directly within each tab
  rules_table_ui("rules_table")
)

