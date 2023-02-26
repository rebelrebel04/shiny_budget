# fluidPage(
#   shinyFeedback::useShinyFeedback(),
#   shinyjs::useShinyjs(),
#   # Application Title
#   titlePanel(
#     h1("PEI Budget", align = 'left'),
#     windowTitle = "PEI Budget"
#   ),
#   # /// pretty sure this is where I can implement the tabbed nav
#   #     and then call the UI functions directly within each tab
#   rules_table_ui("rules_table")
# )

# https://shiny.rstudio.com/gallery/navbar-example.html
navbarPage(
  "PEI Budget",

    
  # LOAD ####
  # This tab lets the user 
  # - upload a new transactions csv
  # - choose the preprocessing function to clean it up
  # - write (append) the transactions to fct_transactions    
  tabPanel(
    "Load",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        etl_file_ui("etl_file") #, "User data (.csv format)")
      ),
      mainPanel(
        # textOutput("text"),
        dataTableOutput("etl_table")
      )
    ),
    icon = icon("database")
  ),

    
  # CATEGORIZE ####  
  # This tab lets the user:
  # - query fct_transactions (filter by: date range, account_nickname) & vis in a DT
  # - test & visualize category rules, and thereby CRUD fct_rules
  #   - NOTE: rule joins are NOT actually serialized here; the goal is just to 
  #     qc the quality of the rules, and perform the joins at analysis time;
  #     as a result, txs with multiple matching rules should be flagged
  tabPanel(
    "Categorize",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        # rules_table_ui("rules_table")                
      ),
      mainPanel(
        # textOutput("text"),
        # dataTableOutput("table")
      )
    ),
    icon = icon("shapes")
  ),

    
  # ANALYZE ####  
  # This tab lets the user:
  # - query fct_transactions after joining on metdata (cats, tags, etc.) via fct_rules
  #   - NOTE: only the `first` matching rule is applied if >1 matches
  # - interactively visualize tx sums with different groupings/filters
  tabPanel(
    "Analyze",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        # csvFileUI("datafile", "User data (.csv format)")
      ),
      mainPanel(
        # textOutput("text"),
        # dataTableOutput("table")
      )
    ),
    icon = icon("chart-simple")
  )
)