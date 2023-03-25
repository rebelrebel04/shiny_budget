analyze_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        titlePanel("Transactions"),      
        # tags$div(HTML("<i>Upload transaction data as csv</i>")),        
        dateRangeInput(
          ns("select_date_range"),
          label = "Select date range: "
        ),
        selectInput(
          ns("select_account_nickname"),
          label = "Select accounts to test: ",
          choices = NULL,
          multiple = TRUE
        ),
        # checkboxInput(
        #   ns("check_dedup"),
        #   label = "Combine transactions with identical description fields",
        #   value = TRUE
        # ),
        # checkboxInput(
        #   ns("check_weight"),
        #   label = "Weight transactions by dollar amount",
        #   value = TRUE
        # ),
        
        # titlePanel("Rules"),                
        # selectInput(
        #   ns("select_rule_name"),
        #   label = "Select a rule to categorize transactions: ",
        #   choices = NULL
        # ),
        # textOutput(
        #   ns("label_rule")
        # ),
        # tags$br(),
        # actionButton(
        #   ns("cmd_edit_rule"),
        #   "Edit",
        #   class = "btn btn-primary btn-sm edit_btn",
        #   style = "color: #fff;",
        #   icon = icon('pencil')
        # ),
        # actionButton(
        #   ns("cmd_delete_rule"),
        #   "Delete",
        #   class = "btn btn-danger btn-sm delete_btn",
        #   style = "color: #fff;",
        #   icon = icon('trash')
        # ),
        # actionButton(
        #   ns("cmd_add_rule"),
        #   "New",
        #   class = "btn btn-sm btn-success",
        #   style = "color: #fff;",
        #   icon = icon('plus')
        #   # width = '100%'
        # )

        # tags$br(),
        # 
        # actionButton(
        #   ns("cmd_apply"),
        #   "Apply & Save",
        #   class = "btn btn-success apply_btn",
        #   style = "color: #fff;",
        #   icon = icon('bolt')
        # )
      ), #end sidebarPanel
      
      mainPanel(
        fluidRow(
        )
      ) #end mainPanel
      
    ) #end sidebarLayout
    
  ) #end tagList
    
}


analyze_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Query Rules data ####
      # Lazy-load rules data (reactive to selected account)
      rules <- reactive({
        session$userData$rules_trigger()
        res <- NULL
        tryCatch({
          res <- 
            conn |> 
            tbl("fct_rules") |> 
            collect() |> 
            filter(account_nickname %in% input$select_account_nickname)
            # filter(account_nickname == input$select_account_nickname)          
        }, error = function(err) {
          print(err)
          showToast("error", "Error querying records from database")
        })
        # print(glimpse(res))
        res
      })
      
      # OnLoad ####
      valid_choices <- list()
      # Populate account_nickname selections
      valid_choices$account_nickname <- NULL
      tryCatch({
        # Grab the existing data in db for this account
        valid_choices$account_nickname <- 
          conn |> 
          tbl("fct_accounts") |> 
          select(account_nickname) |> 
          collect()
      }, error = function(err) {
        print(err)
        showToast("error", "Error querying records from database")
      })
      updateSelectInput(
        inputId = "select_account_nickname",
        choices = valid_choices$account_nickname
      )
      
      
      # Query Tx data ####
      # Tx data will be reactive to:
      # - select: date range
      # - select: account nickname
      txs_query <- reactive({
        res <- NULL
        tryCatch({
          res <- 
            conn |> 
            tbl("fct_transactions") |> 
            select(account_nickname, date, description, type, amount) |> 
            collect() |> 
            filter(account_nickname %in% input$select_account_nickname) |> 
            filter(date >= input$select_date_range[1] & date <= input$select_date_range[2])
        }, error = function(err) {
          print(err)
          showToast("error", "Error querying records from database")
        })
        # print(glimpse(res))
        
        res
      })      
      
      
      # Apply rules ####
      # For each description, compute the number (0, 1, ...) of regex rules that match it
      # Keep the first matching rule to use for joining on rule's metadata (category etc.)
      txs_joined <- reactive({
        txs_query() |>
          fuzzyjoin::regex_left_join(
            rules(),
            by = c("description" = "rule_regex"),
            ignore_case = TRUE
          ) |>
          summarize(
            .by = c(account_nickname, date, description, type, amount, ), 
            # compute the number of matching rules (zero or more) for each tx
            .matches = sum(purrr::map_int(.rule_name, \(x) ifelse(is.na(x), 0L, 1L))),
            # also retain the first key matched, if any
            .rule_name = first(.rule_name)
          ) |>
          ungroup() |>
          mutate(
            .rule_name = ifelse(is.na(.rule_name), "NONE", .rule_name)
          )
      })      
      
    }
  )
}