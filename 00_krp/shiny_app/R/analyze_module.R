analyze_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(type = 'text/css', 'body { overflow-y: scroll; }'),
    sidebarLayout(
      sidebarPanel(
        
        titlePanel("Transactions"),      
        # tags$div(HTML("<i>Upload transaction data as csv</i>")),        
        dateRangeInput(
          ns("select_date_range"),
          label = "Select date range: ",
          start = floor_date(Sys.Date(), "month") %m-% months(3),          
          end = floor_date(Sys.Date(), "month") - days(1),
          startview = "year"
        ),
        sliderInput(
          ns("slider_rolling"),
          label = "Rolling average window (months):",
          min = 0L,
          max = 3L,
          value = 0L,
          step = 1L,
          ticks = FALSE
        ),
        selectInput(
          ns("select_comp"),
          label = "Select comparison: ",
          choices = c("PoP", "YoY"),
          multiple = FALSE
        ),
        selectInput(
          ns("select_account_nickname"),
          label = "Select accounts to test: ",
          choices = NULL,
          multiple = TRUE
        ),
        # selectInput(
        #   ns("select_category_name"),
        #   label = "Select categories:", 
        #   choices = NULL,
        #   multiple = TRUE
        # ),
        # selectInput(
        #   ns("select_tags"),
        #   label = "Select tags:", 
        #   choices = NULL,
        #   multiple = TRUE
        # )
        shinyWidgets::pickerInput(
          ns("select_category_name"),
          label = "Select categories:",
          choices = NULL,
          options = list("actions-box" = TRUE),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          ns("select_tags"),
          label = "Select tags:",
          choices = NULL,
          options = list("actions-box" = TRUE),
          multiple = TRUE
        ),
        checkboxInput(
          ns("check_max_amount"),
          label = "Exclude expenses with amount greater than:",
          value = FALSE
        ),
        numericInput(
          ns("numeric_max_amount"),
          label = "",
          value = 5000L,
          min = 0L,
          max = NA_integer_,
          step = 500L,
          width = "100px"
        )
      ), #end sidebarPanel
      
      mainPanel(
        fluidRow(
          column(
            12,
            plotlyOutput(ns("plotly_categories"))
          ),
          column(
            12,
            plotlyOutput(ns("plotly_netincome"))        
          ),
          column(
            12,
            plotlyOutput(ns("plotly_tx_histogram"))        
          )
        )


        # dataTableOutput(ns("table"))
      ) #end mainPanel
      
    ) #end sidebarLayout
    
  ) #end tagList
    
}


analyze_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # OnLoad: Query rules data ####
      # Not reactive, just query once for entire table
      rules <- NULL
      tryCatch({
        rules <- 
          conn |> 
          tbl("fct_rules") |> 
          collect()
      }, error = function(err) {
        print(err)
        showToast("error", "Error querying records from database")
      })
      # glimpse(rules)
      
      
      # OnLoad: Update inputs ####
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

      
      # Update categories & tags based on selected accounts
      observeEvent(input$select_account_nickname, {
        active_rules <- 
          rules |> 
          filter(account_nickname %in% input$select_account_nickname)
        valid_categories <- pull(distinct(active_rules, category_name), category_name)
        valid_tags <- pull(distinct(active_rules, tags), tags)
        # updateSelectInput(
        updatePickerInput(
          session,
          inputId = "select_category_name",
          choices = valid_categories,
          selected = valid_categories
        )
        # updateSelectInput(
        updatePickerInput(
          session,
          inputId = "select_tags",
          choices = valid_tags,
          selected = valid_tags
        )
      })
      
      # OnLoad: Update accounts list with valid choices based on db      
      updateSelectInput(
        inputId = "select_account_nickname",
        choices = valid_choices$account_nickname,
        # Set the default selections for analysis
        selected = c("BC Joint Checking", "Chase Visa", "CO MasterCard")
      )
      
      
      # OnLoad: Query Tx data ####
      # Note: not reactive, just query entire tx table once on load
      txs_query <- NULL
      tryCatch({
        txs_query <- 
          conn |> 
          tbl("fct_transactions") |> 
          collect() |> 
          select(transaction_id, account_nickname, date, description, type, amount)
      }, error = function(err) {
        print(err)
        showToast("error", "Error querying records from database")
      })
      # glimpse(txs_query)

            
      # Filter txs based on UI selections ####
      # Tx data will be reactive to:
      # - select: account nickname      
      # - select: date range
      # - numeric: max amount
      txs_filtered <- reactive({
        df <- 
          txs_query |> 
          filter(account_nickname %in% input$select_account_nickname) |> 
          filter(date >= input$select_date_range[1] & date <= input$select_date_range[2])
        # Optionally: exclude expenses (type=debit) above input max
        if (input$check_max_amount)
          df <- filter(df, !(type == "debit" & amount >= input$numeric_max_amount))
        df
      })
      
      
      # Apply rules ####
      # For each description, compute the number (0, 1, ...) of regex rules that match it
      # Keep the first matching rule to use for joining on rule's metadata (category etc.)
      # Reactive to:
      # - txs_filtered() [changes to accounts or date range]
      txs_joined <- reactive({
        txs_filtered() |> 
          # Join on rule metadata, separately for each account
          # (since rules are defined independently for each account)
          split(~account_nickname) |> 
          imap(
            \(x, nm) fuzzyjoin::regex_left_join(
              x,
              # Filter to only the rules for the account being iterated over
              rules |> filter(account_nickname == nm) |> select(-account_nickname),
              by = c("description" = "rule_regex"),
              ignore_case = TRUE
            ) |> 
              # Keep only the first matching rule
              distinct(transaction_id, .keep_all = TRUE)
          ) |> 
          list_rbind()
      })
      
      # Tx analysis data will be reactive to:
      # - txs_joined()
      # - select: categories
      # - select: tags
      txs_analyze <- reactive({
        txs_joined() |> 
          # Filter to selected categories & tags
          filter(category_name %in% input$select_category_name) |> 
          filter(tags %in% input$select_tags) |> 
          mutate(
            ym = floor_date(as.Date(date), "months")
          )
      })
      
      # OUTPUT ####
      output$plotly_categories <- renderPlotly({
        p <- 
          txs_analyze() |> 
          filter(tx_type == "expense") |> 
          summarize(
            .by = c(ym, category_name),
            amount = sum(amount, na.rm = TRUE)
          ) |> 
          
          ggplot(aes(x = ym, y = amount, fill = category_name)) +
          geom_bar(stat = "identity") +  
          scale_fill_viridis_d("Category", option = "C") +
          scale_x_date("Month", date_labels = "%b-%y", date_breaks = "1 month") +
          scale_y_continuous("Amount", labels = scales::dollar) +
          ggtitle("Monthly Expenses by Category")
        ggplotly(p)
      })
      
      output$plotly_netincome <- renderPlotly({
        df <- 
          txs_analyze() |> 
          filter(tx_type %in% c("expense", "income")) |> 
          mutate(
            # make expenses negative
            amount = ifelse(tx_type == "expense", -1 * amount, amount)
          ) |> 
          summarize(
            .by = c(ym),
            amount = sum(amount, na.rm = TRUE)
          )
          
        p <- 
          df |> 
          ggplot(aes(x = ym, y = amount, fill = amount < 0)) +
          geom_bar(stat = "identity") +  
          scale_x_date("Month", date_labels = "%b-%y", date_breaks = "1 month") +
          scale_y_continuous("Net Income", labels = scales::dollar) +
          scale_fill_manual(values = c("lightgreen", "pink"), drop = FALSE) +
          theme(legend.position = "none") +
          ggtitle(glue("Monthly Net Income (Cumulative: {scales::dollar(sum(df$amount, na.rm = TRUE), accuracy = 1)})"))
        ggplotly(p)
      })
      
      output$plotly_tx_histogram <- renderPlotly({
        p <- 
          txs_analyze() |> 
          filter(tx_type == "expense") |> 
          mutate(
            text = glue("{subcategory_name}\n{stringr::str_trunc(description, 25)}\n{scales::dollar(amount, accuracy = 1)}")
          ) |> 
          
          ggplot(aes(x = log(amount), fill = category_name, text = text)) +
          geom_histogram(bins = 30) +
          scale_fill_viridis_d("Category", option = "C") +
          xlab("") +
          ylab("Count") +
          ggtitle("Transaction Distribution (Log)")
        # https://plotly-r.com/controlling-tooltips.html        
        ggplotly(p, tooltip = c("fill", "text", "y"))        
      })
      
      
    }
  )
}