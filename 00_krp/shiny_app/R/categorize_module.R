# Categorize Module UI
#'
#' The UI portion of the module for displaying the categorize UI
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
categorize_ui <- function(id) {
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
        checkboxInput(
          ns("check_dedup"),
          label = "Combine transactions with identical description fields",
          value = TRUE
        ),
        checkboxInput(
          ns("check_weight"),
          label = "Weight transactions by dollar amount",
          value = TRUE
        ),
        
        titlePanel("Rules"),                
        selectInput(
          ns("select_rule_name"),
          label = "Select a rule to categorize transactions: ",
          choices = NULL
        ),
        # textOutput(
        #   ns("label_rule")
        # ),
        # tags$br(),
        actionButton(
          ns("cmd_edit_rule"),
          "Edit",
          class = "btn btn-primary btn-sm edit_btn",
          style = "color: #fff;",
          icon = icon('pencil')
        ),
        actionButton(
          ns("cmd_delete_rule"),
          "Delete",
          class = "btn btn-danger btn-sm delete_btn",
          style = "color: #fff;",
          icon = icon('trash')
        ),
        actionButton(
          ns("cmd_add_rule"),
          "New",
          class = "btn btn-sm btn-success",
          style = "color: #fff;",
          icon = icon('plus')
          # width = '100%'
        )

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
          column(
            4,
            flexdashboard::gaugeOutput(ns("gauge_matched_total")),
            textOutput(ns("text_matched_total")),
            textOutput(ns("text_unmatched_total"))
          ),
          column(
            8,
            plotOutput(ns("plot_treemap"), height = "200px")
          )
        ),
        fluidRow(
          column(
            6,
            titlePanel("Matched (Selected Rule)"),
            DTOutput(ns("txs_matched"))
            # tableOutput(ns("txs_matched"))
          ),
          column(
            6,
            titlePanel("Unmatched (All Remaining)"),
            DTOutput(ns("txs_unmatched"))
          )
        )
      ) #end mainPanel
      
    ) #end sidebarLayout
    
  ) #end tagList
      
}



categorize_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) { 
      
      # Trigger to reload rules data
      # needed if user adds a rule to db interactively
      session$userData$rules_trigger <- reactiveVal(0)
      
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
      
      # Update the rules dropdown if the rules df changes
      observeEvent(rules(), {
        updateSelectInput(
          inputId = "select_rule_name",
          choices = rules()$rule_name
        )
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
      
      
      # React to:
      # - check: dedup
      # - check: weight
      txs_dedup <- reactive({
        
        res <- txs_query()
        
        # Optionally: Collapse txs with same 'description' field
        # Will keep most recent date, and sum amounts
        if (input$check_dedup) {
          res <- 
            res |> 
            summarize(
              .by = c(account_nickname, description, type),
              date = last(date),
              amount = sum(amount, na.rm = TRUE)
            ) |> 
            select(account_nickname, date, description, type, amount)
        }
        
        # Create a .wt variable for weighting          
        # Can assume that "amount" is canonical post-ETL                  
        if (input$check_weight) {
          res$.wt <- res$amount
        } else {
          res$.wt <- 1.0          
        }
        
        res

      })

            
      # Apply rules ####
      # For each description, compute the number (0, 1, ...) of regex rules that match it
      txs_rulecheck <- reactive({
        txs_dedup() |>
          fuzzyjoin::regex_left_join(
            rules() |>
              select(.rule_name = rule_name, .rule_regex = rule_regex),
            by = c("description" = ".rule_regex"),
            # by = setNames(".rule", input$select_description),
            ignore_case = TRUE
          ) |>
          summarize(
            .by = c(account_nickname, date, description, type, amount, .wt), 
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
      
      
      
      # Compute some overall metrics (not reactive to currently seleted rule)
      overall_txs_matched <- reactive({
        txs_rulecheck() |>
          filter(.matches > 0) |>
          pull(.wt) |>
          sum(na.rm = TRUE)
      })
      
      overall_txs_unmatched <- reactive({
        txs_rulecheck() |>
          filter(.matches == 0) |>
          pull(.wt) |>
          sum(na.rm = TRUE)
      })
      
      overall_txs <- reactive({
        txs_rulecheck() |>
          pull(.wt) |>
          sum(na.rm = TRUE)
      })
      
      # Currently Matched Tx Diagnostics ----
      # Make 'matched' tx df based on selected rule
      # Count is conditional on choice of weighted or unweighted
      # Get the currently selected rule
      
      rule_regex <- reactive({
        selected_rule <- 
          rules() |> 
          filter(rule_name == input$select_rule_name) |> 
          # just to be safe, although shouldn't be any dup rule_names
          slice_head(n = 1) |> 
          pull(rule_regex)

        # If not rule currently selected, set to NA so below grepl returns        
        if (length(selected_rule) == 0L)
          selected_rule <- NA
        
        cli::cli_alert_info("selected_rule: {selected_rule}")
        selected_rule
      })
      
      # Filter the currently selected txs to those matching the rule      
      current_txs_matched <- reactive({
        txs_dedup() |>
          # Can assume that "description" is canonical post-ETL
          filter(grepl(rule_regex(), description, ignore.case = TRUE)) |> 
          # Create the table: count txs (weighted, if checked) and % of total txs        
          count(description, wt = .wt, sort = TRUE) |>
          mutate(pct = n / overall_txs())
      })

      # Get a df of all the remaining unmatched transactions
      # (txs without any matching rules)
      remaining_txs_unmatched <- reactive({
        txs_rulecheck() |> 
          filter(.matches == 0) |> 
          # Create the table: count txs (weighted, if checked) and % of total txs        
          count(description, wt = .wt, sort = TRUE) |>
          mutate(pct = n / overall_txs())
      })
      
      
      # RENDER ####
      
      # Update the gauge widget to show total rule coverage
      output$gauge_matched_total <- flexdashboard::renderGauge({
        flexdashboard::gauge(
          100 * (overall_txs_matched() / overall_txs()),
          min = 0,
          max = 100,
          symbol = "%",
          label = "% of transactions with at least 1 matching rule",
          abbreviateDecimals = 0,
          sectors = flexdashboard::gaugeSectors(
            success = c(80, 100),
            warning = c(30, 80),
            danger = c(0, 30)
          )
        )
      })

      # Update the treemap plot to show categorized txs
      output$plot_treemap <- renderPlot({
        #print(txs_rulecheck())
        txs_rulecheck() |>
          count(.rule_name, wt = .wt) |>
          ggplot(aes(area = n, fill = n, label = .rule_name)) +
          treemapify::geom_treemap() +
          treemapify::geom_treemap_text(fontface = "italic", colour = "gray", place = "centre", grow = FALSE) +
          scale_fill_viridis_c(option = "B")
      })

      # Update the text outputs to print number of matched Descriptions out of total unique
      output$text_matched_total <- renderText({
        paste0("Txs with a matched rule: ", scales::comma(overall_txs_matched(), accuracy = 1))
      })

      output$text_unmatched_total <- renderText({
        paste0("Txs without a matched rule: ", scales::comma(overall_txs_unmatched(), accuracy = 1))
      })
      
      # Print the matched txs for selected rule
      output$txs_matched <- renderDataTable(
        current_txs_matched() |>
          datatable(
            # rownames = FALSE,
            colnames = c("Description", "Count", '%'),
            selection = "none",
            class = "compact stripe row-border",
          ) |>
          formatRound("n", digits = 2) |>
          formatPercentage("pct", digits = 0)
      )
      
      # Print all remaining unmatched txs (regardless of selected rule)
      output$txs_unmatched <- renderDataTable(
        remaining_txs_unmatched() |> 
          datatable(
            # rownames = FALSE,
            colnames = c("Description", "Count", '%'),
            selection = "none",
            class = "compact stripe row-border",
          ) |>
          formatRound("n", digits = 2) |>
          formatPercentage("pct", digits = 0)
      )
      
      
      # MODALS ####
      #TODO: user new serverModule logic to nest these modules properly
      # For the dropdowns in the rules modal, pre-query DB to get the 
      # valid choices for each field; note that account_nicknames was already pulled above

      tryCatch({
        # Grab the existing data in db for this account
        valid_choices$tx_type <- 
          conn |> 
          tbl("dim_transaction_type") |> 
          select(tx_type) |> 
          collect()
        valid_choices$category_name <- 
          conn |> 
          tbl("fct_categories") |> 
          select(category_name) |> 
          collect()
        # Pulling a df to get complete category*subcategory lookup
        # Will filter to valid subcats based on selected cats within modals
        valid_choices$subcategories <- 
          conn |> 
          tbl("fct_subcategories") |> 
          select(category_name, subcategory_name) |> 
          collect()
      }, error = function(err) {
        print(err)
        showToast("error", "Error querying records from database")
      })
      
      
      callModule(
        rule_edit_module,
        "add_rule",
        modal_title = "Add Rule",
        rule_to_edit = function() NULL,
        modal_trigger = reactive({input$cmd_add_rule}),
        valid_choices = valid_choices,
        selected_accounts = reactive({input$select_account_nickname})         
      )
      
      rule_to_edit <- reactive({
        rules() |>
          filter(rule_name == input$select_rule_name)
      })
      
      callModule(
        rule_edit_module,
        "edit_rule",
        modal_title = "Edit Rule",
        rule_to_edit = rule_to_edit,
        modal_trigger = reactive({input$cmd_edit_rule}),
        valid_choices = valid_choices,
        selected_accounts = reactive({input$select_account_nickname}) 
      )
      
      rule_to_delete <- reactive({
        rules() |>
          filter(rule_name == input$select_rule_name) |>
          as.list()
      })
      
      callModule(
        rule_delete_module,
        "delete_rule",
        modal_title = "Delete Rule",
        rule_to_delete = rule_to_delete,
        modal_trigger = reactive({input$cmd_delete_rule})
      )      

            
      # RETURN ####
      return(list(
        txs_dedup = txs_dedup
      ))
      
    }
  )
}
