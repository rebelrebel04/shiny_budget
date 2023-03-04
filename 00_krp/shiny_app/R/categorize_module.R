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
          label = "Select account to test: ",
          choices = NULL,
          multiple = TRUE
        ),
        checkboxInput(
          ns("check_dedup"),
          label = "Combine transactions with identical description fields"
        ),
        checkboxInput(
          ns("check_weight"),
          label = "Weight transactions by dollar amount"
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
      # session$userData$rules_trigger <- reactiveVal(0)
      
      # Query Rules data ####
      # Lazy-load rules data (reactive to selected account)
      rules <- reactive({
        #session$userData$rules_trigger()
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
      # Populate account_nickname selections
      observe({
        account_nicknames <- NULL
        tryCatch({
          # Grab the existing data in db for this account
          account_nicknames <- 
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
          choices = account_nicknames
        )
        # Invalidate the rules trigger
        #session$userData$rules_trigger(session$userData$rules_trigger() + 1)
      })
      
      # Query Tx data ####
      # Tx data will be reactive to:
      # - select: date range
      # - select: account nickname
      # - check: dedup
      txs_dedup <- reactive({
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
        
        res
      })
      
      
      
      
      # RENDER ####
      output$txs_matched <- renderDataTable({
        txs_dedup()
      })
      
      
      # MODALS ####
      #TODO: user new serverModule logic to nest these modules properly
      callModule(
        rule_edit_module,
        "add_rule",
        modal_title = "Add Rule",
        rule_to_edit = function() NULL,
        modal_trigger = reactive({input$cmd_add_rule})
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
        modal_trigger = reactive({input$cmd_edit_rule})
      )
      
      rule_to_delete <- reactive({
        rules() |>
          filter(rule_name == input$select_rule_name) |>
          as.list()
      })
      
      # rule_to_delete <- eventReactive(input$rule_id_to_delete, {
      #   rules() |>
      #     filter(uid == input$rule_id_to_delete) |>
      #     as.list()
      # })
      
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




#' Rules Table Module Server
#'
#' The Server portion of the module for displaying the rules datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

categorize_server_old <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # trigger to reload data from the "rules" table
      session$userData$rules_trigger <- reactiveVal(0)
      
      # Rules DB Connection ####
      # Get the name of the rules table configured for this db
      rules_table <- db_config$rules_table
      
      # Read in selected "rules" table from the database
      # accounts <- reactive({
      #   session$userData$rules_trigger()
      # 
      #   out <- NULL
      #   tryCatch({
      #     accounts_list <-
      #       conn |>
      #       tbl(rules_table) |>
      #       # tbl(input$select_dbtable) |>
      #       count(account_name) |>
      #       collect() |>
      #       pull(account)
      #   }, error = function(err) {
      # 
      #     msg <- "Database Connection Error"
      #     # print `msg` so that we can find it in the logs
      #     print(msg)
      #     # print the actual error to log it
      #     print(error)
      #     # show error `msg` to user.  User can then tell us about error and we can
      #     # quickly identify where it cam from based on the value in `msg`
      #     showToast("error", msg)
      #   })
      # 
      #   accounts_list
      # })
      
      # observeEvent(accounts(), {
      #   updateSelectInput(session, "select_accounts", choices = accounts())
      # })
      
      # Read in selected "rules" table from the database
      rules <- reactive({
        session$userData$rules_trigger()
        
        out <- NULL
        tryCatch({
          out <-
            conn |>
            tbl(rules_table) |>
            # tbl(input$select_dbtable) |>
            # filter(account %in% c(input$select_accounts)) |>
            collect() |>
            mutate(
              created_at = as.POSIXct(created_at, tz = "UTC"),
              modified_at = as.POSIXct(modified_at, tz = "UTC")
            ) |>
            arrange(desc(modified_at))
        }, error = function(err) {
          
          msg <- "Database Connection Error"
          # print `msg` so that we can find it in the logs
          print(msg)
          # print the actual error to log it
          print(error)
          # show error `msg` to user.  User can then tell us about error and we can
          # quickly identify where it cam from based on the value in `msg`
          showToast("error", msg)
        })
        
        out
      })
      
      
      # # Create a reactive variable to store currently selected rule ID
      # rule_uid_to_edit <- reactiveVal(NULL)
      #
      # # Make the uid reactive to the the selected key
      # observeEvent(input$select_key, {
      #   rules() |>
      #     filter(key == input$select_key) |>
      #     pull(uid) |>
      #     rule_uid_to_edit()
      # })
      
      # Make the (regex) rule reactive to the the selected key
      # OR any updates to the 'rules' db
      rule_regex <- eventReactive({
        input$select_key
        rules()
      },
      {
        rules() |>
          filter(key == input$select_key) |>
          pull(rule)
      })
      
      
      # Populate the key selection choices based on the db
      observeEvent(rules(), {
        updateSelectInput(session, "select_key", choices = rules()$key)
      })
      
      # Print the (regex) rule label for the selected key
      output$label_rule <- renderText(paste0("Rule: /", rule_regex(), "/"))
      
      
      # Tx data processing ####
      # Make the raw txs df reactive to the uploaded csv
      txs_filepath <- eventReactive(input$file_txs, {
        inFile <- input$file_txs
        if (is.null(inFile))
          return(NULL)
        inFile$datapath
        # df <- read.csv(inFile$datapath, header = TRUE)
        # df
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Apply selected preproc fun
      txs_preproc <- reactive({
        if (input$select_preprocfun == "(none)") {
          preproc$default(txs_filepath())
        } else {
          f <- get(input$select_preprocfun)
          do.call(f, list(x = txs_filepath()), envir = preproc)
        }
      })
      
      # Populate the weight selection choices based on the tx data
      # as well as the description field choices
      observeEvent(txs_preproc(), {
        description_choices <-
          txs_preproc() |>
          select(where(is.character)) |>
          names()
        updateSelectInput(session, "select_description", choices = description_choices)
        weight_choices <-
          txs_preproc() |>
          select(where(is.numeric)) |>
          names()
        updateSelectInput(session, "select_weight", choices = c("(Unweighted)", weight_choices))
      })
      
      # Dedup the txs by the description field if checked
      txs_dedup <- reactive({
        if (input$check_dedup) {
          txs_preproc() |>
            group_by(.data[[input$select_description]]) |>
            summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |>
            ungroup()
        } else {
          txs_preproc()
        }
      })
      
      
      
      # Overall diagnostics ----
      # For each description, compute the number (0, 1, ...) of regex rules that match it
      txs_rulecheck <- reactive({
        x <-
          txs_dedup() |>
          # Add a unique id to prevent unintended deduplication
          # (if user has not checked the box to combine identical descriptions)
          mutate(
            .id = 1:n()
            # .wt =	ifelse(input$select_weight == "(Unweighted)", 1, .data[[input$select_weight]])
          )
        
        if (input$select_weight == "(Unweighted)") {
          x$.wt <- 1.0
        } else {
          x$.wt <- x[[input$select_weight]]
        }
        
        x |>
          # distinct(.data[[input$select_description]]) |>
          fuzzyjoin::regex_left_join(
            rules() |>
              select(.key = key, .rule = rule),
            by = setNames(".rule", input$select_description),
            ignore_case = TRUE
          ) |>
          group_by(.id, .wt, .data[[input$select_description]]) |>
          summarize(
            # compute the number of matching keys (zero or more) for each tx
            .matches = sum(purrr::map_int(.key, \(x) ifelse(is.na(x), 0L, 1L))),
            # also retain the first key matched, if any
            .key = first(.key)
          ) |>
          ungroup() |>
          mutate(
            .key = ifelse(is.na(.key), "NONE", .key)
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
          count(.key, wt = .wt) |>
          ggplot(aes(area = n, fill = n, label = .key)) +
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
      
      
      # Currently Matched Tx Diagnostics ----
      # Make 'matched' tx df based on selected rule
      # Count is conditional on choice of weighted or unweighted
      current_txs_matched <- reactive({
        x <-
          txs_dedup() |>
          filter(grepl(rule_regex(), .data[[input$select_description]], ignore.case = TRUE))
        # glimpse(x)
        # print(x[[input$select_weight]])
        # print(str(x[[input$select_weight]]))
        
        # x$.wt <- ifelse(input$select_weight == "(Unweighted)", rep(1.0, nrow(x)), x[[input$select_weight]])
        if (input$select_weight == "(Unweighted)") {
          x$.wt <- 1.0
        } else {
          x$.wt <- x[[input$select_weight]]
        }
        # glimpse(x)
        x <-
          x |>
          # mutate(
          #   .wt =	ifelse(input$select_weight == "(Unweighted)", 1, .data[[input$select_weight]])
          # ) |>
          count(.data[[input$select_description]], wt = .wt, sort = TRUE) |>
          mutate(pct = n / overall_txs())
        # glimpse(x)
        x
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
      
      
      # Unmatched Tx Diagnostics ----
      # txs_unmatched <- reactive({
      #   txs_dedup() |>
      #     filter(!grepl(rule_regex(), .data[[input$select_description]], ignore.case = TRUE)) |>
      #     mutate(
      #       .wt =	ifelse(input$select_weight == "(Unweighted)", 1, .data[[input$select_weight]])
      #     ) |>
      #     count(.data[[input$select_description]], wt = .wt, sort = TRUE) |>
      #     mutate(pct = n / sum(n, na.rm = TRUE))
      # })
      output$txs_unmatched <- renderDataTable(
        txs_rulecheck() |>
          filter(.matches == 0) |>
          count(.data[[input$select_description]], wt = .wt, sort = TRUE) |>
          mutate(pct = n / overall_txs()) |>
          datatable(
            # rownames = FALSE,
            colnames = c("Description", "Count", '%'),
            selection = "none",
            class = "compact stripe row-border",
          ) |>
          formatRound("n", digits = 0) |>
          formatPercentage("pct", digits = 0)
      )
      
      
      # Modal Modules ####
      callModule(
        rule_edit_module,
        "add_rule",
        modal_title = "Add Rule",
        rule_to_edit = function() NULL,
        modal_trigger = reactive({input$cmd_add_rule})
      )
      
      rule_to_edit <- reactive({
        rules() |>
          filter(key == input$select_rule_name)
      })
      
      callModule(
        rule_edit_module,
        "edit_rule",
        modal_title = "Edit Rule",
        rule_to_edit = rule_to_edit,
        modal_trigger = reactive({input$cmd_edit_rule})
      )
      
      rule_to_delete <- reactive({
        rules() |>
          filter(key == input$select_rule_name) |>
          as.list()
      })
      
      # rule_to_delete <- eventReactive(input$rule_id_to_delete, {
      #   rules() |>
      #     filter(uid == input$rule_id_to_delete) |>
      #     as.list()
      # })
      
      callModule(
        rule_delete_module,
        "delete_rule",
        modal_title = "Delete Rule",
        rule_to_delete = rule_to_delete,
        modal_trigger = reactive({input$cmd_delete_rule})
      )
      
      
    }
  )
  
}
