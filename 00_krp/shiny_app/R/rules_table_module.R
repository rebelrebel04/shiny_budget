#' Rules Table Module UI
#'
#' The UI portion of the module for displaying the rules datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
rules_table_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel("Rules Data"),
        tags$div(HTML("<i>Connected to 'rules' database</i>")),
        selectInput(
          ns("select_key"),
          label = "Select a key to categorize transactions: ",
          choices = NULL
        ),
        textOutput(
          ns("label_rule")
        ),
        # textOutput(
        # 	ns("label_uid")
        # ),
        tags$br(),
        actionButton(
          ns("cmd_edit_rule"),
          "Edit",
          class = "btn btn-primary btn-sm edit_btn",
          style = "color: #fff;",
          icon = icon('pencil')
          # id = 1
        ),
        actionButton(
          ns("cmd_delete_rule"),
          "Delete",
          class = "btn btn-danger btn-sm delete_btn",
          style = "color: #fff;",
          icon = icon('trash')
        ),
        actionButton(
          ns("add_rule"),
          "New",
          class = "btn btn-sm btn-success",
          style = "color: #fff;",
          icon = icon('plus')
          # width = '100%'
        ),
        tags$br(),
        tags$br(),

        titlePanel("Transaction Data"),
        # tags$div(HTML("<i>Upload transaction data as csv</i>")),
        fileInput(
          ns("file_txs"),
          label = "Upload transaction data as csv: ",
          accept = ".csv"
        ),
        selectInput(
        	ns("select_weight"),
        	label = "Weight: ",
        	choices = "(Unweighted)"
        )
        # tags$div(
        #   class="btn-group", style="width: 75px;", role="group", 'aria-label'="Basic example",
        #   tags$button(class="btn btn-primary btn-sm edit_btn", 'data-toggle'="tooltip", 'data-placement'="top", title="Edit", id = "4127d505-ea58-440b-b3e7-968ea77c2612", style="margin: 0", tags$i(class="fa fa-pencil-square-o")),
        #   tags$button(class="btn btn-danger btn-sm delete_btn", 'data-toggle'="tooltip", 'data-placement'="top", title="Delete", id = "4127d505-ea58-440b-b3e7-968ea77c2612", style="margin: 0", tags$i(class="fa fa-trash-o"))
        # )
        # tags$br(),
        # tags$br()
      ),

      mainPanel(
      	fluidRow(
      		column(
      			12,
      			flexdashboard::gaugeOutput(ns("gauge_matched_total")),
      			textOutput(ns("text_matched_total")),
      			textOutput(ns("text_txs_total")),
      			tags$div(HTML("<i>% of unique Descriptions with at least 1 matching rule</i>")),
      		)
      	),
        fluidRow(
          column(
            6,
            titlePanel("Matched"),
            DTOutput(ns("txs_matched"))
            # tableOutput(ns("txs_matched"))
          ),
          column(
            6,
            titlePanel("Unmatched"),
            DTOutput(ns("txs_unmatched"))
          )
        )
        # DTOutput(ns('rules_table')) %>%
        #   withSpinner()
        # tags$br(),
        # tags$br()
      )
    )
    # tags$script(src = "rules_table_module.js"),
    # tags$script(paste0("rules_table_module_js('", ns(''), "')"))
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

rules_table_module <- function(input, output, session) {

  # trigger to reload data from the "rules" table
  session$userData$rules_trigger <- reactiveVal(0)

  # Read in "rules" table from the database
  rules <- reactive({
    session$userData$rules_trigger()

    out <- NULL
    tryCatch({
      out <-
        conn %>%
        tbl('rules') %>%
        collect() %>%
        mutate(
          created_at = as.POSIXct(created_at, tz = "UTC"),
          modified_at = as.POSIXct(modified_at, tz = "UTC")
        ) %>%
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


  # Create a reactive variable to store currently selected rule ID
  rule_uid_to_edit <- reactiveVal(NULL)

  # Make the uid reactive to the the selected key
  observeEvent(input$select_key, {
  	rules() %>%
  		filter(key == input$select_key) %>%
  		pull(uid) %>%
  		rule_uid_to_edit()
  })

  # Make the (regex) rule reactive to the the selected key
  # OR any updates to the 'rules' db
  rule_regex <- eventReactive({
    input$select_key
    rules()
  },
  {
    rules() %>%
      filter(key == input$select_key) %>%
      pull(rule)
  })


  # Populate the key selection choices based on the db
  observeEvent(rules(), {
    updateSelectInput(session, "select_key", choices = rules()$key)
  })

  # Print the (regex) rule label for the selected key
  output$label_rule <- renderText(paste0("Rule: /", rule_regex(), "/"))


  # Make the "txs" df reactive to the uploaded csv
  txs_df <- eventReactive(input$file_txs, {
    inFile <- input$file_txs
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE)
    #print(df)
    df
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Populate the weight selection choices based on the tx data
  observeEvent(txs_df(), {
  	choices <-
  		txs_df() %>%
  		select(where(is.numeric)) %>%
  		names()
  	updateSelectInput(session, "select_weight", choices = c("(Unweighted)", choices))
  })

  # Make 'matched' and 'unmatched' tx dfs based on changes to
  # currently selected key OR 'rules' db
  txs_df_matched <- reactive({
  	txs_df() %>%
  		filter(grepl(rule_regex(), Description, ignore.case = TRUE)) %>%
  		mutate(
  			.wt =	ifelse(input$select_weight == "(Unweighted)", 1, .data[[input$select_weight]])
  		) %>%
  		count(Description, wt = .wt, sort = TRUE) %>%
  		mutate(pct = n / sum(n, na.rm = TRUE))
  })

  txs_df_unmatched <- reactive({
  	txs_df() %>%
  		filter(!grepl(rule_regex(), Description, ignore.case = TRUE)) %>%
  		mutate(
  			.wt =	ifelse(input$select_weight == "(Unweighted)", 1, .data[[input$select_weight]])
  		) %>%
  		count(Description, wt = .wt, sort = TRUE) %>%
  		mutate(pct = n / sum(n, na.rm = TRUE))
  })


  # Overall diagnostics ----
  # For each unique Description, compute the number (0, 1, ...) of regex rules that match it
  txs_df_rulecheck <- reactive({
  	txs_df() %>%
  		# mutate(
  		# 	.wt =	ifelse(input$select_weight == "(Unweighted)", 1, .data[[input$select_weight]])
  		# ) %>%
  	 distinct(Description) %>%
  		group_by()
  		fuzzyjoin::regex_left_join(
  			rules() %>%
  				select(.key = key, .rule = rule),
  			by = c(Description = ".rule"),
  			ignore_case = TRUE
  		) %>%
  		group_by(Description) %>%
  		summarize(
  			.matches = sum(purrr::map_int(.key, ~ ifelse(is.na(.x), 0L, 1L)))
  		)
  })

  # Update the gauge widget to show total rule coverage
  output$gauge_matched_total <- flexdashboard::renderGauge({
  	flexdashboard::gauge(
  		#n_txs_matched() / (n_txs_matched() + n_txs_unmatched()),
  		nrow(filter(txs_df_rulecheck(), .matches > 0)) / nrow(txs_df_rulecheck()),
  		min = 0,
  		max = 1,
  		sectors = flexdashboard::gaugeSectors(
  			success = c(0.8, 1),
  			warning = c(0.3, 0.8),
  			danger = c(0, 0.3)
  		)
  	)
  })

  # Update the text outputs to print number of matched Descriptions out of total unique
  output$text_matched_total <- renderText({
  	paste0("Descriptions with a matched rule: ", nrow(filter(txs_df_rulecheck(), .matches > 0)))
  })
  output$text_txs_total <- renderText({
  	paste0("Total unique descriptions in transactions data: ", nrow(txs_df_rulecheck()))
  })


  # Matched Tx Diagnostics ----



  # Unmatched Tx Diagnostics ----


  # DataTable outputs ----
  # Print the matched & unmatched txs for selected rule
  output$txs_matched <- renderDataTable(
  	txs_df_matched() %>%
  		datatable(
  			# rownames = FALSE,
  			colnames = c("Description", "Count", '%'),
  			selection = "none",
  			class = "compact stripe row-border",
  		) %>%
  		formatRound("n", digits = 0) %>%
  		formatPercentage("pct", digits = 0)
  )
  output$txs_unmatched <- renderDataTable(
  	txs_df_unmatched() %>%
  		datatable(
  			# rownames = FALSE,
  			colnames = c("Description", "Count", '%'),
  			selection = "none",
  			class = "compact stripe row-border",
  		) %>%
  		formatRound("n", digits = 0) %>%
  		formatPercentage("pct", digits = 0)
  )


  # Modal Modules ----
  callModule(
    rule_edit_module,
    "add_rule",
    modal_title = "Add Rule",
    rule_to_edit = function() NULL,
    modal_trigger = reactive({input$add_rule})
  )

  rule_to_edit <- reactive({
  	rules() %>%
  		filter(key == input$select_key)
  })

  callModule(
    rule_edit_module,
    "edit_rule",
    modal_title = "Edit Rule",
    rule_to_edit = rule_to_edit,
    modal_trigger = reactive({input$cmd_edit_rule})
  )

  rule_to_delete <- reactive({
  	rules() %>%
  		filter(key == input$select_key) %>%
      as.list()
  })

  # rule_to_delete <- eventReactive(input$rule_id_to_delete, {
  #   rules() %>%
  #     filter(uid == input$rule_id_to_delete) %>%
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
