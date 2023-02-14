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
        tags$div(HTML("<i>Upload transaction data as csv</i>")),
        fileInput(
          ns("file_txs"),
          label = "csv file: ",
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
            6,
            titlePanel("Matched"),
            tableOutput(ns("txs_matched"))
          ),
          column(
            6,
            titlePanel("Unmatched"),
            tableOutput(ns("txs_unmatched"))
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

  # debugging: print uid of selected rule
  # output$label_uid <- renderText({
  # 	rule_uid_to_edit()
  # })


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

  # Print the matched & unmatched txs for selected rule
  output$txs_matched <- renderTable(txs_df_matched(), striped = TRUE, hover = TRUE)
  output$txs_unmatched <- renderTable(txs_df_unmatched(), striped = TRUE, hover = TRUE)


  # rules_table_prep <- reactiveVal(NULL)
  #
  # observeEvent(rules(), {
  #   out <- rules()
  #
  #   ids <- out$uid
  #
  #   actions <- purrr::map_chr(ids, function(id_) {
  #     paste0(
  #       '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
  #         <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
  #         <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
  #       </div>'
  #     )
  #   })
  #
  #   # Remove the `uid` column. We don't want to show this column to the user
  #   out <-
  #     out %>%
  #     select(-uid)
  #
  #   # Set the Action Buttons row to the first column of the `rules` table
  #   out <-
  #     cbind(
  #       tibble(" " = actions),
  #       out
  #     )
  #
  #   if (is.null(rules_table_prep())) {
  #     # loading data into the table for the first time, so we render the entire table
  #     # rather than using a DT proxy
  #     rules_table_prep(out)
  #
  #   } else {
  #
  #     # table has already rendered, so use DT proxy to update the data in the
  #     # table without rerendering the entire table
  #     replaceData(rules_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
  #
  #   }
  # })
  #
  # output$rules_table <- renderDT({
  #   req(rules_table_prep())
  #   out <- rules_table_prep()
  #
  #   datatable(
  #     out,
  #     rownames = FALSE,
  #     colnames = c("key", "rule", 'Created At', 'Created By', 'Modified At', 'Modified By'),
  #     # colnames = c('Model', 'Miles/Gallon', 'Cylinders', 'Displacement (cu.in.)',
  #     #              'Horsepower', 'Rear Axle Ratio', 'Weight (lbs)', '1/4 Mile Time',
  #     #              'Engine', 'Transmission', 'Forward Gears', 'Carburetors', 'Created At',
  #     #              'Created By', 'Modified At', 'Modified By'),
  #     selection = "none",
  #     class = "compact stripe row-border nowrap",
  #     # Escape the HTML in all except 1st column (which has the buttons)
  #     escape = -1,
  #     extensions = c("Buttons"),
  #     options = list(
  #       scrollX = TRUE,
  #       dom = 'Bftip',
  #       buttons = list(
  #         list(
  #           extend = "excel",
  #           text = "Download",
  #           title = paste0("rules-", Sys.Date()),
  #           exportOptions = list(
  #             columns = 1:(length(out) - 1)
  #           )
  #         )
  #       ),
  #       columnDefs = list(
  #         list(targets = 0, orderable = FALSE)
  #       ),
  #       drawCallback = JS("function(settings) {
  #         // removes any lingering tooltips
  #         $('.tooltip').remove()
  #       }")
  #     )
  #   ) %>%
  #     formatDate(
  #       columns = c("created_at", "modified_at"),
  #       method = 'toLocaleString'
  #     )
  #
  # })
  #
  # rules_table_proxy <- DT::dataTableProxy('rules_table')

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
