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
        actionButton(
          ns("add_rule"),
          "Add",
          class = "btn-success",
          style = "color: #fff;",
          icon = icon('plus'),
          width = '100%'
        ),
        selectInput(
          ns("select_key"),
          label = "Key: ",
          choices = NULL
        ),
        textOutput(
          ns("label_rule")
        ),
        textOutput(
        	ns("label_uid")
        ),
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
        DTOutput(ns('rules_table')) %>%
          withSpinner()
        # tags$br(),
        # tags$br()
      )
    ),
    tags$script(src = "rules_table_module.js"),
    tags$script(paste0("rules_table_module_js('", ns(''), "')"))
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

  #///how to set the 'id' of the button to the uid of the selected
  #   key, so the modal knows which key to edit/delete?
  # input$rule_id_to_edit <- eventReactive(input$select_key, {
  #   rules() %>%
  #     filter(key == input$select_key) %>%
  #     pull(uid)
  # })
  observeEvent(input$select_key, {

  	rules() %>%
  		filter(key == input$select_key) %>%
  		pull(uid) %>%
  		rule_uid_to_edit()

    # updateActionButton(
    #   session,
    #   inputId = "cmd_edit_rule",
    #   label = rules() %>%
    #     filter(key == input$select_key) %>%
    #     pull(uid)
    # )
  })



  # Populate the key selection choices based on the db
  observeEvent(rules(), {
    updateSelectInput(session, "select_key", choices = rules()$key)
  })

  # Populate the regex rule label for the selected key
  output$label_rule <-
    renderText({
      rules() %>%
        filter(key == input$select_key) %>%
        pull(rule)
        # pull(uid)
    })

  output$label_uid <- renderText({
  	rule_uid_to_edit()
  })

  # # Update the rule regex in the rules_df when clicking Update Rule button
  # observeEvent(input$cmd_edit_rule, {
  #   inFile <- input$file_rules
  #   if (is.null(inFile))
  #     return(NULL)
  #   # Update the regex for selected rule
  #   read.csv(inFile$datapath, header = TRUE) %>%
  #     mutate(
  #       rule = case_when(
  #         key == input$select_rule ~ input$rule_text,
  #         TRUE ~ rule
  #       )
  #     ) %>%
  #     write.csv(inFile$datapath)
  # })


  rules_table_prep <- reactiveVal(NULL)

  observeEvent(rules(), {
    out <- rules()

    ids <- out$uid

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })

    # Remove the `uid` column. We don't want to show this column to the user
    out <-
      out %>%
      select(-uid)

    # Set the Action Buttons row to the first column of the `rules` table
    out <-
      cbind(
        tibble(" " = actions),
        out
      )

    if (is.null(rules_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      rules_table_prep(out)

    } else {

      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(rules_table_proxy, out, resetPaging = FALSE, rownames = FALSE)

    }
  })

  output$rules_table <- renderDT({
    req(rules_table_prep())
    out <- rules_table_prep()

    datatable(
      out,
      rownames = FALSE,
      colnames = c("key", "rule", 'Created At', 'Created By', 'Modified At', 'Modified By'),
      # colnames = c('Model', 'Miles/Gallon', 'Cylinders', 'Displacement (cu.in.)',
      #              'Horsepower', 'Rear Axle Ratio', 'Weight (lbs)', '1/4 Mile Time',
      #              'Engine', 'Transmission', 'Forward Gears', 'Carburetors', 'Created At',
      #              'Created By', 'Modified At', 'Modified By'),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("Buttons"),
      options = list(
        scrollX = TRUE,
        dom = 'Bftip',
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("rules-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    ) %>%
      formatDate(
        columns = c("created_at", "modified_at"),
        method = 'toLocaleString'
      )

  })

  rules_table_proxy <- DT::dataTableProxy('rules_table')

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

  # rule_to_edit <- eventReactive(input$rule_id_to_edit, {
  #   rules() %>%
  #     filter(uid == input$rule_id_to_edit)
  # })

  callModule(
    rule_edit_module,
    "edit_rule",
    modal_title = "Edit Rule",
    # rule_to_edit = rule_uid_to_edit,
    rule_to_edit = rule_to_edit,
    #modal_trigger = rule_to_edit
    modal_trigger = reactive({input$cmd_edit_rule})
    #modal_trigger = reactive({input$cmd_rule_edit})
    # modal_trigger = reactive({input$rule_id_to_edit})
  )

  rule_to_delete <- eventReactive(input$rule_id_to_delete, {

    rules() %>%
      filter(uid == input$rule_id_to_delete) %>%
      as.list()
  })

  callModule(
    rule_delete_module,
    "delete_rule",
    modal_title = "Delete Rule",
    rule_to_delete = rule_to_delete,
    modal_trigger = reactive({input$rule_id_to_delete})
  )

}
