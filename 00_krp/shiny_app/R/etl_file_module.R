# Module UI function
etl_file_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("file_etl"),
      label = "Upload transaction data (csv): ",
      accept = ".csv"
    ),
    selectInput(
      ns("select_preprocfun"),
      label = "Preprocessing function: ",
      choices = c("none", lsf.str(envir = preproc)),
      selected = "none"
    ),
    actionButton(
      ns("cmd_etl_save"),
      "Save",
      class = "btn btn-success apply_btn",
      style = "color: #fff;",
      icon = icon('floppy-disk')
    )
  )
}


# Module server function
etl_file_server <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # Make the raw txs df reactive to the uploaded csv      
      etl_file <- reactive({
        validate(need(input$file_etl, message = "A csv file is required"))
        input$file_etl
      })
      
      # Apply selected preproc fun: parse uploaded csv into a data frame
      etl_preproc <- reactive({
        if (input$select_preprocfun == "none") {
          preproc$default(etl_file()$datapath)
        } else {
          f <- get(input$select_preprocfun)
          do.call(f, list(x = etl_file()$datapath), envir = preproc)
        }
      })
      
      # We can run observers in here if we want to
      observeEvent(etl_file(), {
        cli::cli_alert_success("Uploaded: {etl_file()$name}")
      })
      
      # Observe cmd_etl_save: write uploaded data to fct_transactions
      observeEvent(input$cmd_etl_save, {
        rows_to_append <- 
          etl_preproc() |> 
          mutate(
            created_by = session$userData$email,
            modified_by = session$userData$email
          )
        # glimpse(rows_to_append)
        res <- NULL
        tryCatch({
          res <- dbAppendTable(
            conn, 
            name = "fct_transactions", 
            value = rows_to_append
          )
        }, error = function(err) {
          msg <- "Database Connection Error"
          print(msg)
          print(err)
          showToast("error", msg)
        })
        showToast("success", glue("Appended {nrow(rows_to_append)} rows to fct_transactions"))
        res
      })

            
      # Return the reactive that yields the data frame
      # for rendering in the DT output
      return(etl_preproc)
    }
  )   
}