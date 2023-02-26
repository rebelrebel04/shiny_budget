# Module UI function
etl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("file_etl"),
          label = "Upload transaction data (csv): ",
          accept = ".csv"
        ),
        selectInput(
          ns("select_preprocfun"),
          label = "Preprocessing function: ",
          choices = lsf.str(envir = preproc),
          selected = "none"
        ),
        actionButton(
          ns("cmd_etl_save"),
          "Save",
          class = "btn btn-success apply_btn",
          style = "color: #fff;",
          icon = icon('floppy-disk')
        )
      ), #end sidebarPanel
      
      mainPanel(
        dataTableOutput("etl_table")
      )
    ) #end sidebarLayout    
  ) #end tagList
}


# Module server function
etl_server <- function(id) {
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
          preproc$none(etl_file()$datapath)
        } else {
          f <- get(input$select_preprocfun)
          do.call(f, list(csv_file = etl_file()$datapath), envir = preproc)
        }
      })
      
      # We can run observers in here if we want to
      observeEvent(etl_file(), {
        cli::cli_alert_success("Uploaded: {etl_file()$name}")
      })
      
      # Observe cmd_etl_save: write uploaded data to fct_transactions
      # - IF there is a valid preproc fun selected
      # - AND there are no non-unique warnings
      dup_txs <- reactiveVal()
      observeEvent(input$cmd_etl_save, {
        res <- NULL
        toast_status <- NULL
        if (input$select_preprocfun == "none") {
          # If no valid preproc fun is selected, warn user & do not append
          toast_status <- list(
            type = "warning",            
            msg = "Please select a valid preprocessing function before saving"
          )
        } else {
          # We have a valid preproc fun selected
          # Now confirm that there are no duplicated records
          # in fct_transactions for this account_nickname (over all history)
          tryCatch({
            # Grab the existing data in db for this account
            res <- 
              conn |> 
              tbl("fct_transactions") |> 
              # filter(account_nickname == input$select_preprocfun) |> 
              select(transaction_id) |> 
              collect()
            # res <- dbGetQuery(
            #   conn,
            #   glue("select * from fct_transactions where account_nickname = {input$select_preprocfun})")
            # )
          }, error = function(err) {
            print(err)
            showToast("error", "Error querying records from database")
          })
          
          # Check for an intersection between tx_ids
          dups <- intersect(res$transaction_id, etl_preproc()$transaction_id)

          if (length(dups) > 0) {
            # Do not append any records to db if any dups are detected
            # instead, warn user & display the transaction_ids in the DT
            
            # Add the dup txs as a tibble to the reactive variable
            # for rendering in the DT (will override display of etl_preproc df)
            dup_txs(tibble(dup_tx_ids = dups))
            toast_status <- list(
              type = "warning",            
              msg = glue("{length(dups)} non-unique records detected; see results table")
            )
            
          } else {
            # Otherwise both checks have passed: try to append
            rows_to_append <- 
              etl_preproc() |> 
              mutate(
                created_by = session$userData$email,
                modified_by = session$userData$email
              )
            # glimpse(rows_to_append)
            tryCatch({
              res <- dbAppendTable(
                conn, 
                name = "fct_transactions", 
                value = rows_to_append
              )
              toast_status <- list(
                type = "success",              
                msg = glue("Appended {nrow(rows_to_append)} rows to fct_transactions")
              )
            }, error = function(err) {
              print(err)
              showToast("error", "Error appending records to database")
            })
          }
        }
         
        # Display the final toast message based on the conditions above 
        showToast(toast_status$type, toast_status$msg)
      })

            
      # Return the reactive that yields the data frame
      # for rendering in the DT output
      return(list(
        etl_preproc = etl_preproc,
        dup_txs = dup_txs
      ))
    }
  )   
}