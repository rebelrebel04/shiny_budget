function(input, output, session) {

  # Use session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$email <- 'kurt.r.peters@gmail.com'

  # So: a server function doesn't need to be called directly as above
  # instead it should be treated like a gigantic reactive variable
  # that returns output that can render an output (or update an input)
  
  # example of just rendering an output directly
  # output$text <- renderPrint("hi there")
  
  # ETL ####
  # The return from the etl_server module is a list of reactives
  etl_server_res <- etl_server("etl") #, stringsAsFactors = FALSE)
  # Render the loaded csv into the DT for review
  output$etl_table <- renderDataTable({
    if (!is.null(etl_server_res$dup_txs())) 
      etl_server_res$dup_txs()
    else 
      etl_server_res$etl_preproc()
  })
  

  # CATEGORIZE ####
  categorize_server_res <- categorize_server("categorize")
  # output$txs_matched <- renderDataTable({
  #   categorize_server_res$txs_dedup()
  # })
  
  
  # ANALYZE ####
  analyze_server_res <- analyze_server("analyze")  
  
}
