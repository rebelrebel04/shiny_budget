function(input, output, session) {

  # Use session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$email <- 'kurt.r.peters@gmail.com'

  # Call the server function portion of the module file
  # rules_table_server("rules_table")
  
  
  # So: a server function doesn't need to be called directly as above
  # instead it should be treated like a gigantic reactive variable
  # that returns output that can render an output (or update an input)
  
  # example of just rendering an output directly
  output$text <- renderPrint("hi there")
  
  # example of rendering an output as the return of a server module
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  output$table <- renderDataTable({
    datafile()
  })
  
}
