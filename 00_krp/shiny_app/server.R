function(input, output, session) {

  # Use session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$email <- 'kurt.r.peters@gmail.com'

  # Call the server function portion of the module file
  rules_table_server("rules_table")
  
}
