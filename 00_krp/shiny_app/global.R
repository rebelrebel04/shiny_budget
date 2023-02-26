# Library in packages used in this application
library(shiny)
library(flexdashboard)
library(DT)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(shinyFeedback)
library(dplyr)
library(glue)
library(readr)
library(tidyr)
library(dbplyr)
library(ggplot2)

db_config <- config::get()$db

# Create database connection
conn <- dbConnect(
  RSQLite::SQLite(),
  dbname = db_config$dbname
)

preproc <- new.env()
source(config::get()$preproc_funs, local = preproc)

# Stop database connection when application stops
shiny::onStop(function() {
  dbDisconnect(conn)
})

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)
