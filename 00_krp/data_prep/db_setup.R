library(RSQLite)
library(tidyverse)
library(googlesheets4)

# gs4_deauth()
gs4_auth(email = "kurt.r.peters@gmail.com")
sheet_db <- "https://docs.google.com/spreadsheets/d/121x9k9CIn8WMFK8TqCTKhIgliE5Qvk8_5rc2peQZVt0/edit#gid=911520687"


# Read individual sheets into a list of dfs
tbl_names <- sheet_names(sheet_db)

# Only read in dim tables from the gsheet
# The gsheet is the source of truth for dim tables
# But any fct tables are just for example purposes; the actual fct table schemas
# will be created below and appended to over time (as rules & txs accumulate)
tbl_names
dim_tbl_names <- tbl_names[grepl("^dim_", tbl_names)]
dim_tbl_names

dim_tables <- 
  dim_tbl_names |> 
  map(\(x) read_sheet(sheet_db, x)) |> 
  set_names(dim_tbl_names)
str(dim_tables, max.level = 1)

# Create a connection object with local SQLite db
conn <- 
  dbConnect(
    RSQLite::SQLite(),
    ":memory:" #testing
    # "00_krp/shiny_app/data/pei_budget.sqlite3" #prod
  )
dbplyr::src_memdb()
dbListTables(conn)

# Load all the sheets as db tables with same names
# https://stackoverflow.com/questions/65004814/create-table-from-sql-statement-using-dplyr
db_dim_tables <- 
  dim_tables |> 
  imap(\(x, idx) copy_to(conn, x, name = idx, overwrite = TRUE))
dbListTables(conn)
db_dim_tables

# conn |> 
#   tbl("dim_account") |>
#   collect()


dbDisconnect(conn)
