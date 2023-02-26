library(RSQLite)
library(tidyverse)
library(glue)
library(googlesheets4)

# DB CONNECTION ####
# Create a connection object with local SQLite db
conn <- 
  dbConnect(
    RSQLite::SQLite(),
    # ":memory:" #testing
    "00_krp/shiny_app/data/pei_budget_20230225.sqlite3" #prod
  )
# dbplyr::src_memdb()
dbListTables(conn)


# READ GSHEET ####
# gs4_deauth()
gs4_auth(email = "kurt.r.peters@gmail.com")
sheet_db <- "https://docs.google.com/spreadsheets/d/121x9k9CIn8WMFK8TqCTKhIgliE5Qvk8_5rc2peQZVt0/edit#gid=911520687"


# READ TBL SHEETS ####
# Read individual sheets into a list of dfs
tbl_names <- sheet_names(sheet_db)
tbl_names

# Read in dim/fct/schema tables from the gsheet
# The gsheet is the source of truth for dim tables
# and for some of the fct tables
# But any schema_fct tables are just for layout; the actual fct tables
# will be created below and appended to over time (as rules & txs accumulate)
dim_tbl_names <- tbl_names[grepl("^dim_", tbl_names)]
dim_tbl_names
fct_tbl_names <- tbl_names[grepl("^fct_", tbl_names)]
fct_tbl_names
schema_tbl_names <- tbl_names[grepl("^schema_", tbl_names)]
schema_tbl_names
# Drop the "schema_" prefix for these tables
schema_tbl_names <- gsub("^schema_", "", schema_tbl_names)


dim_tables <- 
  dim_tbl_names |> 
  map(\(x) read_sheet(sheet_db, x)) |> 
  set_names(dim_tbl_names)
str(dim_tables, max.level = 1)

fct_tables <- 
  fct_tbl_names |> 
  map(\(x) read_sheet(sheet_db, x)) |> 
  set_names(fct_tbl_names)
str(fct_tables, max.level = 1)

schema_tables <- 
  schema_tbl_names |> 
  map(\(x) read_sheet(sheet_db, paste0("schema_", x))) |> 
  set_names(schema_tbl_names)
str(schema_tables, max.level = 1)


# CREATE DIM & FCT TABLES ####
# https://stackoverflow.com/questions/65004814/create-table-from-sql-statement-using-dplyr
# Load all the dim & fct sheets as db tables with matching names
db_dim_tables <- 
  dim_tables |> 
  imap(\(x, idx) copy_to(conn, x, name = idx, overwrite = TRUE, temporary = FALSE))
dbListTables(conn)
db_dim_tables

db_fct_tables <- 
  fct_tables |> 
  imap(\(x, idx) copy_to(conn, x, name = idx, overwrite = TRUE, temporary = FALSE))
dbListTables(conn)
db_fct_tables


# SCHEMA TABLES ####
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
#               WARNING
# 
# THE BELOW CODE WILL DROP ANY EXISTING
# fct_rules & fct_transactions TABLES
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# CTA these tables from schema definitions, since they will be
# accumulated over time as rules/txs are added to db
schema_cta <- 
  schema_tables |> 
  imap(
    \(x, idx) x |> 
      unite(sql, field, constraint, sep = " ") |> 
      pull(sql) |> 
      paste(collapse = ", ") |> 
      map_chr(\(fields) glue("CREATE TABLE {idx} ( {fields} )"))
  )
schema_cta

db_schema_tables <- 
  schema_cta |> 
  imap(
    \(x, idx) {
      # Drop the table if it already exists
      dbExecute(conn, glue("DROP TABLE IF EXISTS {idx}"))
      # Create table
      dbExecute(conn, x)
    }
  )
db_schema_tables
dbListTables(conn)
collect(tbl(conn, "fct_transactions"))
collect(tbl(conn, "fct_rules"))


# DISCONNECT ####
dbDisconnect(conn)
