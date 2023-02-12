library(RSQLite)
library(tibble)

# Create a connection object with SQLite
conn <- dbConnect(
  RSQLite::SQLite(),
  "00_krp/shiny_app/data/pei_budget.sqlite3"
  #"01_traditional/shiny_app/data/mtcars.sqlite3"
)

# Create a query to prepare the 'mtcars' table with additional 'uid', 'id',
# & the 4 created/modified columns
create_rules_query = "CREATE TABLE rules (
  uid                             TEXT PRIMARY KEY,
  key                             TEXT,
  rule                            TEXT,
  created_at                      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by                      TEXT,
  modified_at                     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_by                     TEXT
)"

# dbExecute() executes a SQL statement with a connection object
# Drop the table if it already exists
dbExecute(conn, "DROP TABLE IF EXISTS rules")
# Execute the query created above
dbExecute(conn, create_rules_query)

# Read in the RDS file created in 'data_prep.R'
dat <- readRDS("00_krp/data_prep/data/regex_tester/rules.RDS")

# add uid column to the `dat` data frame
dat$uid <- uuid::UUIDgenerate(n = nrow(dat))

# reorder the columns
dat <- 
  dat %>%
  select(uid, everything())

# Fill in the SQLite table with the values from the RDS file
DBI::dbWriteTable(
  conn,
  name = "rules",
  value = dat,
  overwrite = FALSE,
  append = TRUE
)

# List tables to confirm 'mtcars' table exists
dbListTables(conn)

# disconnect from SQLite before continuing
dbDisconnect(conn)
