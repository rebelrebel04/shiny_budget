conn <- dbConnect(
  RSQLite::SQLite(),
  "00_krp/shiny_app/data/pei_budget.sqlite3"
  #"01_traditional/shiny_app/data/mtcars.sqlite3"
)

#out <-
conn |>
  tbl("rules") |>
  # tbl(input$select_dbtable) |>
  # filter(account %in% c(input$select_accounts)) |>
  count(category, sort = TRUE) |> 
  collect()
  # mutate(
  #   created_at = as.POSIXct(created_at, tz = "UTC"),
  #   modified_at = as.POSIXct(modified_at, tz = "UTC")
  # ) |>
  # arrange(desc(modified_at))

dbDisconnect(conn)
