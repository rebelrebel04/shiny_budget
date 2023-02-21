db_config <- config::get()$db

conn <- dbConnect(
	RSQLite::SQLite(),
	dbname = db_config$dbname
)

out <-
	conn |>
	tbl(rules_table) |>
	# tbl(input$select_dbtable) |>
	filter(account %in% c(input$select_accounts)) |>
	collect() |>
	mutate(
		created_at = as.POSIXct(created_at, tz = "UTC"),
		modified_at = as.POSIXct(modified_at, tz = "UTC")
	) |>
	arrange(desc(modified_at))
