
# Create database connection
conn <- dbConnect(
  RSQLite::SQLite(),
  dbname = "00_krp/shiny_app/data/pei_budget_20230322.sqlite3"
)

rules <- 
  conn |> 
  tbl("fct_rules") |> 
  collect() |> 
  select(-matches("created|modified"))
  # filter(account_nickname %in% input$select_account_nickname)

txs <- 
  conn |> 
  tbl("fct_transactions") |> 
  # select(account_nickname, date, description, type, amount) |> 
  collect() |> 
  select(-matches("created|modified"))  
  # filter(account_nickname %in% input$select_account_nickname) |> 
  # filter(date >= input$select_date_range[1] & date <= input$select_date_range[2])

# Join on rule metadata (apply first matching rule)
txs_joined <- 
  txs |> 
  split(~account_nickname) |> 
  imap(
    \(x, nm) fuzzyjoin::regex_left_join(
      x,
      # Filter to only the rules for the account being iterated over
      rules |> filter(account_nickname == nm) |> select(-account_nickname),
      by = c("description" = "rule_regex"),
      ignore_case = TRUE
    ) |> 
      distinct(transaction_id, .keep_all = TRUE)
  ) |> 
  list_rbind()

glimpse(txs_joined)


txs_joined |> 
  count(tx_type, wt = amount, sort = TRUE)

# category totals by month ####
p <- 
  txs_joined |> 
  filter(tx_type == "expense") |> 
  mutate(
    # year = year(as.Date(date)),
    # month = month(as.Date(date)),
    ym = floor_date(as.Date(date), "months")
  ) |> 
  summarize(
    .by = c(ym, category_name),
    amount = sum(amount, na.rm = TRUE)
  ) |> 
  arrange(ym) |> 
  filter(ym >= as.Date("2022-09-01")) |> 
  
  ggplot(aes(x = ym, y = amount, fill = category_name)) +
  # geom_area() +
  geom_bar(stat = "identity") +  
  scale_fill_viridis_d(option = "C")
p
ggplotly(p)
  
  
  ggplot(aes(x = ym, y = amount, color = category_name)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "D")
  
  
#theme_dark()
plotly::ggplotly(p)


txs_joined |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  View()

  
# net income by month ####
p <- 
  txs_joined |> 
  filter(tx_type != "exclude") |> 
  mutate(
    amount = ifelse(tx_type == "expense", -1 * amount, amount),
    # year = year(as.Date(date)),
    # month = month(as.Date(date)),
    ym = floor_date(as.Date(date), "months")
  ) |> 
  summarize(
    .by = c(ym),
    amount = sum(amount, na.rm = TRUE)
  ) |> 
  arrange(ym) |> 
  filter(ym >= as.Date("2022-09-01")) |> 
  
  ggplot(aes(x = ym, y = amount, fill = factor(amount > 0))) +
  # geom_area() +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  theme(legend.position = "none")
  #scale_fill_viridis_d(option = "C")
p
plotly::ggplotly(p)


# Tx distribution ####
p <- 
  txs_joined |> 
  filter(tx_type == "expense") |> 
  mutate(
    text = glue("{subcategory_name}\n{stringr::str_trunc(description, 25)}\n{scales::dollar(amount, accuracy = 1)}")
  ) |> 
  
  ggplot(aes(x = log(amount), fill = category_name, text = text)) +
  geom_histogram(bins = 30) +
  scale_fill_viridis_d("Category", option = "C") +
  xlab("") +
  ylab("Count") +
  ggtitle("Transaction Distribution (Log)")

p
ggplotly(p, tooltip = c("fill", "text", "y"))
  layout(
    hovermode = "name"
  )
