library(dplyr)
library(tidyr)
library(tibble)

# RULES ####
# Make some sample rules to seed this table
# rule = regex string
# key = category (or whatever) to assign matching cases
rules <- tribble(
  ~account,                   ~key,         ~category,      ~subcategory,  ~expense_type,  ~tags,      ~rule,
  "BayCoast Joint Checking",  "Vanguard",   "retirement",   "401k",        "expense",      "savings",  "vanguard",
  "BayCoast Joint checking",  "AT&T",       "utilities",    "mobile",      "expense",      "",         "att"
)
rules
saveRDS(rules, "./00_krp/data_prep/data/regex_tester/rules.RDS")


# test some visualizations
library(ggplot2)
library(treemapify)
set.seed(1234)
txs <- tibble(
  description = sample(c("vanguard", "interest", "att", "foo"), 100, replace = TRUE),
  amount = sample(10:1000, 100, replace = TRUE)
)
txs %>%
  count(description, sort = TRUE) %>%
  # count(description, wt = amount, sort = TRUE) %>%
  ggplot(aes(area = n, fill = n, label = description)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "gray", place = "centre", grow = FALSE) +
  scale_fill_viridis_c(option = "B")


