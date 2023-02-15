library(dplyr)
library(tidyr)
library(tibble)

# RULES ####
# Make some sample rules to seed this table
# rule = regex string
# key = category (or whatever) to assign matching cases
rules <- tribble(
  ~key,         ~category,      ~subcategory,  ~expense_type,  ~tags,      ~rule,
  "Vanguard",   "retirement",   "401k",        "expense",      "savings",  "vanguard",
  "AT&T",       "utilities",    "mobile",      "expense",      "",         "att"      
)
rules
saveRDS(rules, "./00_krp/data_prep/data/regex_tester/rules.RDS")


