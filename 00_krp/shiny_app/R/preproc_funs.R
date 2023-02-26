# Preprocessing functions: take an input df (from read.csv),
# perform cleaning steps and return it
# Canonical schema for fct_transactions is:
# CREATE TABLE fct_transactions ( 
#1  account_nickname TEXT NOT NULL, 
#2  date TEXT NOT NULL, 
#3  transaction_number INT, 
#4  description TEXT, 
#5  type TEXT,
#6  amount REAL NOT NULL, 
#7  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, 
#8  created_by TEXT, 
#9  modified_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, 
#10 modified_by TEXT )


none <- function(x) {
  readr::read_csv(x, col_names = TRUE, name_repair = "universal")
}

# if other BC accounts are needed in future, could just refactor
# a function factory that spawns off the account_nickname,
# which is currently hardcoded here ("joint checking")
bc_joint_checking <- function(x) {
  x <- 
    readr::read_csv(
      x,
      # "~/Downloads/BayCoast_raw_alltodate.csv", 
      skip = 3, 
      col_names = TRUE,
      col_types = "ccccnnnnn",
      name_repair = "universal",
      show_col_types = FALSE      
    ) |> 
    tidyr::replace_na(list(Amount.Debit = 0, Amount.Credit = 0)) |>  
    tidyr::unite("description", Description, Memo, na.rm = TRUE) |> 
    dplyr::mutate(
      account_nickname = "BC Joint Checking",
      date = paste(as.Date(strptime(Date, format = "%m/%d/%Y"))),
      # description = concat(Description, Memo),
      # Amount.Debit is negative, Amount.Credit is positive
      amount = abs(Amount.Debit + Amount.Credit),
      type = ifelse(amount < 0, "debit", "credit"),
      transaction_number = uuid::UUIDgenerate(n = n())
    ) |> 
    dplyr::select(
      account_nickname,
      date,
      transaction_number,
      description,
      type,
      amount
    )
  # readr::write_csv(x, "~/Desktop/Shiny budget app/BayCoast_preproc.csv")
  # glimpse(x)
  x
}
