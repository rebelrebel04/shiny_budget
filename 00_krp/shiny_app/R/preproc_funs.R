# Preprocessing functions: take an input df (from read.csv),
# perform cleaning steps and return it
# Canonical schema for fct_transactions is:
# CREATE TABLE fct_transactions ( 
#1  account_nickname TEXT NOT NULL, 
#2  date TEXT NOT NULL, 
#3  transaction_number INT, 
#4  description TEXT, 
#5  amount REAL NOT NULL, 
#6  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, 
#7  created_by TEXT, 
#8  modified_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, 
#9  modified_by TEXT )


default <- function(x) {
  readr::read_csv(x, col_names = TRUE, name_repair = "universal")
}

baycoast <- function(x) {
  x <- 
    readr::read_csv(
      x,
      # "~/Downloads/BayCoast_raw_alltodate.csv", 
      skip = 3, 
      col_names = TRUE,
      col_types = "ccccnnnnn",
      name_repair = "universal"
    ) |> 
    tidyr::replace_na(list(Amount.Debit = 0, Amount.Credit = 0)) |>  
    tidyr::unite("description", Description, Memo, na.rm = TRUE) |> 
    dplyr::mutate(
      account = "BayCoast Joint Checking",
      date = as.Date(strptime(Date, format = "%m/%d/%Y")),
      # description = concat(Description, Memo),
      # Amount.Debit is negative, Amount.Credit is positive
      amount = abs(Amount.Debit + Amount.Credit),
      type = ifelse(amount < 0, "debit", "credit")
    ) |> 
    dplyr::group_by(account, date) |> 
    dplyr::mutate(transaction_number = 1:n()) |> 
    dplyr::ungroup() |> 
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
