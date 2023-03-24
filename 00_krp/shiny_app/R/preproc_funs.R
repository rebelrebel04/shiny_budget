# Preprocessing functions: take an input df (from read.csv),
# perform cleaning steps and return it
# Canonical schema for fct_transactions is:
# CREATE TABLE fct_transactions ( 
#1  account_nickname TEXT NOT NULL, 
#2  date TEXT NOT NULL, 
#3  description TEXT, 
#4  type TEXT,
#5  amount REAL NOT NULL, 
#6  transaction_id TEXT NOT NULL, 
#7  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, 
#8  created_by TEXT, 
#9  modified_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, 
#10 modified_by TEXT )


none <- function(csv_file, account_nickname = NULL) {
  readr::read_csv(csv_file, col_names = TRUE, name_repair = "universal")
}

# if other BC accounts are needed in future, could just refactor
# a function factory that spawns off the account_nickname,
# which is currently hardcoded here ("joint checking")
# NOTE: the account_nickname arg must exactly match the options
#       in fct_accounts
bc_joint_checking <- function(csv_file, account_nickname = "BC Joint Checking") {
  x <- 
    readr::read_csv(
      csv_file,
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
      account_nickname = account_nickname,
      date = paste(as.Date(strptime(Date, format = "%m/%d/%Y"))),
      # description = concat(Description, Memo),
      # Amount.Debit is negative, Amount.Credit is positive
      amount = Amount.Debit + Amount.Credit,
      type = ifelse(amount < 0, "debit", "credit"),      
      # Make all amounts positive values
      amount = abs(amount),
      # transaction_id = uuid::UUIDgenerate(n = n())
      # Compute the "unique" tx id as the concatenation of the fields
      # that *should* make this tx unique -- if this *isn't* unique
      # it will throw an error during ETL, so duplicate txs aren't appended
      transaction_id = glue("{account_nickname}-{date}-{description}-{type}-{amount}")
    ) |> 
    dplyr::select(
      account_nickname,
      date,
      description,
      type,
      amount,
      transaction_id      
    )
  # readr::write_csv(x, "~/Desktop/Shiny budget app/BayCoast_preproc.csv")
  # glimpse(x)
  x
}


# NOTE: the account_nickname arg must exactly match the options
#       in fct_accounts
chase_visa <- function(csv_file, account_nickname = "Chase Visa") {
  x <- 
    readr::read_csv(
      # csv_file,
      "~/Desktop/Shiny budget app/Chase7791_Activity20220901_20230228_20230323.CSV",
      skip = 0, 
      col_names = TRUE,
      col_types = "cccccnnc",
      name_repair = "universal",
      show_col_types = FALSE      
    ) |> 
    tidyr::replace_na(list(Amount = 0)) |>  
    tidyr::unite("description", Description, Category, Memo, na.rm = TRUE) |> 
    dplyr::mutate(
      account_nickname = account_nickname,
      date = paste(as.Date(strptime(Transaction.Date, format = "%m/%d/%Y"))),
      # description = concat(Description, Memo),
      type = ifelse(Amount < 0, "debit", "credit"),      
      # Make all amounts positive values
      amount = abs(Amount),
      # transaction_id = uuid::UUIDgenerate(n = n())
      # Compute the "unique" tx id as the concatenation of the fields
      # that *should* make this tx unique -- if this *isn't* unique
      # it will throw an error during ETL, so duplicate txs aren't appended
      transaction_id = glue("{account_nickname}-{date}-{description}-{type}-{amount}")
    ) |> 
    dplyr::select(
      account_nickname,
      date,
      description,
      type,
      amount,
      transaction_id      
    )
  # readr::write_csv(x, "~/Desktop/Shiny budget app/BayCoast_preproc.csv")
  # glimpse(x)
  x
}
