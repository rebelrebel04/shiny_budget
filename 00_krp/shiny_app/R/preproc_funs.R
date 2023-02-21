# Preprocessing functions: take an input df (from read.csv),
# perform cleaning steps and return it

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
      account,
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
