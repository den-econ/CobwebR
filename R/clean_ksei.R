#' Load and Clean KSEI Ownership Register
#' @param file_path Path to the KSEI file: the raw IDX Excel download, or a CSV with the same columns (e.g. the bundled `ksei_ownership_2026-10-30.csv`).
#' @return A cleaned tibble of shareholdings.
#' @export
load_ksei_register <- function(file_path) {
  raw_data <- if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    readxl::read_excel(file_path, skip = 3)
  }
  
  cleaned <- raw_data |>
    dplyr::rename(
      date = 1,
      ticker = 2,
      issuer_name = 3,
      investor_name = 4,
      investor_class = 5,
      controller_affiliate = 6,
      local_foreign = 7,
      nationality = 8,
      domicile = 9,
      holdings_scripless = 10,
      holdings_scrip = 11,
      total_holding_shares = 12,
      percentage = 13
    ) |>
    dplyr::mutate(
      percentage = as.numeric(percentage),
      total_holding_shares = as.numeric(total_holding_shares)
    )
  
  return(cleaned)
}