#' Run the Green List Screener
#' @param ksei_df Cleaned KSEI dataframe.
#' @param market_data Dataframe containing Bloomberg market cap, turnover, and broker stats.
#' @return Filtered dataframe of stocks passing all criteria.
#' @export
run_green_list_screener <- function(ksei_df, market_data) {
  # Criterion I: Trading Integrity (No HSC, no special notations, no UMA/suspensions)
  # Criterion F: Free Float >= 15%, Ownership Concentration <= 85%
  # Criterion B: Top broker share < 30%, Group brokers <= 15%
  # Criterion P: Institutional participation rules ①, ②, ③
  # Criterion S: Free Float Market Cap >= IDR 1tn
  # Criterion L: ADTV complementary check
  
  # Placeholder integration logic
  greentable <- market_data %>%
    dplyr::filter(
      criterion_integrity == TRUE,
      criterion_free_float >= 0.15,
      criterion_owner_conc <= 0.85,
      criterion_broker_top < 0.30,
      criterion_institutional == TRUE,
      criterion_size_ffmc >= 1.0 # IDR 1tn
    ) %>%
    dplyr::arrange(dplyr::desc(free_float_mcap))
  
  return(greentable)
}