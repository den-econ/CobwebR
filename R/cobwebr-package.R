#' @keywords internal
#' @importFrom utils globalVariables
"_PACKAGE"

# Column names used via dplyr/ggplot2 non-standard evaluation.
globalVariables(c(
  "ticker", "issuer_name", "investor_name", "percentage",
  "total_holding_shares", "stake_top_10", "frontier_status",
  "unique_stocks", "free_float_mcap",
  "criterion_integrity", "criterion_free_float", "criterion_owner_conc",
  "criterion_broker_top", "criterion_institutional", "criterion_size_ffmc",
  "C", "H", "grp", "label", "y", "holder", "stake", "face"
))
