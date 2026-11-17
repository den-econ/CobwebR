#' Calculate High Shareholding Concentration (HSC) Frontier
#' @param ksei_df Cleaned KSEI dataframe.
#' @param threshold Top-10 stake threshold (default 0.925).
#' @return Summary dataframe of stock concentration and frontier status.
#' @export
calculate_hsc_frontier <- function(ksei_df, threshold = 0.925) {
  frontier_summary <- ksei_df %>%
    dplyr::group_by(ticker, issuer_name) %>%
    dplyr::arrange(dplyr::desc(percentage)) %>%
    dplyr::summarise(
      total_holders_gt_1pct = n(),
      stake_all_gt_1pct = sum(percentage, na.rm = TRUE) / 100,
      stake_top_10 = sum(percentage[1:min(10, n())], na.rm = TRUE) / 100,
      largest_holder = first(investor_name),
      largest_holder_stake = first(percentage) / 100,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      gap_to_threshold = (stake_top_10 - threshold) * 100,
      frontier_status = dplyr::if_else(stake_top_10 >= threshold, "Inside", "Outside")
    )
  
  return(frontier_summary)
}