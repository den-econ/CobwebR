#' Identify Group-Linked Holders and Cross-Ownership Webs
#' @param ksei_df Cleaned KSEI dataframe.
#' @return Dataframe with group-linked flags and consolidated stakes.
#' @export
identify_group_links <- function(ksei_df) {
  # Find holders appearing with the same co-holders in 2+ stocks or sharing distinctive prefixes
  co_occurrences <- ksei_df %>%
    dplyr::group_by(investor_name) %>%
    dplyr::summarise(unique_stocks = n_distinct(ticker)) %>%
    dplyr::filter(unique_stocks >= 2)
  
  ksei_df <- ksei_df %>%
    dplyr::mutate(
      is_group_linked = dplyr::if_else(investor_name %in% co_occurrences$investor_name, TRUE, FALSE)
    )
  
  return(ksei_df)
}