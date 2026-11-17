#' Plot HSC Frontier
#' @param frontier_df Output from calculate_hsc_frontier().
#' @return ggplot2 object.
#' @export
plot_hsc_frontier <- function(frontier_df) {
  ggplot2::ggplot(frontier_df, ggplot2::aes(x = reorder(ticker, stake_top_10), y = stake_top_10, fill = frontier_status)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "HSC Shareholding Concentration Frontier",
      x = "Ticker",
      y = "Top-10 Stakeholder Concentration"
    ) +
    ggplot2::theme_minimal()
}