#' HSC frontier and HSC-equivalent classification
#'
#' Computes the combined stake \eqn{C_i} and holder count \eqn{H_i} of each
#' stock, the frontier anchors (eq. sw-frontier) and the HSC-equivalent zone
#' (eq. sw-zone):
#' \deqn{\mathcal{Z}_{HSC} = \{(C, H) : \exists j \in \mathcal{D},\ C \ge C_j,\ H \le H_j\}}
#' With `links`, each stock is also tested at its consolidated holder count
#' \eqn{\tilde{H}_i}, and stocks are classified as `HSC` (designated),
#' `Equivalent` (in the zone as reported), `Added` (in the zone only after the
#' spider web assessment) or `Outside`.
#'
#' @param x A KSEI register from [load_ksei_register()], or a stock-level data
#'   frame with columns `ticker`, `C` and `H` (e.g. from a monthly panel).
#' @param hsc_list Designated HSC stocks: a character vector of tickers or a
#'   data frame with a `sw_ticker` or `ticker` column.
#' @param links Optional `"spider_web"` object from [identify_group_links()].
#' @param digits Rounding of `C` before comparison (the paper uses 2 decimals).
#' @return An object of class `"hsc_frontier"`: a list with `stocks` (one row
#'   per stock), `anchors` and `hsc` (the designated stocks' C and H).
#' @export
calculate_hsc_frontier <- function(x, hsc_list, links = NULL, digits = 2) {
  hsc <- if (is.data.frame(hsc_list)) {
    hsc_list[[intersect(c("sw_ticker", "ticker"), names(hsc_list))[1]]]
  } else hsc_list
  if (!is.null(links)) {
    s <- consolidate_holders(links)
  } else if (all(c("ticker", "C", "H") %in% names(x))) {
    s <- as.data.frame(x)[, c("ticker", "C", "H")]
    s$H_tilde <- s$H
  } else {
    s <- stats::aggregate(percentage ~ ticker, x, function(p) c(C = sum(p), H = length(p)))
    s <- data.frame(ticker = s$ticker, C = s$percentage[, "C"], H = s$percentage[, "H"])
    s$H_tilde <- s$H
  }
  s$C <- round(s$C, digits)
  s$is_hsc <- s$ticker %in% hsc
  pts <- s[s$is_hsc, c("ticker", "C", "H")]
  s$H_max <- frontier_holders(s$C, pts)
  s$group <- ifelse(s$is_hsc, "HSC",
             ifelse(s$H <= s$H_max, "Equivalent",
             ifelse(s$H_tilde <= s$H_max, "Added", "Outside")))
  structure(list(stocks = s, anchors = frontier_anchors(pts), hsc = pts,
                 linked = !is.null(links)),
            class = "hsc_frontier")
}

#' Frontier anchors: the least concentrated designated stocks
#'
#' @param hsc Data frame with columns `C` and `H` for designated stocks.
#' @return The anchor rows, ordered by `C`.
#' @export
frontier_anchors <- function(hsc) {
  keep <- vapply(seq_len(nrow(hsc)), function(v) {
    !any(hsc$C <= hsc$C[v] & hsc$H >= hsc$H[v] & (hsc$C < hsc$C[v] | hsc$H > hsc$H[v]))
  }, logical(1))
  a <- hsc[keep, , drop = FALSE]
  a <- a[!duplicated(a[, c("C", "H")]), , drop = FALSE]
  a[order(a$C), , drop = FALSE]
}

#' Maximum holder count allowed by the frontier
#'
#' For each combined stake, the largest holder count of a designated stock with
#' an equal or lower combined stake. A stock with `H <= frontier_holders(C)` is
#' HSC-equivalent; `-Inf` means no designated stock has a stake that low.
#'
#' @param C Combined stakes.
#' @param hsc Data frame with columns `C` and `H` for designated stocks.
#' @return Numeric vector.
#' @export
frontier_holders <- function(C, hsc) {
  vapply(C, function(c) {
    h <- hsc$H[hsc$C <= c + 1e-9]
    if (length(h)) max(h) else -Inf
  }, numeric(1))
}

#' @export
print.hsc_frontier <- function(x, ...) {
  cat("<hsc_frontier>\n  Anchors (C, H):",
      paste(sprintf("(%.2f, %d)", x$anchors$C, as.integer(x$anchors$H)), collapse = " "), "\n")
  n <- table(factor(x$stocks$group, c("HSC", "Equivalent", "Added", "Outside")))
  cat(sprintf("  HSC %d | Equivalent %d | Added %d | Outside %d\n", n[1], n[2], n[3], n[4]))
  invisible(x)
}

#' Summarise HSC-equivalent stocks by group
#'
#' Reproduces the layout of the paper's results table.
#'
#' @param object An `"hsc_frontier"` object.
#' @param market Optional data frame with `sw_ticker` (or `ticker`) and
#'   `mcap_idr_bn` (market capitalisation, IDR billion).
#' @param ... Unused.
#' @return Data frame with stocks and, if `market` is given, market
#'   capitalisation in IDR trillion per group, with a total row for
#'   HSC-equivalent stocks.
#' @export
summary.hsc_frontier <- function(object, market = NULL, ...) {
  s <- object$stocks
  lev <- c("HSC", "Equivalent", "Added")
  out <- data.frame(group = c(lev, "Total HSC-equivalent"),
                    stocks = c(vapply(lev, function(g) sum(s$group == g), integer(1)), NA))
  if (!is.null(market)) {
    key <- intersect(c("sw_ticker", "ticker"), names(market))[1]
    s$mcap <- market$mcap_idr_bn[match(s$ticker, market[[key]])] / 1000
    out$mcap_idr_tn <- c(vapply(lev, function(g) sum(s$mcap[s$group == g], na.rm = TRUE),
                                numeric(1)), NA)
    out$mcap_idr_tn[4] <- sum(out$mcap_idr_tn[1:3])
  }
  out$stocks[4] <- sum(out$stocks[1:3])
  out
}

#' Sensitivity of the HSC-equivalent count to the frontier
#'
#' Shifts the combined stake of every designated stock by each value in
#' `shifts` (percentage points) and counts undesignated stocks that are
#' HSC-equivalent on reported ownership.
#'
#' @param x An `"hsc_frontier"` object.
#' @param shifts Shifts in percentage points.
#' @return Data frame with `shift` and `n`.
#' @export
frontier_sensitivity <- function(x, shifts = seq(-2, 2, by = 0.25)) {
  s <- x$stocks[!x$stocks$is_hsc, ]
  data.frame(shift = shifts, n = vapply(shifts, function(d) {
    hsc <- x$hsc
    hsc$C <- hsc$C + d
    sum(s$H <= frontier_holders(s$C, hsc))
  }, integer(1)))
}

#' Stocks inside the top-10 concentration threshold over time
#'
#' Counts stocks whose ten largest holders above 1 percent own at least
#' `threshold` percent, per period, and the entries and exits between
#' consecutive periods.
#'
#' @param panel Data frame with `sw_ticker`, `period_end` and `stake_top10_pct`
#'   (e.g. the bundled `top10_panel_2026.csv`).
#' @param threshold Threshold in percent.
#' @return Data frame with `period_end`, `inside`, `entries` and `exits`.
#' @export
frontier_stability <- function(panel, threshold = 92.5) {
  inside <- split(panel$sw_ticker[panel$stake_top10_pct >= threshold],
                  panel$period_end[panel$stake_top10_pct >= threshold])
  p <- sort(names(inside))
  data.frame(
    period_end = p,
    inside = lengths(inside[p]),
    entries = c(NA, vapply(seq_along(p)[-1], function(k)
      length(setdiff(inside[[p[k]]], inside[[p[k - 1]]])), integer(1))),
    exits = c(NA, vapply(seq_along(p)[-1], function(k)
      length(setdiff(inside[[p[k - 1]]], inside[[p[k]]])), integer(1))),
    row.names = NULL
  )
}
