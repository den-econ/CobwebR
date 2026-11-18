sw_blue <- "#2b6cb0"
sw_orange <- "#dd8a00"

frontier_path <- function(anchors, x_max = 100) {
  a <- anchors[order(anchors$C), ]
  nxt <- c(a$C[-1], x_max)
  data.frame(C = c(a$C[1], as.vector(rbind(a$C, nxt))),
             H = c(0, as.vector(rbind(a$H, a$H))))
}

#' Plot the HSC frontier and the HSC-equivalent zone
#'
#' Draws every stock in the concentration space (combined stake against holder
#' count), coloured by group, with the frontier staircase and the shaded zone,
#' as in the paper's frontier figure. Stocks added by the spider web assessment
#' are drawn at their reported holder count.
#'
#' @param x An `"hsc_frontier"` object.
#' @param xlim Range of combined stakes shown, in percent.
#' @param h_max Holder counts above this are drawn at `h_max`.
#' @param labels Optional named character vector: names are tickers, values are
#'   labels drawn next to those stocks.
#' @return A `ggplot` object.
#' @export
plot_hsc_frontier <- function(x, xlim = c(80, 100), h_max = 30, labels = NULL) {
  stopifnot(inherits(x, "hsc_frontier"))
  d <- x$stocks[x$stocks$C >= xlim[1], ]
  d$H <- pmin(d$H, h_max)
  lev <- c("Outside", "Equivalent", "Added", "HSC")
  nm <- c("Outside the zone", "HSC-equivalent, not designated",
          "Added by the spider web assessment", "Designated HSC")
  nm <- paste0(nm, " (", vapply(lev, function(g) sum(d$group == g), integer(1)), ")")
  d$grp <- factor(nm[match(d$group, lev)], levels = nm)
  fp <- frontier_path(x$anchors, xlim[2])
  zone <- rbind(fp, data.frame(C = xlim[2], H = 0))
  lab <- d[d$ticker %in% names(labels), ]
  lab$label <- labels[lab$ticker]
  ggplot2::ggplot(d, ggplot2::aes(C, H)) +
    ggplot2::geom_polygon(data = zone, fill = sw_blue, alpha = 0.07) +
    ggplot2::geom_point(ggplot2::aes(colour = grp), size = 1.3, alpha = 0.85) +
    ggplot2::geom_path(data = fp, linewidth = 0.8) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = label), hjust = 1.1, vjust = -0.6,
                       size = 3, fontface = "bold") +
    ggplot2::scale_colour_manual(values = stats::setNames(c("grey75", sw_blue, sw_orange, "black"), nm),
                                 name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = xlim, labels = function(v) paste0(v, "%")) +
    ggplot2::scale_y_continuous(limits = c(0, h_max)) +
    ggplot2::labs(x = "Combined stake of holders above 1%", y = "Holder count") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, override.aes = list(size = 2.5))) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}

#' Plot one stock against the frontier, before and after the assessment
#'
#' The frontier is drawn from the designated stocks other than `ticker`, so the
#' test does not depend on the stock's own designation.
#'
#' @param x An `"hsc_frontier"` object.
#' @param ticker Stock code.
#' @param label Label for the stock, e.g. `"Stock G"`.
#' @param H_after Holder count after the assessment; defaults to the stock's
#'   consolidated holder count in `x`.
#' @param xlim,h_max Plot ranges.
#' @return A `ggplot` object.
#' @export
plot_stock_case <- function(x, ticker, label = ticker, H_after = NULL,
                            xlim = c(85, 100), h_max = 25) {
  s <- x$stocks[x$stocks$ticker == ticker, ]
  if (is.null(H_after)) H_after <- s$H_tilde
  hsc <- x$hsc[x$hsc$ticker != ticker, ]
  hsc$H <- pmin(hsc$H, h_max)
  fp <- frontier_path(frontier_anchors(hsc), xlim[2])
  zone <- rbind(fp, data.frame(C = xlim[2], H = 0))
  rep <- data.frame(C = s$C, H = s$H)
  aft <- data.frame(C = s$C, H = H_after)
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = zone, ggplot2::aes(C, H), fill = sw_blue, alpha = 0.08) +
    ggplot2::geom_point(data = hsc, ggplot2::aes(C, H), colour = "grey65", size = 1.5) +
    ggplot2::geom_path(data = fp, ggplot2::aes(C, H), linewidth = 0.8) +
    ggplot2::annotate("segment", x = rep$C, xend = aft$C, y = rep$H - 0.5, yend = aft$H + 0.6,
                      colour = sw_orange, linetype = "dashed",
                      arrow = ggplot2::arrow(length = grid::unit(0.15, "cm"))) +
    ggplot2::geom_point(data = rep, ggplot2::aes(C, H), shape = 21, fill = "white",
                        colour = sw_orange, size = 2.8, stroke = 1.1) +
    ggplot2::geom_point(data = aft, ggplot2::aes(C, H), colour = sw_orange, size = 2.8) +
    ggplot2::annotate("text", x = rep$C, y = rep$H, hjust = 1.08, vjust = -0.9, size = 3,
                      fontface = "bold", label = sprintf("%s as reported: %d holders", label, rep$H)) +
    ggplot2::annotate("text", x = aft$C, y = aft$H, hjust = -0.06, vjust = 1.8, size = 3,
                      fontface = "bold",
                      label = sprintf("After the spider web assessment: %d holders", aft$H)) +
    ggplot2::scale_x_continuous(limits = xlim, labels = function(v) paste0(v, "%")) +
    ggplot2::scale_y_continuous(limits = c(0, h_max)) +
    ggplot2::labs(x = "Combined stake of holders above 1%", y = "Holder count") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

#' Plot a stock's holders as reported and after linking
#'
#' Left: every holder above 1 percent. Right: linked holders counted as one.
#' Shaded rows are linked.
#'
#' @param web Data frame with `holder`, `stake` and `group` (`""` when not
#'   linked), e.g. from [stock_web()].
#' @return A `ggplot` object.
#' @export
plot_stock_web <- function(web) {
  fmt <- function(v) ifelse(v >= 10, paste0(round(v), "%"), paste0(sprintf("%.1f", v), "%"))
  df <- web
  n <- nrow(df)
  df$y <- -seq_len(n)
  grp <- unique(df$group[df$group != ""])
  after <- rbind(
    data.frame(label = sprintf("%s (%d holders)", grp,
                               vapply(grp, function(g) sum(df$group == g), integer(1))),
               stake = vapply(grp, function(g) sum(df$stake[df$group == g]), numeric(1)),
               face = rep("bold", length(grp))),
    data.frame(label = df$holder[df$group == ""], stake = df$stake[df$group == ""],
               face = rep("plain", sum(df$group == "")))
  )
  after$y <- -seq_len(nrow(after))
  m <- nrow(after)
  total <- sum(df$stake)
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = df[df$group != "", ],
                       ggplot2::aes(xmin = 0, xmax = 4, ymin = y - 0.45, ymax = y + 0.45),
                       fill = sw_blue, alpha = 0.15) +
    ggplot2::geom_text(data = df, ggplot2::aes(0.1, y, label = holder), hjust = 0, size = 3.1) +
    ggplot2::geom_text(data = df, ggplot2::aes(3.9, y, label = fmt(stake)), hjust = 1, size = 3.1) +
    ggplot2::geom_rect(data = after[after$face == "bold", ],
                       ggplot2::aes(xmin = 5, xmax = 9, ymin = y - 0.45, ymax = y + 0.45),
                       fill = sw_blue, alpha = 0.15) +
    ggplot2::geom_text(data = after, ggplot2::aes(5.1, y, label = label, fontface = face),
                       hjust = 0, size = 3.1) +
    ggplot2::geom_text(data = after, ggplot2::aes(8.9, y, label = fmt(stake), fontface = face),
                       hjust = 1, size = 3.1) +
    ggplot2::annotate("segment", x = 0, xend = 4, y = -n - 0.6, yend = -n - 0.6, colour = "grey70") +
    ggplot2::annotate("text", x = 0.1, y = -n - 1.1, label = sprintf("Total: %d holders", n),
                      hjust = 0, fontface = "bold", size = 3.1) +
    ggplot2::annotate("text", x = 3.9, y = -n - 1.1, label = fmt(total), hjust = 1,
                      fontface = "bold", size = 3.1) +
    ggplot2::annotate("segment", x = 5, xend = 9, y = -m - 0.6, yend = -m - 0.6, colour = "grey70") +
    ggplot2::annotate("text", x = 5.1, y = -m - 1.1, label = sprintf("Total: %d holders", m),
                      hjust = 0, fontface = "bold", size = 3.1) +
    ggplot2::annotate("text", x = 8.9, y = -m - 1.1, label = fmt(total), hjust = 1,
                      fontface = "bold", size = 3.1) +
    ggplot2::annotate("text", x = 0, y = 0.9, label = "As reported", hjust = 0,
                      fontface = "bold", size = 3.5) +
    ggplot2::annotate("text", x = 5, y = 0.9, label = "After the spider web assessment",
                      hjust = 0, fontface = "bold", size = 3.5) +
    ggplot2::annotate("segment", x = 4.2, xend = 4.8, y = -2, yend = -2,
                      arrow = ggplot2::arrow(length = grid::unit(0.15, "cm"))) +
    ggplot2::coord_cartesian(xlim = c(0, 9), ylim = c(-n - 1.5, 1.3), clip = "off") +
    ggplot2::theme_void()
}

#' @export
plot.hsc_frontier <- function(x, ...) print(plot_hsc_frontier(x, ...))
