#' Consolidate linked holders within each stock
#'
#' Applies the spider web consolidation (Section 3.4): linked holders in a
#' stock are counted as one holder, and KSEI-based free float is recomputed
#' after adding linked holders together:
#' \deqn{\tilde{H}_i = |\mathcal{C}_i|, \qquad
#'       f_{K,i} = 100 - \sum_{V \in \mathcal{C}_i : s_{iV} \ge 5} s_{iV}}
#' Eligible institutions below 10 percent that are not linked are never
#' removed from free float. Stocks excluded in the review table keep their
#' reported values.
#'
#' @param links A `"spider_web"` object from [identify_group_links()].
#' @return One row per stock: `ticker`, combined stake `C`, reported holder
#'   count `H`, consolidated holder count `H_tilde`, free float before
#'   (`f_K_reported`) and after (`f_K`) linking, and `reviewed` (TRUE when a
#'   review decision kept the stock at reported values).
#' @export
consolidate_holders <- function(links) {
  stopifnot(inherits(links, "spider_web"))
  h <- links$holders
  excluded <- links$review$sw_ticker[links$review$decision == "exclude_stock"]
  float_after <- function(stake, group, protected) {
    block <- tapply(stake, group, sum)
    removable <- tapply(!protected, group, any)
    100 - sum(block[block >= 5 & removable])
  }
  out <- do.call(rbind, lapply(split(h, h$ticker), function(d) {
    protected <- d$is_institution & d$percentage < 10 &
      !(d$group %in% d$group[duplicated(d$group)])
    own <- seq_len(nrow(d))
    rev <- d$ticker[1] %in% excluded
    data.frame(
      ticker = d$ticker[1],
      C = sum(d$percentage),
      H = nrow(d),
      H_tilde = if (rev) nrow(d) else length(unique(d$group)),
      f_K_reported = float_after(d$percentage, own, protected),
      f_K = float_after(d$percentage, if (rev) own else d$group, protected),
      reviewed = rev,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

#' Holders of one stock, as reported and after the spider web assessment
#'
#' @param links A `"spider_web"` object.
#' @param ticker Stock code.
#' @param anonymise Replace holder names by generic labels (controller,
#'   offshore entity, domestic holder, nominee account, ...), as in the paper.
#' @return One row per holder, largest first: `holder`, `stake`, `type`,
#'   `group` (a label shared by linked holders, `""` if not linked) and the
#'   rules that link it.
#' @export
stock_web <- function(links, ticker, anonymise = TRUE) {
  h <- links$holders[links$holders$ticker == ticker, ]
  h <- h[order(-h$percentage), ]
  dup <- h$group[duplicated(h$group)]
  type <- ifelse(h$is_controller, "Controller",
          ifelse(h$is_nominee, "Nominee account",
          ifelse(h$is_institution, "Institution",
          ifelse(h$is_individual, "Individual",
          ifelse(h$local_foreign %in% "F" | h$is_offshore_vehicle, "Offshore entity",
                 "Domestic entity")))))
  grp_ids <- unique(h$group[h$group %in% dup])
  group <- ifelse(h$group %in% dup, paste("Linked group", match(h$group, grp_ids)), "")
  if (ticker %in% links$review$sw_ticker[links$review$decision == "exclude_stock"]) group[] <- ""
  e <- links$edges
  rules <- vapply(h$holder, function(x) {
    r <- e$rule[(e$holder_a == x | e$holder_b == x) & (is.na(e$stock) | e$stock == ticker)]
    paste(sort(unique(r)), collapse = ",")
  }, character(1))
  holder <- h$investor_name
  if (anonymise) {
    holder <- ave(type, type, FUN = function(t) {
      if (length(t) == 1 && t[1] == "Controller") return("Controlling holder")
      paste(t, seq_along(t))
    })
  }
  data.frame(holder = holder, stake = h$percentage, type = type, group = group,
             rules = unname(rules), stringsAsFactors = FALSE)
}
