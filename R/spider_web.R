#' Identify linked holders: the market-wide shareholder linkage graph
#'
#' Builds the linkage graph \eqn{G = (V, E)} of Section 3.3 and partitions it
#' into connected components, each treated as one economic holder. Two holders
#' are linked when at least one rule holds:
#'
#' * **(i) co-occurrence**: they appear together as holders above 1 percent in
#'   at least `min_shared_stocks` stocks. Pairs of two individuals are not
#'   linked, because family links are not yet applied.
#' * **(ii) group name**: a corporate holder's name starts with the same
#'   distinctive word as the name of the stock's declared corporate controller
#'   (the group brand, e.g. MNC or CAPITAL), after legal forms and common words
#'   are removed.
#' * **(iii) offshore vehicles**: a single-stock offshore vehicle sits in a
#'   stock where one linked group already holds at least
#'   `min_offshore_vehicles` offshore vehicles; it joins that group.
#' * **(iv) captive fund**: a fund whose name contains the group brand of the
#'   stock's corporate controller.
#'
#' Broker and custodian nominee accounts and public sector holders are never
#' linked. Eligible institutions are linked only through rule (iv).
#'
#' Rule (i) is applied first. Rule (iii) uses the groups formed by rule (i).
#' Rules (ii) and (iv) attach holders to the controller within one stock.
#'
#' @param ksei_df Register from [load_ksei_register()].
#' @param rules Rules to apply, any of `"i"`, `"ii"`, `"iii"`, `"iv"`.
#' @param min_shared_stocks Minimum number of shared stocks for rule (i).
#' @param min_offshore_vehicles Minimum offshore vehicles of one group in a
#'   stock for rule (iii).
#' @param link_individual_pairs Link pairs of two individuals under rule (i)?
#' @param review Optional supervisory review table, see [read_link_review()].
#' @return An object of class `"spider_web"`: a list with `edges` (one row per
#'   link with the rule and the evidence), `holders` (the classified register
#'   with the group of every holding), `groups` (group sizes), `graph` (an
#'   `igraph` object), `review` and `params`.
#' @export
identify_group_links <- function(ksei_df, rules = c("i", "ii", "iii", "iv"),
                                 min_shared_stocks = 2L, min_offshore_vehicles = 3L,
                                 link_individual_pairs = FALSE, review = NULL) {
  rules <- match.arg(rules, c("i", "ii", "iii", "iv"), several.ok = TRUE)
  h <- classify_holders(ksei_df)
  edges <- list()
  add <- function(stock, a, b, rule, evidence) {
    if (length(a)) edges[[length(edges) + 1]] <<- data.frame(
      stock = stock, holder_a = a, holder_b = b, rule = rule, evidence = evidence,
      stringsAsFactors = FALSE)
  }
  vertices <- data.frame(name = unique(h$holder))

  if ("i" %in% rules) {
    e <- h[!h$is_nominee & !h$is_institution & !h$is_public_sector, ]
    individuals <- unique(e$holder[e$is_individual])
    by_stock <- lapply(split(e$holder, e$ticker), function(x) sort(unique(x)))
    by_stock <- by_stock[lengths(by_stock) >= 2]
    pairs <- do.call(rbind, Map(function(x, stock) {
      m <- utils::combn(x, 2)
      data.frame(a = m[1, ], b = m[2, ], stock = stock, stringsAsFactors = FALSE)
    }, by_stock, names(by_stock)))
    key <- paste(pairs$a, pairs$b, sep = "\r")
    n_shared <- table(key)
    p <- pairs[key %in% names(n_shared)[n_shared >= min_shared_stocks], ]
    if (!link_individual_pairs) p <- p[!(p$a %in% individuals & p$b %in% individuals), ]
    if (nrow(p)) {
      ev <- tapply(p$stock, paste(p$a, p$b, sep = "\r"), paste, collapse = " ")
      ab <- do.call(rbind, strsplit(names(ev), "\r", fixed = TRUE))
      add(NA_character_, ab[, 1], ab[, 2], "i", paste("co-holders in", unname(ev)))
    }
  }

  if ("iii" %in% rules) {
    el <- if (length(edges)) do.call(rbind, edges)[, c("holder_a", "holder_b")] else
      data.frame(from = character(0), to = character(0))
    g <- igraph::graph_from_data_frame(el, directed = FALSE, vertices = vertices)
    memb <- igraph::components(g)$membership
    size <- tabulate(memb)
    keep <- !h$is_nominee
    for (d in split(h[keep, ], h$ticker[keep])) {
      grp <- memb[d$holder]
      linked_vehicle <- d$is_offshore_vehicle & size[grp] > 1
      if (!any(linked_vehicle)) next
      counts <- table(grp[linked_vehicle])
      if (max(counts) < min_offshore_vehicles) next
      root <- as.integer(names(counts)[which.max(counts)])
      anchor <- d$holder[linked_vehicle & grp == root][1]
      new <- d$holder[d$is_offshore_vehicle & d$n_stocks == 1 & grp != root]
      add(d$ticker[1], new, rep(anchor, length(new)), "iii",
          rep(sprintf("single-stock offshore vehicle; linked group holds %d offshore vehicles here",
                      max(counts)), length(new)))
    }
  }

  if (any(c("ii", "iv") %in% rules)) {
    h$brand <- brand_token(h$holder)
    keep <- !h$is_nominee & !h$is_public_sector
    for (d in split(h[keep, ], h$ticker[keep])) {
      for (cn in unique(d$holder[d$is_controller & !d$is_individual])) {
        b <- d$brand[d$holder == cn][1]
        if (is.na(b)) next
        if ("ii" %in% rules) {
          s <- d$brand %in% b & d$holder != cn & !d$is_individual & !d$is_institution
          add(d$ticker[1], d$holder[s], rep(cn, sum(s)), "ii",
              rep(paste("shares controller's group name:", b), sum(s)))
        }
        if ("iv" %in% rules) {
          fund <- d$investor_class %in% c("Mutual Funds", "Exchange Traded Funds",
                                          "Investment Manager")
          s <- fund & d$holder != cn &
            vapply(d$holder, function(x) b %in% distinctive_tokens(x), logical(1))
          add(d$ticker[1], d$holder[s], rep(cn, sum(s)), "iv",
              rep(paste("fund named after controller:", b), sum(s)))
        }
      }
    }
    h$brand <- NULL
  }

  el <- if (length(edges)) do.call(rbind, edges) else
    data.frame(stock = character(0), holder_a = character(0), holder_b = character(0),
               rule = character(0), evidence = character(0))
  el <- el[!duplicated(el[, c("stock", "holder_a", "holder_b", "rule")]), ]
  review <- validate_review(review)
  unlink <- review[review$decision == "unlink", ]
  if (nrow(unlink)) {
    pair <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "\r")
    el <- el[!pair(el$holder_a, el$holder_b) %in% pair(unlink$holder_a, unlink$holder_b), ]
  }
  rownames(el) <- NULL

  g <- igraph::graph_from_data_frame(el[, c("holder_a", "holder_b", "rule", "stock")],
                                     directed = FALSE, vertices = vertices)
  memb <- igraph::components(g)$membership
  h$group <- unname(memb[h$holder])
  groups <- data.frame(group = seq_along(tabulate(memb)), n_holders = tabulate(memb))

  structure(list(edges = el, holders = h, groups = groups, graph = g, review = review,
                 params = list(rules = rules, min_shared_stocks = min_shared_stocks,
                               min_offshore_vehicles = min_offshore_vehicles,
                               link_individual_pairs = link_individual_pairs)),
            class = "spider_web")
}

#' @export
print.spider_web <- function(x, ...) {
  linked <- x$groups$n_holders > 1
  cat("<spider_web> shareholder linkage graph\n")
  cat(sprintf("  Holders: %d, links: %d, linked groups: %d (largest: %d holders)\n",
              nrow(x$groups), nrow(x$edges), sum(linked),
              if (any(linked)) max(x$groups$n_holders) else 0L))
  r <- table(factor(x$edges$rule, c("i", "ii", "iii", "iv")))
  cat(sprintf("  Links by rule: (i) %d, (ii) %d, (iii) %d, (iv) %d\n", r[1], r[2], r[3], r[4]))
  ex <- x$review$sw_ticker[x$review$decision == "exclude_stock"]
  if (length(ex)) cat(sprintf("  Stocks kept at reported ownership after review: %s\n",
                              paste(ex, collapse = ", ")))
  invisible(x)
}

#' @export
summary.spider_web <- function(object, ...) {
  r <- table(factor(object$edges$rule, c("i", "ii", "iii", "iv")))
  data.frame(rule = names(r), links = as.vector(r),
             description = c("co-holders in two or more stocks", "shared group name",
                             "single-stock offshore vehicles", "captive fund"))
}

#' Read a supervisory review table for linked holders
#'
#' The review table records decisions that override the mechanical rules:
#' `decision = "exclude_stock"` keeps a stock at its reported ownership in the
#' spider web assessment; `decision = "unlink"` removes the link between
#' `holder_a` and `holder_b`. Every row carries a `reason`.
#'
#' @param file_path CSV with columns `sw_ticker`, `holder_a`, `holder_b`,
#'   `decision`, `reason`. Defaults to the bundled `link_review.csv`, which
#'   reproduces the paper's baseline.
#' @return A data frame.
#' @export
read_link_review <- function(file_path = system.file("extdata", "link_review.csv",
                                                     package = "CobwebR")) {
  validate_review(utils::read.csv(file_path, stringsAsFactors = FALSE,
                                  na.strings = c("", "NA")))
}

validate_review <- function(review) {
  cols <- c("sw_ticker", "holder_a", "holder_b", "decision", "reason")
  if (is.null(review)) {
    return(stats::setNames(data.frame(matrix(character(0), 0, 5),
                                      stringsAsFactors = FALSE), cols))
  }
  stopifnot(all(cols %in% names(review)),
            all(review$decision %in% c("exclude_stock", "unlink")))
  review$holder_a <- ifelse(is.na(review$holder_a), NA, normalise_holder(review$holder_a))
  review$holder_b <- ifelse(is.na(review$holder_b), NA, normalise_holder(review$holder_b))
  review[, cols]
}
