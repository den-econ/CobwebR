# Reference implementation of the paper's calculations (Sections 3-5).
# Used by make_placeholder_data.R (calibration) and check_placeholder_data.R
# (public replication check). Base R only, so it runs without the package.
# These functions are the specification to port into CobwebR/R/.

eligible_institution_types <- c(
  "Mutual Funds", "Exchange Traded Funds", "Insurance", "Pension Funds"
)

read_ksei <- function(path) {
  k <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    as.data.frame(readxl::read_excel(path, skip = 4))
  }
  k$PERCENTAGE <- as.numeric(k$PERCENTAGE)
  k
}

# Owner classification (Section 4.1). Without a linkage table, every holder is
# its own component; `links` is reserved for the four linked-holder rules.
classify_holders <- function(ksei, excluded_managers = character(0)) {
  cls <- ifelse(is.na(ksei$INVESTOR_CLASSIFICATION), "", ksei$INVESTOR_CLASSIFICATION)
  foreign <- !is.na(ksei$LOCAL_FOREIGN) & ksei$LOCAL_FOREIGN == "F"
  eligible_type <- cls %in% eligible_institution_types |
    (cls == "Sovereign Wealth Fund" & foreign)
  declared_group <- !is.na(ksei$CONTROLLER_OR_AFFILIATE) & ksei$CONTROLLER_OR_AFFILIATE != ""
  ksei$eligible_type <- eligible_type
  ksei$owner <- !eligible_type | ksei$PERCENTAGE >= 10 | declared_group |
    ksei$INVESTOR_NAME %in% excluded_managers
  ksei
}

# Stock-level measures: combined stake C, holder count H (Section 3.2),
# owner concentration C_own, residual R, KSEI float f_K (eq. sw-freefloat).
stock_measures <- function(ksei) {
  ksei <- classify_holders(ksei)
  by_stock <- split(ksei, ksei$SHARE_CODE)
  out <- do.call(rbind, lapply(by_stock, function(d) {
    p <- d$PERCENTAGE
    data.frame(
      ticker = d$SHARE_CODE[1],
      C = sum(p),
      H = nrow(d),
      C_own = sum(p[d$owner]),
      inst_visible = sum(p[d$eligible_type & !d$owner]),
      inst_owner = sum(p[d$eligible_type & d$owner]),
      f_K = 100 - sum(p[d$owner & p >= 5])
    )
  }))
  out$R <- 100 - out$C_own
  rownames(out) <- NULL
  out
}

# HSC-equivalent zone (eq. sw-zone) and frontier anchors (eq. sw-frontier).
frontier_anchors <- function(hsc) {
  keep <- vapply(seq_len(nrow(hsc)), function(v) {
    !any(hsc$C <= hsc$C[v] & hsc$H >= hsc$H[v] &
           (hsc$C < hsc$C[v] | hsc$H > hsc$H[v]))
  }, logical(1))
  unique(hsc[keep, c("C", "H")])
}

in_hsc_zone <- function(C, H, hsc) {
  mapply(function(c, h) any(hsc$C <= c & hsc$H >= h), C, H)
}

classify_spiderweb <- function(measures, hsc_tickers, H_consolidated = NULL) {
  m <- measures
  m$C <- round(m$C, 2)
  hsc <- m[m$ticker %in% hsc_tickers, ]
  m$H_tilde <- if (is.null(H_consolidated)) m$H else H_consolidated[m$ticker]
  m$group <- ifelse(m$ticker %in% hsc_tickers, "HSC",
             ifelse(in_hsc_zone(m$C, m$H, hsc), "Equivalent",
             ifelse(in_hsc_zone(m$C, m$H_tilde, hsc), "Added", "Outside")))
  m
}

# Green List screens (Section 4, lst-gl-algorithm). Returns one row per stock
# with PASS / FAIL / UNRESOLVED per screen and the sequential first failure.
green_list_screen <- function(measures, market, brokers, events, inst, fund_house,
                              hsc_tickers, assessment_date = as.Date("2026-10-30"),
                              lookback_start = as.Date("2024-11-01"),
                              min_float = 15, max_own = 85, max_top_broker = 30,
                              max_group_broker = 15, min_participation = 10,
                              high_participation = 30, velocity_cut = 2.2,
                              fh_residual = 0.15, fh_inst = 0.40, min_ffmc_bn = 1000) {
  d <- merge(market, measures[, c("ticker", "C_own", "R", "f_K", "inst_owner")],
             by.x = "sw_ticker", by.y = "ticker", all.x = TRUE)
  no_holders <- is.na(d$R)
  d$C_own[no_holders] <- 0; d$R[no_holders] <- 100; d$f_K[no_holders] <- 100
  d$inst_owner[no_holders] <- 0
  d$screenable <- !is.na(d$traded_value_12m_idr_bn) & d$traded_value_12m_idr_bn > 0 &
    !is.na(d$ff_bloomberg_pct)

  # Measures
  d$f <- pmin(d$f_K, d$ff_issuer_pct, d$ff_bloomberg_pct, d$R)
  d$M_f <- d$mcap_idr_bn * d$f / 100
  d$velocity <- d$traded_value_12m_idr_bn / d$M_f

  agg <- stats::aggregate(gross_value_idr_bn ~ sw_ticker, brokers, sum)
  top <- stats::aggregate(gross_value_idr_bn ~ sw_ticker, brokers, max)
  grp <- stats::aggregate(gross_value_idr_bn ~ sw_ticker,
                          transform(brokers, gross_value_idr_bn =
                                      gross_value_idr_bn * is_issuer_group_broker), sum)
  b <- data.frame(sw_ticker = agg$sw_ticker,
                  s_max = 100 * top$gross_value_idr_bn / agg$gross_value_idr_bn,
                  s_grp = 100 * grp$gross_value_idr_bn / agg$gross_value_idr_bn)
  d <- merge(d, b, by = "sw_ticker", all.x = TRUE)

  J_month <- stats::aggregate(holding_pct ~ sw_ticker + month_end, inst, sum)
  J <- stats::aggregate(holding_pct ~ sw_ticker, J_month, mean)
  names(J)[2] <- "J_total"
  d <- merge(d, J, by = "sw_ticker", all.x = TRUE)
  d$J <- pmax(d$J_total - d$inst_owner, 0)
  d$P <- 100 * d$J / d$R
  fh <- stats::aggregate(holding_pct ~ sw_ticker, fund_house, max)
  names(fh)[2] <- "fh_max"
  d <- merge(d, fh, by = "sw_ticker", all.x = TRUE)
  d$fh_max[is.na(d$fh_max)] <- 0

  # Screens
  ev <- events
  ev$announcement_date <- as.Date(ev$announcement_date)
  ev$end_date <- as.Date(ev$end_date)
  active <- ev$event_type %in% c("Special notation", "Suspension") &
    ev$announcement_date <= assessment_date &
    (is.na(ev$end_date) | ev$end_date > assessment_date)
  recent <- ev$event_type %in% c("UMA", "Price-related suspension") &
    ev$announcement_date >= lookback_start & ev$announcement_date <= assessment_date
  flagged <- unique(c(ev$sw_ticker[active | recent], hsc_tickers))
  d$integrity <- !(d$sw_ticker %in% flagged)
  d$free_float <- d$f >= min_float & d$C_own <= max_own
  d$broker <- d$s_max < max_top_broker & d$s_grp <= max_group_broker
  need <- ifelse(d$velocity > velocity_cut, high_participation, min_participation)
  fh_dominant <- d$fh_max > fh_residual * d$R & d$fh_max > fh_inst * d$J
  d$institutional <- d$P >= need & !fh_dominant
  d$size <- d$M_f >= min_ffmc_bn

  screens <- c("integrity", "free_float", "broker", "institutional", "size")
  d$first_fail <- apply(d[, screens], 1, function(x) {
    i <- which(is.na(x) | !x)
    if (length(i)) screens[i[1]] else "none"
  })
  d$status <- ifelse(!d$screenable, "Not screenable",
              ifelse(d$first_fail == "none", "Eligible", "Excluded"))
  d
}

sequential_table <- function(screen) {
  s <- screen[screen$screenable, ]
  stages <- c("integrity", "free_float", "broker", "institutional", "size")
  excluded <- vapply(stages, function(x) sum(s$first_fail == x), integer(1))
  data.frame(stage = c("initial", stages, "liquidity"),
             excluded = c(NA, excluded, 0L),
             remaining = c(nrow(s), nrow(s) - cumsum(excluded), sum(s$status == "Eligible")))
}
