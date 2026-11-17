# Build the pseudonymised and simulated datasets shipped in inst/extdata/.
#
# Inputs (restricted, never shipped): the private ticker key, the raw October
# KSEI >1% file and the Feb/Sep/Oct top-10 workbook, all in COBWEBR_RESTRICTED_DIR.
#
# Real (public) data are pseudonymised: KSEI ownership, the IDX HSC list, the
# Feb/Sep/Oct top-10 panel. Licensed or unavailable data (Bloomberg market cap,
# float and traded value; issuer float; IDX broker summary; surveillance events;
# KSEI holdings by investor type; fund-house holdings) are SIMULATED, calibrated
# so that the reference pipeline reproduces the paper's published numbers.
# Simulated values carry no information about the real companies.
#
# Run from the CobwebR/ directory:  Rscript data-raw/make_placeholder_data.R

restricted_dir <- Sys.getenv("COBWEBR_RESTRICTED_DIR", "../data/raw")
out_dir <- "inst/extdata"
source("data-raw/reference_pipeline.R")

key <- utils::read.csv(file.path(restricted_dir, "ticker_key.csv"), stringsAsFactors = FALSE)
to_sw <- stats::setNames(key$sw_ticker, key$real_ticker)
key <- key[order(key$sw_ticker), ]  # all random draws follow pseudonym order
set.seed(20261030)

# 1. Pseudonymised KSEI ownership -------------------------------------------
ksei <- read_ksei(file.path(restricted_dir, "peng-2026-10-00029-satu-persen.xlsx"))
ksei$SHARE_CODE <- unname(to_sw[ksei$SHARE_CODE])
ksei$ISSUER_NAME <- paste("Issuer", ksei$SHARE_CODE)
ksei$DATE <- format(as.Date(ksei$DATE))
ksei <- ksei[order(ksei$SHARE_CODE, -ksei$PERCENTAGE), ]
utils::write.csv(ksei, file.path(out_dir, "ksei_ownership_2026-10-30.csv"), row.names = FALSE, na = "")

# 2. IDX HSC list -------------------------------------------------------------
hsc_tickers <- key$sw_ticker[key$hsc_designated_2026_11_05]
utils::write.csv(data.frame(sw_ticker = sort(hsc_tickers), status = "Designated",
                            as_of_date = "2026-11-05", source = "IDX announcements"),
                 file.path(out_dir, "hsc_list_2026-11-05.csv"), row.names = FALSE)

# 3. Feb/Sep/Oct top-10 panel (Section 5.4.1) ----------------------------------
wb_file <- file.path(restricted_dir, "HSC_frontier_Feb_Sep_Oct_2026.xlsx")
sheets <- c("2026-02-27" = "Feb 2026", "2026-09-30" = "Sep 2026", "2026-10-30" = "Oct 2026")
panel <- do.call(rbind, lapply(names(sheets), function(dt) {
  x <- as.data.frame(readxl::read_excel(wb_file, sheets[[dt]], skip = 7, col_names = FALSE))[, 1:5]
  x <- x[!is.na(x[[1]]), ]
  data.frame(sw_ticker = unname(to_sw[x[[1]]]), period_end = dt, holders_gt1 = as.integer(x[[3]]),
             stake_all_gt1_pct = round(100 * as.numeric(x[[4]]), 4),
             stake_top10_pct = round(100 * as.numeric(x[[5]]), 4))
}))
utils::write.csv(panel[order(panel$period_end, panel$sw_ticker), ],
                 file.path(out_dir, "top10_panel_2026.csv"), row.names = FALSE)

# 4. Stock universe and real ownership measures ------------------------------
meas <- stock_measures(ksei)
u <- merge(key[, c("sw_ticker", "real_ticker", "spiderweb_group_paper")],
           meas, by.x = "sw_ticker", by.y = "ticker", all.x = TRUE)
u <- u[u$sw_ticker %in% meas$ticker | u$real_ticker == "CNTX", ]
u <- u[order(u$sw_ticker), ]
cntx <- u$real_ticker == "CNTX"
u$C_own[cntx] <- 0; u$R[cntx] <- 100; u$f_K[cntx] <- 100
u$inst_visible[cntx] <- 0; u$inst_owner[cntx] <- 0
u$grp <- u$spiderweb_group_paper
u$grp[cntx] <- "Below 80%"
n <- nrow(u)  # 962: the paper's ownership universe
stopifnot(n == 962)

# Paper Table tbl-glr-top10: published float capitalization (IDR bn).
top10 <- c(BBCA = 309, BBRI = 200, BMRI = 140, TLKM = 89, ASII = 77,
           BBNI = 44, UNTR = 31, INDF = 26, ANTM = 25, MDKA = 24) * 1000
is_top10 <- u$real_ticker %in% names(top10)
equiv <- u$grp %in% c("Equivalent", "Added")
is_hsc <- u$grp == "HSC"

pick <- function(pool, size, weight = NULL) {
  if (length(pool) < size) stop("pool too small")
  if (length(pool) == size) return(pool)
  sample(pool, size, prob = weight)
}

# 5. Assign each stock the stage at which it leaves the Green List -----------
fate <- rep(NA_character_, n)
idx <- seq_len(n)
free <- function() idx[is.na(fate)]

# Outside the screenable sample: 53 no trades, 28 Bloomberg gaps (Section 5.2.1).
pool <- free()[!is_hsc[free()] & !is_top10[free()] & !cntx[free()] & u$grp[free()] != "Added"]
fate[pick(pool, 53)] <- "no_trades"
pool <- free()[!is_hsc[free()] & !is_top10[free()] & !cntx[free()] & u$grp[free()] != "Added"]
fate[pick(pool, 28)] <- "bloomberg_gap"

# Integrity: every HSC stock, CNTX, and 427 others, tilted toward low residuals.
fate[is_hsc | cntx] <- "integrity"
pool <- free()[!is_top10[free()]]
w <- ifelse(u$R[pool] < 15, 4, ifelse(equiv[pool], 3, 1))
fate[pick(pool, 511 - sum(fate == "integrity", na.rm = TRUE), w)] <- "integrity"

# Free float: every remaining stock with R < 15 or C_own > 85, topped up to 204.
forced <- free()[u$R[free()] < 15 | u$C_own[free()] > 85]
stopifnot(length(forced) <= 204)
fate[forced] <- "free_float"
pool <- free()[!is_top10[free()]]
w <- ifelse(equiv[pool], 5, 1)  # HSC-equivalent stocks stay off the Green List
fate[pick(pool, 204 - length(forced), w)] <- "free_float"

pool <- free()[!is_top10[free()]]
fate[pool[equiv[pool]]] <- "broker"
fate[pick(free()[!is_top10[free()]], 44 - sum(fate == "broker", na.rm = TRUE))] <- "broker"
fate[pick(free()[!is_top10[free()]], 57)] <- "institutional"
size_fail <- pick(free()[!is_top10[free()]], 10)
fate[size_fail] <- "size"
fate[is.na(fate)] <- "eligible"
stopifnot(sum(fate == "eligible") == 55, all(fate[is_top10] == "eligible"))
passes_ff <- fate %in% c("broker", "institutional", "size", "eligible")

# 6. Float: reconciled f = min(f_K, f_E, f_B, R) ------------------------------
# Issuer-reported float tends to exceed the KSEI residual (Stock A, G examples).
f_target <- rep(NA_real_, n)
ff_fail_hiR <- fate == "free_float" & u$R >= 15 & u$C_own <= 85
f_target[ff_fail_hiR] <- runif(sum(ff_fail_hiR), 4, 14)
size_small <- size_fail[1:5]          # M_f in [0.5, 1) tn: the +5 in tbl-greenlist-sensitivity
size_tiny  <- size_fail[6:10]         # M_f < 0.5 tn
Mf <- rep(NA_real_, n)
Mf[size_small] <- runif(5, 560, 940)
Mf[size_tiny]  <- runif(5, 120, 440)

# Green List: top-10 published values; 45 others sum to 1,204 - 965 tn.
gl <- which(fate == "eligible")
gl_other <- gl[!is_top10[gl]]
Mf[gl[is_top10[gl]]] <- top10[u$real_ticker[gl[is_top10[gl]]]]
x <- rlnorm(45, 0, 0.9)
x <- 1100 + (x / sum(x)) * (239000 - 45 * 1100)
while (any(x > 23500)) {  # keep below MDKA's 24 tn so the top-10 order holds
  excess <- sum(pmax(x - 23500, 0)); x <- pmin(x, 23500)
  room <- x < 23500; x[room] <- x[room] + excess * x[room] / sum(x[room])
}
Mf[gl_other] <- x
f_target[gl[is_top10[gl]]] <- pmin(u$R, u$f_K)[gl[is_top10[gl]]]
# Choose the 45 floats so total Green List capitalization is IDR 3,415 tn.
top10_M <- sum(Mf[gl[is_top10[gl]]] / (f_target[gl[is_top10[gl]]] / 100))
lo <- 15.5
hi <- pmin(u$R, u$f_K)[gl_other]
gl_M <- function(lambda) sum(Mf[gl_other] / ((lo + lambda * (hi - lo)) / 100))
lambda <- uniroot(function(l) gl_M(l) - (3415000 - top10_M), c(0, 1))$root
f_target[gl_other] <- lo + lambda * (hi - lo)

# Size-fail and other F-passers keep a float of at least 15.
others_pass <- which(passes_ff & fate != "eligible")
f_target[others_pass] <- 15 + runif(length(others_pass), 0.1, 1) * (pmin(u$R, u$f_K)[others_pass] - 15)

# 7. Market capitalization (IDR bn) -------------------------------------------
M <- rep(NA_real_, n)
fixed <- !is.na(Mf)
M[fixed] <- Mf[fixed] / (f_target[fixed] / 100)
scale_to <- function(ids, total, cap = Inf, tilt = 1) {
  v <- rlnorm(length(ids), 0, 1.4) * tilt
  v <- v / sum(v) * total
  for (i in 1:50) {
    if (!any(v > cap)) break
    excess <- sum(pmax(v - cap, 0)); v <- pmin(v, cap)
    room <- v < cap; v[room] <- v[room] + excess * v[room] / sum(v[room])
  }
  v
}
M[is_hsc] <- scale_to(which(is_hsc), 3049000)
eq_ids <- which(u$grp == "Equivalent")
top5 <- sample(eq_ids, 5)
M[top5] <- c(81000, 75000, 70000, 64000, 59000)
M[setdiff(eq_ids, top5)] <- scale_to(setdiff(eq_ids, top5), 1296000 - 349000, cap = 55000)
M[u$grp == "Added"] <- scale_to(which(u$grp == "Added"), 476000, cap = 57000)
rest <- which(is.na(M) & !cntx)
# Larger firms tend to be more dispersed: tilt size toward higher residual float.
M[rest] <- scale_to(rest, 10491000 - sum(M[!cntx], na.rm = TRUE), cap = 50000,
                    tilt = (pmin(u$R, u$f_K)[rest] / 20)^2)
M[cntx] <- 150

# 8. Bloomberg and issuer float -----------------------------------------------
# Remaining stocks: f_B = mu * min(R, f_K), mu chosen so market float cap is 1,945 tn.
cap_univ <- !cntx
free_f <- is.na(f_target)
base <- pmin(u$R, u$f_K)
float_cap <- function(mu) {
  f <- f_target; f[free_f] <- mu * base[free_f]
  sum(M[cap_univ] * f[cap_univ] / 100)
}
mu <- uniroot(function(m) float_cap(m) - 1945000, c(0.05, 1))$root
f_target[free_f] <- mu * base[free_f]

gap <- fate == "bloomberg_gap"
ff_bloomberg <- ifelse(gap, NA, f_target)
# Bloomberg gaps: issuer float carries the reconciliation instead.
ff_issuer <- ifelse(gap, f_target, pmin(100, base + runif(n, 3, 30)))
ff_issuer[cntx] <- 100

# 9. Traded value (12 months, one-sided, IDR bn) ------------------------------
Mf_all <- M * f_target / 100
velocity <- rlnorm(n, log(0.5), 1)
velocity[abs(velocity - 2.2) < 0.15] <- 1.9  # keep clear of the 2.2 cut
inst_fail_route <- rep(NA_character_, n)
inst_ids <- which(fate == "institutional")
traded <- velocity * Mf_all
traded[fate == "no_trades"] <- 0
traded[gap] <- NA

# 10. Institutional holdings: P = 100 J / R ----------------------------------
P_lo <- 100 * u$inst_visible / u$R + 0.5
P <- pmin(runif(n, 2, 60), 95)
elig_ids <- which(fate == "eligible")
for (i in elig_ids) {
  need <- if (velocity[i] > 2.2) 30 else 10
  P[i] <- max(P_lo[i], runif(1, need + 2, need + 40))
}
for (i in inst_ids) {
  if (P_lo[i] < 8) {                   # too few institutions
    inst_fail_route[i] <- "low_participation"
    velocity[i] <- min(velocity[i], 2.0)
    P[i] <- runif(1, P_lo[i], 8)
  } else if (P_lo[i] < 27) {           # high velocity raises the floor to 30
    inst_fail_route[i] <- "high_velocity"
    velocity[i] <- runif(1, 2.5, 5)
    P[i] <- runif(1, max(P_lo[i], 12), 27)
  } else {                             # one fund house dominates
    inst_fail_route[i] <- "fund_house"
    P[i] <- max(P_lo[i], runif(1, 20, 60))
  }
}
traded[inst_ids] <- velocity[inst_ids] * Mf_all[inst_ids]
P <- pmax(P, P_lo)
J <- P / 100 * u$R
J_total <- J + u$inst_owner

months <- c("2026-08-31", "2026-09-30", "2026-10-31")
types <- c("Mutual Funds", "Exchange Traded Funds", "Insurance", "Pension Funds", "Sovereign Wealth Fund")
inst <- do.call(rbind, lapply(which(!is.na(u$R)), function(i) {
  drift <- c(runif(2, 0.94, 1.06)); drift <- c(drift, 3 - sum(drift))  # mean preserved
  w <- rgamma(5, c(3, 0.5, 1.5, 1, 0.7)); w <- w / sum(w)
  data.frame(sw_ticker = u$sw_ticker[i], month_end = rep(months, each = 5),
             investor_type = types, holding_pct = round(rep(J_total[i] * drift, each = 5) * w, 6))
}))

fh <- do.call(rbind, lapply(which(!is.na(u$R)), function(i) {
  k <- sample(2:6, 1)
  if (!is.na(inst_fail_route[i]) && inst_fail_route[i] == "fund_house") {
    big <- min(J[i] * 0.95, max(0.15 * u$R[i], 0.40 * J[i]) * runif(1, 1.15, 1.4))
    rest_h <- runif(k - 1); rest_h <- rest_h / sum(rest_h) * (J[i] - big) * 0.6
    h <- c(big, rest_h)
  } else {
    h <- rgamma(k, 1); h <- h / sum(h) * J[i] * runif(1, 0.4, 0.8)
    h <- pmin(h, 0.35 * J[i])
  }
  data.frame(sw_ticker = u$sw_ticker[i], fund_house = sprintf("FH%02d", sample(40, k)),
             holding_pct = round(h, 6))
}))

# 11. Broker summary (gross buy + sell, Oct 2025 - Oct 2026) -----------------
broker_fail <- which(fate == "broker")
brokers <- do.call(rbind, lapply(which(fate != "no_trades" & !cntx | cntx), function(i) {
  k <- sample(12:30, 1)
  fail <- i %in% broker_fail || (fate[i] %in% c("integrity", "free_float") && runif(1) < 0.25)
  route_grp <- fail && runif(1) < 0.35
  s1 <- if (fail && !route_grp) runif(1, 32, 65) else runif(1, max(8, 130 / k), 26)
  others <- rgamma(k - 1, 1); others <- others / sum(others) * (100 - s1)
  while (any(others >= s1)) {
    ex <- sum(pmax(others - 0.95 * s1, 0)); others <- pmin(others, 0.95 * s1)
    others <- others + ex / length(others)
  }
  s <- c(s1, others)
  grp <- rep(FALSE, k)
  if (route_grp) {                      # issuer-group brokers above 15% combined
    j <- 2:3; grp[j] <- TRUE
    need <- runif(1, 17, 25)
    s[j] <- need / 2; s[-c(1, j)] <- s[-c(1, j)] / sum(s[-c(1, j)]) * (100 - s1 - need)
  } else if (runif(1) < 0.3) {
    grp[which.min(s)] <- TRUE                      # a small group broker, below the limit
  }
  tv <- if (is.na(traded[i])) 50 else max(traded[i], 1)
  data.frame(sw_ticker = u$sw_ticker[i], broker_code = sprintf("BR%02d", sample(60, k)),
             gross_value_idr_bn = round(2 * tv * 13 / 12 * s / 100, 4),
             is_issuer_group_broker = grp)
}))

# 12. Surveillance events -----------------------------------------------------
rdate <- function(a, b) as.Date(a) + sample(0:as.integer(as.Date(b) - as.Date(a)), 1)
ev_list <- list()
for (i in which(fate == "integrity" & !is_hsc)) {
  type <- sample(c("UMA", "Price-related suspension", "Special notation", "Suspension"), 1,
                 prob = c(0.45, 0.25, 0.22, 0.08))
  if (type %in% c("UMA", "Price-related suspension")) {
    a <- rdate("2024-11-01", "2026-10-30")
    e <- if (type == "UMA") NA else a + sample(1:10, 1)
  } else {
    a <- rdate("2024-06-01", "2026-10-30"); e <- NA
  }
  ev_list[[length(ev_list) + 1]] <- data.frame(sw_ticker = u$sw_ticker[i], event_type = type,
    announcement_date = a, effective_date = a, end_date = e)
}
# Some HSC stocks also have UMA history; some eligible stocks have events that
# fall before the 24-month lookback, so they must not be excluded.
for (i in sample(which(is_hsc), 40)) {
  a <- rdate("2024-11-01", "2026-10-30")
  ev_list[[length(ev_list) + 1]] <- data.frame(sw_ticker = u$sw_ticker[i], event_type = "UMA",
    announcement_date = a, effective_date = a, end_date = NA)
}
for (i in sample(which(fate %in% c("eligible", "broker", "institutional", "size")), 15)) {
  a <- rdate("2022-01-01", "2024-10-25")
  type <- sample(c("UMA", "Price-related suspension"), 1)
  ev_list[[length(ev_list) + 1]] <- data.frame(sw_ticker = u$sw_ticker[i], event_type = type,
    announcement_date = a, effective_date = a, end_date = if (type == "UMA") NA else a + 3)
}
events <- do.call(rbind, ev_list)
events <- events[order(events$sw_ticker, events$announcement_date), ]

# 13. Write simulated files -----------------------------------------------------
market <- data.frame(
  sw_ticker = u$sw_ticker,
  mcap_idr_bn = round(M, 3), mcap_date = "2026-10-01",
  ff_bloomberg_pct = round(ff_bloomberg, 4),
  ff_issuer_pct = round(ff_issuer, 4), ff_issuer_date = "2026-09-30",
  traded_value_12m_idr_bn = round(traded, 4),
  traded_window = "2025-11-01/2026-10-31",
  in_cap_universe = !cntx,
  simulated = TRUE
)
w <- function(x, f) utils::write.csv(x, file.path(out_dir, f), row.names = FALSE, na = "")
w(market, "sim_market_data.csv")
w(transform(brokers, simulated = TRUE), "sim_broker_summary.csv")
w(transform(events, simulated = TRUE), "sim_surveillance_events.csv")
w(transform(inst, simulated = TRUE), "sim_institutional_holdings.csv")
w(transform(fh, simulated = TRUE), "sim_fund_house_holdings.csv")
w(data.frame(investor_name_1 = character(), investor_name_2 = character(),
             rule = character(), evidence = character(), status = character()),
  "template_holder_links.csv")
w(data.frame(manager_name = character(), reason = character(), effective_date = character()),
  "template_manager_exclusion.csv")

# 14. Verify against the paper, including the restricted-only checks ---------
source("data-raw/check_placeholder_data.R")
added <- u$sw_ticker[u$grp == "Added"]
cat(sprintf("Added (needs linkage; checked via key only): %d stocks, IDR %.0f tn\n",
            length(added), sum(market$mcap_idr_bn[market$sw_ticker %in% added]) / 1000))
