# Public replication check: recompute the paper's numbers from the files in
# inst/extdata only (pseudonymised real data + simulated placeholders).
# Replace any sim_* file with real data of the same schema to run the paper on
# real inputs. Run from the CobwebR/ directory:
#   Rscript data-raw/check_placeholder_data.R

if (!exists("green_list_screen")) source("data-raw/reference_pipeline.R")
ext <- function(f) file.path("inst/extdata", f)
rd <- function(f) utils::read.csv(ext(f), stringsAsFactors = FALSE)

ksei <- read_ksei(ext("ksei_ownership_2026-10-30.csv"))
hsc_tickers <- rd("hsc_list_2026-11-05.csv")$sw_ticker
market <- rd("sim_market_data.csv")
brokers <- rd("sim_broker_summary.csv")
events <- rd("sim_surveillance_events.csv")
inst <- rd("sim_institutional_holdings.csv")
fund_house <- rd("sim_fund_house_holdings.csv")
panel <- rd("top10_panel_2026.csv")

results <- list()
check <- function(what, got, expected, tol = 0) {
  ok <- isTRUE(all.equal(as.numeric(got), as.numeric(expected), tolerance = 0)) ||
    all(abs(as.numeric(got) - as.numeric(expected)) <= tol)
  results[[length(results) + 1]] <<- data.frame(
    check = what, got = paste(round(got, 2), collapse = "/"),
    paper = paste(expected, collapse = "/"), status = if (ok) "OK" else "MISMATCH")
}

# Spider Web module (Section 5.1, tbl-sw-results) ------------------------------
meas <- stock_measures(ksei)
sw <- classify_spiderweb(meas, hsc_tickers)
sw <- merge(sw, market[, c("sw_ticker", "mcap_idr_bn")], by.x = "ticker", by.y = "sw_ticker")
n_grp <- table(factor(sw$group, c("HSC", "Equivalent", "Added")))
cap_grp <- tapply(sw$mcap_idr_bn, factor(sw$group, c("HSC", "Equivalent", "Added")), sum) / 1000
check("HSC designated: stocks", n_grp[["HSC"]], 83)
check("HSC-equivalent, not designated: stocks", n_grp[["Equivalent"]], 331)
check("HSC designated: mcap (IDR tn)", cap_grp[["HSC"]], 3049, tol = 0.5)
check("HSC-equivalent, not designated: mcap (IDR tn)", cap_grp[["Equivalent"]], 1296, tol = 0.5)
anchors <- frontier_anchors(sw[sw$group == "HSC", ])
anchors <- anchors[order(anchors$C), ]
check("Frontier anchors C", anchors$C, c(88.08, 89.35, 91.96, 95.63))
check("Frontier anchors H", anchors$H, c(5, 9, 12, 16))
top_eq <- sort(sw$mcap_idr_bn[sw$group == "Equivalent"], decreasing = TRUE)[1:5] / 1000
check("Five largest undesignated: mcap range (IDR tn)", range(top_eq), c(59, 81), tol = 0.5)

# Frontier stability (Section 5.4.1) --------------------------------------------
inside <- split(panel$sw_ticker[panel$stake_top10_pct >= 92.5],
                panel$period_end[panel$stake_top10_pct >= 92.5])
check("Top-10 >= 92.5%: Feb/Sep/Oct", lengths(inside), c(291, 286, 284))
check("Sep -> Oct exits/entries",
      c(length(setdiff(inside[["2026-09-30"]], inside[["2026-10-30"]])),
        length(setdiff(inside[["2026-10-30"]], inside[["2026-09-30"]]))), c(5, 3))

# Green List (Section 5.2) ------------------------------------------------------
gl <- green_list_screen(meas, market, brokers, events, inst, fund_house, hsc_tickers)
seq_tab <- sequential_table(gl)
check("Sequential: remaining", seq_tab$remaining, c(881, 370, 166, 122, 65, 55, 55))
elig <- gl[gl$status == "Eligible", ]
cap_u <- gl[gl$in_cap_universe, ]
f_all <- with(cap_u, pmin(f_K, ff_issuer_pct, ff_bloomberg_pct, R, na.rm = TRUE))
check("Ownership universe", nrow(gl), 962)
check("Cap universe: stocks", nrow(cap_u), 961)
check("Cap universe: mcap (IDR tn)", sum(cap_u$mcap_idr_bn) / 1000, 10491, tol = 0.5)
check("Cap universe: float cap (IDR tn)", sum(cap_u$mcap_idr_bn * f_all / 100) / 1000, 1945, tol = 0.5)
check("Green List: stocks", nrow(elig), 55)
check("Green List: mcap (IDR tn)", sum(elig$mcap_idr_bn) / 1000, 3415, tol = 0.5)
check("Green List: float cap (IDR tn)", sum(elig$M_f) / 1000, 1204, tol = 0.5)
top <- elig[order(-elig$M_f), ][1:10, ]
check("Top-10 float cap (IDR tn)", round(top$M_f / 1000),
      c(309, 200, 140, 89, 77, 44, 31, 26, 25, 24))
check("Green List and HSC-equivalent overlap",
      sum(elig$sw_ticker %in% sw$ticker[sw$group != "Outside"]), 0)
gl05 <- green_list_screen(meas, market, brokers, events, inst, fund_house, hsc_tickers,
                          min_ffmc_bn = 500)
check("Green List at IDR 0.5 tn", sum(gl05$status == "Eligible"), 60)

results <- do.call(rbind, results)
print(results, row.names = FALSE)
cat(sprintf("\n%d of %d checks match the paper.\n", sum(results$status == "OK"), nrow(results)))
cat("Not reproducible until the linked-holder rules are ported: the 9 stocks added by the\n",
    "spider web assessment (IDR 476 tn), hence the 423 total and IDR 4,822 tn.\n")
