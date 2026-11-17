# One-off: create the PRIVATE key that maps real IDX tickers to the
# pseudonymous tickers (SW001, SW002, ...) used in the paper and in CobwebR.
#
# The key is written to the restricted folder (default: ../data/raw, which is
# gitignored in spiderweb-hsc). Never commit it or ship it with the package.
# No seed is set on purpose: the permutation must not be reproducible from
# this public script.
#
# Run from the CobwebR/ directory:  Rscript data-raw/make_ticker_key.R

restricted_dir <- Sys.getenv("COBWEBR_RESTRICTED_DIR", "../data/raw")
paper_data_dir <- Sys.getenv("COBWEBR_PAPER_DATA_DIR", "../data/spiderweb")
key_file <- file.path(restricted_dir, "ticker_key.csv")
if (file.exists(key_file)) {
  stop("Key already exists at ", key_file,
       ". Regenerating it breaks every published pseudonym; delete it by hand if you really mean to.")
}

source("data-raw/reference_pipeline.R")

ksei <- read_ksei(file.path(restricted_dir, "peng-2026-10-00029-satu-persen.xlsx"))
wb_file <- file.path(restricted_dir, "HSC_frontier_Feb_Sep_Oct_2026.xlsx")
read_sheet <- function(s) {
  x <- as.data.frame(readxl::read_excel(wb_file, s, skip = 7, col_names = FALSE))[, 1:5]
  names(x) <- c("ticker", "issuer", "H", "C_all", "C_top10")
  x$C_all <- 100 * as.numeric(x$C_all)
  x[!is.na(x$ticker), ]
}
panel <- lapply(c(feb = "Feb 2026", sep = "Sep 2026", oct = "Oct 2026"), read_sheet)

issuers <- rbind(
  unique(data.frame(real_ticker = ksei$SHARE_CODE, issuer_name = ksei$ISSUER_NAME)),
  do.call(rbind, lapply(panel, function(x) data.frame(real_ticker = x$ticker, issuer_name = x$issuer))),
  # In the paper's 962-stock ownership universe but without any holder above 1%.
  data.frame(real_ticker = "CNTX", issuer_name = NA)
)
key <- issuers[!duplicated(issuers$real_ticker), ]
key$sw_ticker <- sprintf("SW%03d", sample(nrow(key)))
key$in_ksei_oct2026 <- key$real_ticker %in% ksei$SHARE_CODE

# Reconstruct the IDX HSC list and the paper's spider web groups by matching the
# (C, H) pairs in data/spiderweb/frontier_oct2026.csv to the October KSEI file.
m <- stock_measures(ksei)
m$C <- round(m$C, 2)
fr <- utils::read.csv(file.path(paper_data_dir, "frontier_oct2026.csv"), stringsAsFactors = FALSE)
fr$lab[is.na(fr$lab)] <- ""
fr$key <- paste(fr$c, fr$h)
m$key <- paste(m$C, m$H)

key$spiderweb_group_paper <- NA
key$hsc_match <- NA
key$paper_label <- NA
for (k in unique(fr$key)) {
  rows <- fr[fr$key == k, ]
  tickers <- sort(m$ticker[m$key == k])
  stopifnot(length(tickers) == nrow(rows))
  # When several stocks share (C, H), the paper's group labels cannot tell them
  # apart; HSC goes to the alphabetically first ticker and is flagged for checking.
  rows <- rows[order(rows$g != "HSC", rows$g), ]
  idx <- match(tickers, key$real_ticker)
  key$spiderweb_group_paper[idx] <- rows$g
  key$hsc_match[idx] <- ifelse(rows$g != "HSC", NA,
                               if (length(tickers) == 1) "unique" else "ambiguous: verify against IDX list")
  key$paper_label[idx] <- ifelse(rows$lab == "", NA, paste("Stock", rows$lab))
}
key$spiderweb_group_paper[is.na(key$spiderweb_group_paper) & key$in_ksei_oct2026] <- "Below 80%"

# Stock D and Stock V are identified from the September panel (Section 5.1.2).
sep <- panel$sep
sep_key <- paste(round(sep$C_all, 2), sep$H)
d_tk <- sep$ticker[sep_key == "91.92 11"]
v_tk <- sep$ticker[sep_key == "91.33 11"]
if (length(d_tk) == 1) key$paper_label[key$real_ticker == d_tk] <- "Stock D"
if (length(v_tk) == 1) key$paper_label[key$real_ticker == v_tk] <- "Stock V"

key$hsc_designated_2026_11_05 <- key$spiderweb_group_paper %in% "HSC"
key <- key[order(key$sw_ticker), c("sw_ticker", "real_ticker", "issuer_name", "in_ksei_oct2026",
                                   "hsc_designated_2026_11_05", "hsc_match",
                                   "spiderweb_group_paper", "paper_label")]
utils::write.csv(key, key_file, row.names = FALSE, na = "")
message("Wrote ", nrow(key), " rows to ", key_file)
print(table(key$spiderweb_group_paper, useNA = "ifany"))
print(key[!is.na(key$paper_label) | grepl("ambiguous", key$hsc_match), c("sw_ticker", "real_ticker", "spiderweb_group_paper", "hsc_match", "paper_label")])
