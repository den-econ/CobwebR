toy_register <- function() {
  row <- function(t, inv, pct, cls = "Corporate", ctrl = NA, lf = "L", dom = "INDONESIA") {
    data.frame(ticker = t, investor_name = inv, investor_class = cls,
               controller_affiliate = ctrl, local_foreign = lf, domicile = dom,
               percentage = pct, stringsAsFactors = FALSE)
  }
  rbind(
    row("AAA", "PT Alpha Group", 60, ctrl = "CONTROLLER"),
    row("AAA", "Web One Ltd", 4, lf = "F", dom = "VIRGIN ISLANDS, BRITISH"),
    row("AAA", "Web Two Ltd", 3, lf = "F", dom = "VIRGIN ISLANDS, BRITISH"),
    row("AAA", "PT Alpha Kapital", 2),
    row("AAA", "Custody Bank", 6, cls = "Bank"),
    row("BBB", "Web One Ltd", 2, lf = "F", dom = "VIRGIN ISLANDS, BRITISH"),
    row("BBB", "Web Two Ltd", 2, lf = "F", dom = "VIRGIN ISLANDS, BRITISH"),
    row("BBB", "PT Beta", 90, ctrl = "CONTROLLER"),
    row("CCC", "Ann", 3, cls = "Individual"),
    row("CCC", "Bob", 3, cls = "Individual"),
    row("DDD", "Ann", 3, cls = "Individual"),
    row("DDD", "Bob", 3, cls = "Individual")
  )
}

test_that("rule (i) links co-holders in two stocks but not pairs of individuals", {
  L <- identify_group_links(toy_register(), rules = "i")
  e <- L$edges
  expect_true(any(e$holder_a == "WEB ONE" & e$holder_b == "WEB TWO"))
  expect_false(any(e$holder_a == "ANN" & e$holder_b == "BOB"))
  L2 <- identify_group_links(toy_register(), rules = "i", link_individual_pairs = TRUE)
  expect_true(any(L2$edges$holder_a == "ANN" & L2$edges$holder_b == "BOB"))
})

test_that("rule (ii) attaches holders carrying the controller's group name", {
  L <- identify_group_links(toy_register(), rules = c("i", "ii"))
  e <- L$edges[L$edges$rule == "ii", ]
  expect_equal(e$holder_a, "ALPHA KAPITAL")
  expect_equal(e$holder_b, "ALPHA GROUP")
})

test_that("nominee accounts are never linked", {
  L <- identify_group_links(toy_register())
  expect_false(any(c(L$edges$holder_a, L$edges$holder_b) == "CUSTODY BANK"))
})

test_that("consolidation counts linked holders once and adds them before the 5% cut", {
  s <- consolidate_holders(identify_group_links(toy_register(), rules = "i"))
  a <- s[s$ticker == "AAA", ]
  expect_equal(c(a$H, a$H_tilde), c(5, 4))
  expect_equal(a$f_K_reported, 100 - 60 - 6)
  expect_equal(a$f_K, 100 - 60 - 6 - 7)
})

test_that("review decisions keep a stock at reported ownership", {
  rv <- data.frame(sw_ticker = "AAA", holder_a = NA, holder_b = NA,
                   decision = "exclude_stock", reason = "test")
  s <- consolidate_holders(identify_group_links(toy_register(), rules = "i", review = rv))
  a <- s[s$ticker == "AAA", ]
  expect_equal(a$H_tilde, a$H)
  expect_true(a$reviewed)
})

test_that("frontier anchors and zone follow the dominance rule", {
  hsc <- data.frame(ticker = c("P", "Q", "R"), C = c(88, 90, 92), H = c(5, 9, 6))
  expect_equal(frontier_anchors(hsc)$ticker, c("P", "Q"))
  expect_equal(frontier_holders(c(87, 89, 95), hsc), c(-Inf, 5, 9))
})

test_that("bundled data reproduce the paper's spider web results", {
  ext <- function(f) system.file("extdata", f, package = "CobwebR")
  ksei <- load_ksei_register(ext("ksei_ownership_2026-10-30.csv"))
  hsc <- read.csv(ext("hsc_list_2026-11-05.csv"))
  market <- read.csv(ext("sim_market_data.csv"))
  links <- identify_group_links(ksei, review = read_link_review())
  fr <- calculate_hsc_frontier(ksei, hsc, links = links)
  tab <- summary(fr, market = market)
  expect_equal(tab$stocks, c(83, 331, 9, 423))
  expect_equal(round(tab$mcap_idr_tn[1:3]), c(3049, 1296, 476))
  expect_equal(fr$anchors$C, c(88.08, 89.35, 91.96, 95.63))
  expect_equal(fr$anchors$H, c(5, 9, 12, 16))
  g <- fr$stocks[fr$stocks$ticker == "SW924", ]
  expect_equal(c(g$H, g$H_tilde), c(14, 2))
  expect_equal(round(g$f_K, 2), 11.34)
  expect_equal(frontier_sensitivity(calculate_hsc_frontier(ksei, hsc), c(-1, 0, 1))$n,
               c(351, 331, 295))
  st <- frontier_stability(read.csv(ext("top10_panel_2026.csv")))
  expect_equal(st$inside, c(291, 286, 284))
  expect_equal(c(st$entries[3], st$exits[3]), c(3, 5))
})
