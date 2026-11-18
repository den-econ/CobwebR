# CobwebR bundled data

All files use pseudonymous tickers (`SW001`–`SW966`). The same pseudonyms are
used in the paper. The key that maps them to IDX tickers is held privately by
the authors and is not distributed.

## Real data, pseudonymised

| File | Content | Source |
|:--|:--|:--|
| `ksei_ownership_2026-10-30.csv` | Holders above 1%, 30 Oct 2026, same columns as the IDX download; issuer names replaced | KSEI via IDX |
| `hsc_list_2026-11-05.csv` | Stocks designated High Shareholding Concentration as of 5 Nov 2026 | IDX announcements |
| `top10_panel_2026.csv` | Holder count, combined stake and top-10 stake per stock, Feb/Sep/Oct 2026 | KSEI via IDX |

Investor names are kept because the linked-holder rules need them. Because the
KSEI file is public, a determined reader can re-identify stocks by matching
holdings. The pseudonyms therefore keep the paper readable without naming
firms; they do not make re-identification impossible.

## Simulated placeholders (`sim_*`)

These stand in for licensed or unavailable inputs. **They are synthetic and say
nothing about the real companies.** The values are calibrated so that the
reference pipeline reproduces the paper's published results. To run the paper
on real data, replace a file with your own data in the same columns.

| File | Stands in for | Key columns |
|:--|:--|:--|
| `sim_market_data.csv` | Bloomberg market cap (1 Oct 2026), Bloomberg free float, 12-month one-sided traded value; issuer-reported free float (30 Sep 2026) | `mcap_idr_bn`, `ff_bloomberg_pct`, `ff_issuer_pct`, `traded_value_12m_idr_bn` |
| `sim_broker_summary.csv` | IDX broker summary, gross buy + sell, Oct 2025–Oct 2026 | `broker_code`, `gross_value_idr_bn`, `is_issuer_group_broker` |
| `sim_surveillance_events.csv` | IDX UMA notices, price-related suspensions, special notations, suspensions | `event_type`, `announcement_date`, `end_date` |
| `sim_institutional_holdings.csv` | KSEI holdings by investor type (eligible institutions), Aug–Oct 2026 | `month_end`, `investor_type`, `holding_pct` |
| `sim_fund_house_holdings.csv` | Holdings aggregated by fund house, all funds incl. below 1% | `fund_house`, `holding_pct` |

Conventions: a missing `ff_bloomberg_pct` together with a missing traded value
marks a Bloomberg coverage gap. A traded value of zero marks no trades in the
window. Both put the stock outside the screenable sample.

## Templates (header only)

| File | To be filled with |
|:--|:--|
| `template_holder_links.csv` | Links between holders from the four linked-holder rules (`rule` = i–iv, `status` = confirmed/plausible) |
| `template_manager_exclusion.csv` | Fund managers excluded for fund dissolution or licence revocation |

## What these files reproduce

`data-raw/check_placeholder_data.R` (in the source repository) recomputes the
paper's numbers from these files only. Currently 20 of 20 checks match:
- HSC 83 / 3,049 tn; equivalent 331 / 1,296 tn; frontier anchors
- frontier stability 291/286/284, 5 exits and 3 entries
- Green List sequence 881→370→166→122→65→55; 3,415 tn and 1,204 tn; top-10 float caps; 60 at IDR 0.5 tn
- market totals 10,491 tn and 1,945 tn

The spider web module is reproduced by the package itself (`identify_group_links()`,
`calculate_hsc_frontier()`, see `tests/testthat/test-spiderweb.R` and the
spider web replication guide): 83 / 331 / 9 / 423 stocks, 3,049 / 1,296 / 476 tn,
Stock G 14 → 2 holders and 11.34% free float.

## Spider web review and case files

| File | Content |
|:--|:--|
| `link_review.csv` | Supervisory review decisions that override the mechanical linked-holder rules. Six stocks that the rules add are kept at reported ownership, as in the paper's baseline; reasons pending author confirmation |
| `case_stock_d_2026-09-30.csv` | Stock D holders on 30 Sep 2026 with the paper's anonymised labels and linked groups (the September holder-level register is not bundled) |
