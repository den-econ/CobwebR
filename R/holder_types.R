legal_form_tokens <- c(
  "PT", "TBK", "LTD", "LIMITED", "INC", "CORP", "CORPORATION", "PTE", "CO",
  "SA", "AG", "LLC", "PLC", "BHD", "SDN", "CV"
)

# Words too common in Indonesian and offshore entity names to identify a group.
generic_name_tokens <- c(
  "THE", "OF", "AND", "DAN", "INDONESIA", "INDONESIAN", "INTERNATIONAL",
  "INTERNASIONAL", "GLOBAL", "HOLDING", "HOLDINGS", "INVESTMENT", "INVESTMENTS",
  "INVESTAMA", "INVESTASI", "GROUP", "ASIA", "PACIFIC", "FUND", "FUNDS", "REKSA",
  "DANA", "SAHAM", "EQUITY", "TRUST", "BANK", "PERSADA", "SEJAHTERA", "MANDIRI",
  "UTAMA", "ABADI", "JAYA", "MAKMUR", "LESTARI", "SENTOSA", "PRIMA", "MITRA",
  "KARYA", "NUSANTARA", "CEMERLANG", "ENERGI", "RESOURCES", "PERKASA", "SUKSES",
  "MULIA", "AGUNG", "INDAH", "MAS", "SINAR", "MANAGEMENT", "ASSET", "ASSETS",
  "VENTURES", "VENTURE", "PARTNERS", "ENTERPRISE", "ENTERPRISES", "DEVELOPMENT",
  "INDUSTRI", "INDUSTRIES", "SECURITIES", "SEKURITAS", "FINANCE", "FINANCIAL",
  "LIFE", "INSURANCE", "SINGAPORE", "HONG", "KONG", "PERUSAHAAN", "PERSEROAN",
  "PERSERO", "PEMDA", "PROVINSI", "KABUPATEN", "KOTA", "YAYASAN", "KOPERASI",
  "KARYAWAN", "NEGARA", "REPUBLIK"
)

nominee_classes <- c(
  "Securities Company", "Private Bank", "Bank", "Trustee Bank",
  "Brokerage Firms", "Investment Fund Selling Agent"
)
nominee_pattern <- paste0(
  "\\b(SEKURITAS|SECURITIES|NOMINEE|NOMINEES|CUSTODY|CUSTODIAN|JPMCB|HSBC|",
  "CITIBANK|UBS AG|DBS BANK|JULIUS BAER|PICTET|LGT|NOMURA|UOB KAY HIAN|",
  "MORGAN STANLEY|GOLDMAN|CREDIT SUISSE|BNP PARIBAS|STANDARD CHARTERED|",
  "S A|A C|REGISTRAR)\\b"
)
eligible_institution_classes <- c(
  "Mutual Funds", "Exchange Traded Funds", "Insurance", "Pension Funds"
)
public_sector_classes <- c("Government", "State Owned Enterprises", "State Owned Company")
offshore_pattern <- paste0(
  "VIRGIN|CAYMAN|MARSHALL|SEYCHELLES|SAMOA|PANAMA|BERMUDA|BAHAMAS|MAURITIUS|",
  "LABUAN|BRITISH INDIAN|ANGUILLA|BELIZE|GUERNSEY|JERSEY|ISLE OF MAN|",
  "ARAB EMIRATES|HONG KONG|SINGAPORE|MALAYSIA|LUXEMBOURG|CYPRUS|",
  "LIECHTENSTEIN|VANUATU|COOK"
)

#' Normalise registered holder names
#'
#' Upper-cases, strips punctuation and drops legal-form words (PT, Tbk, Ltd,
#' Pte, ...), so that spelling variants of one registered holder across KSEI
#' filings map to one key.
#'
#' @param x Character vector of holder names.
#' @return Character vector of normalised names.
#' @export
#' @examples
#' normalise_holder(c("PT. Sejahtera Mahkota Persada", "PT SEJAHTERA MAHKOTA PERSADA"))
normalise_holder <- function(x) {
  x <- gsub("[^A-Z0-9 ]", " ", toupper(x))
  vapply(strsplit(x, " +"), function(w) {
    paste(w[nzchar(w) & !w %in% legal_form_tokens], collapse = " ")
  }, character(1))
}

distinctive_tokens <- function(name) {
  w <- strsplit(name, " ", fixed = TRUE)[[1]]
  unique(w[nchar(w) >= 3 & !grepl("^[0-9]+$", w) & !w %in% generic_name_tokens])
}

# The group brand: the first distinctive word of a name (MNC, CAPITAL, MEDCO).
brand_token <- function(name) {
  vapply(name, function(x) {
    t <- distinctive_tokens(x)
    if (length(t)) t[1] else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Classify holders for the linked-holder rules
#'
#' Adds the holder types used by [identify_group_links()]: nominee accounts
#' (never linked automatically), eligible institutions, individuals, public
#' sector holders, declared controllers and single-stock offshore vehicles.
#'
#' @param ksei_df Register from [load_ksei_register()].
#' @return `ksei_df` with columns `holder`, `n_stocks`, `is_nominee`,
#'   `is_institution`, `is_individual`, `is_public_sector`, `is_controller`,
#'   `is_offshore_vehicle`.
#' @export
classify_holders <- function(ksei_df) {
  d <- as.data.frame(ksei_df, stringsAsFactors = FALSE)
  cls <- ifelse(is.na(d$investor_class), "", d$investor_class)
  d$holder <- normalise_holder(d$investor_name)
  d$n_stocks <- as.vector(tapply(d$ticker, d$holder, function(t) length(unique(t)))[d$holder])
  d$is_nominee <- cls %in% nominee_classes | grepl(nominee_pattern, d$holder)
  d$is_institution <- cls %in% eligible_institution_classes |
    (cls == "Sovereign Wealth Fund" & d$local_foreign %in% "F")
  d$is_individual <- cls == "Individual"
  d$is_public_sector <- cls %in% public_sector_classes
  d$is_controller <- d$controller_affiliate %in% "CONTROLLER"
  foreign <- d$local_foreign %in% "F" |
    (is.na(d$local_foreign) & !grepl("\\bPT\\b", toupper(d$investor_name)))
  offshore <- is.na(d$domicile) | grepl(offshore_pattern, toupper(d$domicile))
  d$is_offshore_vehicle <- foreign & offshore & !d$is_nominee & !d$is_institution &
    !d$is_individual & !d$is_public_sector
  d
}
