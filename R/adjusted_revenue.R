#' Fill missing commercial revenue with dealer average prices
#'
#' Temporary patch for missing `SPPVALUE` in the commercial (comlandr) pull.
#' From 2020 onward many landings records (e.g. blue crab, American eel,
#' bluefin tuna, spot) have landed weight but no value in the database.
#' This function fills those values using monthly average dealer prices per
#' pound (`NEFSC_GARFO.CFDRS_AVG_PRICE_TST`, joined to NESPP3 codes through
#' `CAMS_GARFO.CFG_ITIS`), converted to price per metric ton and deflated to
#' the same reference year/month as the pull with the deflator series shipped
#' in comlandr (`extdata/fred/fred.rds`).
#'
#' Based on the adjustment applied by A. Beet for the 2026 report
#' (`commercial_bennet_sppvalue_fixed.rds`). Remove once the source data
#' are fixed; see [missing_revenue_check()].
#'
#' @param comland_data List. One element of [get_commercial_data()] output
#'   (`$comdat` or `$bennet`); must contain a `comland` data frame with
#'   `YEAR`, `MONTH`, `NESPP3`, `SPPLIVMT` and `SPPVALUE`.
#' @param channel An object inherited from DBIConnection-class.
#' @param ref_year Integer. Reference year for dollar values. Should match the
#'   pull (`get_commercial_data()` uses end year - 1).
#' @param ref_month Integer. Reference month for dollar values (default 1).
#' @param start_year Integer. First year to fill (default 2020).
#' @param drop_unpriced Logical. If `TRUE`, drop records whose value is still
#'   missing because no monthly price exists (this removes their landings too;
#'   the 2026-report adjustment did this). Default `FALSE` keeps the landings
#'   with missing value.
#'
#' @return list. `comland_data` with:
#' \item{comland}{landings data with missing `SPPVALUE` filled (a data.table, as required by [create_bennet()])}
#' \item{revenue_adjustment}{one-row summary of what was filled: records and tons missing, records filled, value added, tons still unpriced, and the settings used}
#'
#' @section Data sources:
#' * `NEFSC_GARFO.CFDRS_AVG_PRICE_TST`: monthly average dealer price per pound by ITIS
#' * `CAMS_GARFO.CFG_ITIS`: ITIS to NESPP3 lookup
#' * comlandr `extdata/fred/fred.rds`: deflator series used by comlandr
#'
#' @examples
#' \dontrun{
#' channel <- dbutils::connect_to_database("server", "user")
#' raw <- get_commercial_data(channel)
#' raw$comdat <- adjusted_revenue(raw$comdat, channel, ref_year = 2025)
#' }
#'
#' @importFrom dplyr all_of arrange distinct filter group_by if_else left_join mutate n pull row_number select summarise transmute
#' @importFrom tibble as_tibble
#' @importFrom data.table as.data.table
#' @importFrom DBI dbGetQuery
#'
#' @export
#'

adjusted_revenue <- function(
    comland_data,
    channel,
    ref_year,
    ref_month = 1,
    start_year = 2020,
    drop_unpriced = FALSE
) {
  lbstomt <- 0.000453592 # metric tons per pound
  
  # 1. Price per metric ton by year, month and NESPP3 ----
  # monthly average price per pound by ITIS, mapped to NESPP3
  prices <- DBI::dbGetQuery(
    channel,
    "select * from NEFSC_GARFO.CFDRS_AVG_PRICE_TST"
  )
  itis <- DBI::dbGetQuery(channel, "select * from CAMS_GARFO.CFG_ITIS")
  
  price_mt <- prices |>
    dplyr::left_join(itis, by = c("VALID_ITIS" = "ITIS_TSN")) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      YEAR = as.numeric(YEAR),
      MONTH = as.numeric(MONTH_IN_YEAR),
      NESPP3 = as.numeric(DLR_NESPP3),
      PRICEPP = AVERAGE_PRICE
    ) |>
    dplyr::filter(!is.na(NESPP3)) |>
    dplyr::group_by(YEAR, MONTH, NESPP3) |>
    dplyr::summarise(
      pricepmt = mean(PRICEPP, na.rm = TRUE) / lbstomt,
      .groups = "drop"
    )
  
  # 2. Deflator reference value (same series comlandr uses for the pull) ----
  fred_path <- system.file("extdata/fred/fred.rds", package = "comlandr")
  if (fred_path == "") {
    stop("comlandr deflator data (extdata/fred/fred.rds) not found")
  }
  defl <- readRDS(fred_path) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      YEAR = as.numeric(YEAR),
      MONTH = as.numeric(MONTH),
      defl = value
    ) |>
    dplyr::distinct(YEAR, MONTH, .keep_all = TRUE)
  reference <- defl |>
    dplyr::filter(YEAR == ref_year, MONTH == ref_month) |>
    dplyr::pull(defl)
  if (length(reference) != 1 || is.na(reference)) {
    stop("No deflator value for reference year/month ", ref_year, "/", ref_month)
  }
  
  # 3. Fill missing values from start_year on ----
  orig_cols <- names(comland_data$comland)
  cl <- comland_data$comland |>
    tibble::as_tibble() |>
    dplyr::mutate(
      .row = dplyr::row_number(),
      .YEAR = as.numeric(YEAR),
      .MONTH = as.numeric(MONTH),
      .NESPP3 = as.numeric(NESPP3)
    )
  
  filled <- cl |>
    dplyr::left_join(
      price_mt,
      by = c(".YEAR" = "YEAR", ".MONTH" = "MONTH", ".NESPP3" = "NESPP3")
    ) |>
    dplyr::left_join(defl, by = c(".YEAR" = "YEAR", ".MONTH" = "MONTH")) |>
    dplyr::mutate(
      .fill = .YEAR >= start_year & is.na(SPPVALUE),
      SPPVALUE = dplyr::if_else(
        .fill,
        SPPLIVMT * pricepmt * reference / defl,
        as.numeric(SPPVALUE)
      )
    )
  if (nrow(filled) != nrow(cl)) {
    stop("Joining prices changed the number of landings records")
  }
  
  # 4. Summarise the adjustment and return ----
  summary <- filled |>
    dplyr::filter(.fill) |>
    dplyr::summarise(
      records_missing = dplyr::n(),
      mt_missing = sum(SPPLIVMT, na.rm = TRUE),
      records_filled = sum(!is.na(SPPVALUE)),
      value_filled = sum(SPPVALUE, na.rm = TRUE),
      mt_unpriced = sum(SPPLIVMT[is.na(SPPVALUE)], na.rm = TRUE)
    ) |>
    dplyr::mutate(
      ref_year = ref_year,
      ref_month = ref_month,
      start_year = start_year,
      dropped_unpriced = drop_unpriced
    )
  
  if (drop_unpriced) {
    filled <- filled |> dplyr::filter(!(.fill & is.na(SPPVALUE)))
  }
  
  comland_data$comland <- filled |>
    dplyr::arrange(.row) |>
    dplyr::select(dplyr::all_of(orig_cols)) |>
    data.table::as.data.table()
  comland_data$revenue_adjustment <- summary
  
  return(comland_data)
}