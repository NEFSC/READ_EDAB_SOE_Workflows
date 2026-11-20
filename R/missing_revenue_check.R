#' Check for missing revenue at the end of a commercial time series
#'
#' Calculates the share of landings (metric tons) with missing `SPPVALUE` in
#' the most recent years of a comlandr pull. Used by the automation workflow
#' to decide whether [adjusted_revenue()] is needed.
#'
#' @param comland_data List containing a `comland` data frame (one element of
#'   [get_commercial_data()] output).
#' @param n_years Integer. Number of most recent years to check (default 5).
#' @param threshold Numeric. Share of landings with missing value above which
#'   an adjustment is needed (default 0.01 = 1%).
#'
#' @return list
#' \item{needed}{logical. `TRUE` if the share of landings with missing value exceeds `threshold`}
#' \item{share}{numeric. Proportion of landings (metric tons) with missing `SPPVALUE`}
#' \item{years}{numeric. First and last year checked}
#'
#' @examples
#' \dontrun{
#' raw <- readRDS("path/to/commercial_comdat_data.rds")
#' missing_revenue_check(raw)
#' }
#'
#' @export
#'

missing_revenue_check <- function(comland_data, n_years = 5, threshold = 0.01) {
  cl <- comland_data$comland
  yrs <- as.numeric(cl$YEAR)
  last <- max(yrs, na.rm = TRUE)
  recent <- yrs > last - n_years
  mt_recent <- sum(cl$SPPLIVMT[recent], na.rm = TRUE)
  mt_missing <- sum(cl$SPPLIVMT[recent & is.na(cl$SPPVALUE)], na.rm = TRUE)
  share <- if (mt_recent > 0) mt_missing / mt_recent else 0
  return(list(
    needed = share > threshold,
    share = share,
    years = c(last - n_years + 1, last)
  ))
}