#' Extract and Process Menhaden Landings Data
#'
#' @description
#' `get_menhaden_data()` loads, cleans, and aggregates Atlantic menhaden commercial
#' landings data from NOAA FOSS and ACCSP local raw data files to prepare a standardized
#' dataset for the `comdat` workflow. It has a placeholder for eventual API integration.
#'
#' @details
#' The function processes commercial landings data split temporally across two primary sources:
#' \itemize{
#'   \item **NOAA FOSS Data (through 2015):** Extracted from regional data (Middle Atlantic &
#'         New England) and North Carolina data.
#'   \item **ACCSP Data (2016 and on):** Extracted from state-level data (Maine through North Carolina).
#' }
#'
#' Landings are mapped to Ecological Production Units (EPUs):
#' \itemize{
#'   \item `"GOM"` (Gulf of Maine): "New England" (FOSS) or "North Atlantic" (ACCSP)
#'   \item `"MAB"` (Mid-Atlantic Bight): All other subregions/states (including North Carolina)
#' }
#'
#' ACCSP weights are converted from pounds to metric tons using 1 metric ton = 2204.62 lbs.
#'
#' @param api Logical. If `TRUE`, attempts to query online APIs for FOSS and ACCSP data.
#'   *Note:* API integration is currently unimplemented; setting `TRUE` triggers a warning
#'   and automatically falls back to local CSV files. Defaults to `FALSE`.
#' @param input_path_foss Character string. Full path to the FOSS regional landings CSV file.
#' @param input_path_foss_nc Character string. Full path to the FOSS NC landings CSV file.
#' @param input_path_accsp Character string. Full path to the ACCSP landings CSV file.
#'
#' @return A `tibble` (or `data.frame`) with the following columns:
#' \describe{
#'   \item{`year`}{Numeric year of reporting.}
#'   \item{`EPU`}{Ecological Production Unit (`"GOM"` or `"MAB"`).}
#'   \item{`metric_tons`}{Total commercial landings in metric tons.}
#'   \item{`dollars`}{Total commercial landed value in USD.}
#'   \item{`species`}{Species grouping identifier (`"Menhadens"`).}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   menhaden_df <- get_menhaden_data()
#'   head(menhaden_df)
#' }

get_menhaden_data <- function(
  api = FALSE,
  input_path_foss = NULL,
  input_path_foss_nc = NULL,
  input_path_accsp = NULL
) {
  if (api == TRUE) {
    message(
      "You set api = TRUE but the API methods don't exist yet. Use local files instead."
    )
    api <- FALSE
  }

  if (api) {
    # Get data from API
    # foss_region <- ...
    # foss_nc <- ...
    # accsp <- ...
  } else {
    # Get data from local files

    ## FOSS downloads
    # URL: https://www.fisheries.noaa.gov/foss/f?p=215:200:5384584248688:::::

    ## regional settings:
    # Data Set = Commercial
    # YEar = all years
    # Region type = "NMFS Regions"
    # State Landed = Middle Atlantic, New England
    # Species = Menhaden, Atlantic & Menhadens **
    # Report format = Totals by year/region
    foss_region <- read.csv(
      input_path_foss,

      skip = 1
    ) |>
      janitor::clean_names() |>
      dplyr::mutate(
        metric_tons = stringr::str_remove(metric_tons, ",") |>
          as.numeric(),
        dollars = stringr::str_remove_all(dollars, ",") |>
          as.numeric()
      ) |>
      dplyr::filter(year <= 2015)

    ## NC settings:
    # Data Set = Commercial
    # YEar = all years
    # Region type = States
    # State Landed = North Carolina
    # Species = Menhaden, Atlantic & Menhadens **
    # Report format = Totals by year
    foss_nc <- read.csv(
      input_path_foss_nc,
      skip = 1
    ) |>
      janitor::clean_names() |>
      dplyr::mutate(
        metric_tons = stringr::str_remove(metric_tons, ",") |>
          as.numeric(),
        dollars = stringr::str_remove_all(dollars, ",") |>
          as.numeric()
      ) |>
      dplyr::filter(year <= 2015)

    ## ACCSP download
    # URL: https://safis.accsp.org:8443/accsp_prod/f?p=1510:LOGIN:1568150841700:::::
    # requires free account
    # data warehouse --> non-confidential data --> warehouse non-confidential data --> non-confidential commercial landings

    ## settings:
    # start year = 1950
    # end year = 2025
    # spatial of landing = state
    # States = ME, NH, MA, RI, CT, NY, NJ, DE, MD, VA, NC
    # Species = Menhaden, Atlantic (161732) & Menhadens (161731)

    accsp <- read.csv(input_path_accsp) |>
      janitor::clean_names() |>
      dplyr::mutate(
        metric_tons = pounds / 2204.62
      ) |>
      dplyr::filter(year > 2015)
  }

  foss_region_cleaned <- foss_region |>
    dplyr::mutate(
      EPU = dplyr::case_when(region_name == "New England" ~ "GOM", TRUE ~ "MAB")
    ) |>
    dplyr::select(year, metric_tons, dollars, EPU)

  foss_nc_cleaned <- foss_nc |>
    dplyr::select(year, metric_tons, dollars) |>
    dplyr::mutate(EPU = "MAB")

  accsp_cleaned <- accsp |>
    dplyr::mutate(
      EPU = dplyr::case_when(
        subregion == "North Atlantic" ~ "GOM",
        TRUE ~ "MAB"
      )
    ) |>
    dplyr::select(year, metric_tons, dollars, EPU)

  output <- dplyr::bind_rows(
    foss_region_cleaned,
    foss_nc_cleaned,
    accsp_cleaned
  ) |>
    dplyr::group_by(year, EPU) |>
    dplyr::summarise(
      metric_tons = sum(metric_tons, na.rm = TRUE),
      dollars = sum(dollars, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(species = "Menhadens")

  return(output)
}
