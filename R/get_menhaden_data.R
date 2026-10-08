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
#'   \item **NOAA FOSS Data ($\le 2015$):** Extracted from `FOSS_landings.csv` (Middle Atlantic &
#'         New England) and `FOSS_landings_NC.csv` (North Carolina).
#'   \item **ACCSP Data ($> 2015$):** Extracted from `accsp_menhaden_SOE2027.csv` (Maine through North Carolina).
#' }
#'
#' Landings are mapped to Ecological Production Units (EPUs):
#' \itemize{
#'   \item `"GOM"` (Gulf of Maine): "New England" (FOSS) or "North Atlantic" (ACCSP)
#'   \item `"MAB"` (Mid-Atlantic Bight): All other subregions/states (including North Carolina)
#' }
#'
#' ACCSP weights are converted from pounds to metric tons using $1 \text{ metric ton} = 2204.62 \text{ lbs}$.
#'
#' @param api Logical. If `TRUE`, attempts to query online APIs for FOSS and ACCSP data.
#'   *Note:* API integration is currently unimplemented; setting `TRUE` triggers a warning
#'   and automatically falls back to local CSV files. Defaults to `FALSE`.
#' @param foss_region_path Character string. Full path to the FOSS regional landings CSV file. Use: `here::here("data-raw\\FOSS_landings.csv")`
#' @param foss_nc_path Character string. Full path to the FOSS NC landings CSV file. Use: `here::here("data-raw\\FOSS_landings_NC.csv")`
#' @param accsp_path Character string. Full path to the ACCSP landings CSV file. Use: `here::here("data-raw\\accsp_menhaden_SOE2027.csv")`
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
#' @note
#' **Required File Structure:**
#' This function expects the following files relative to the project root (`here::here()`):
#' \itemize{
#'   \item `data-raw/FOSS_landings.csv`
#'   \item `data-raw/FOSS_landings_NC.csv`
#'   \item `data-raw/accsp_menhaden_SOE2027.csv`
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
  foss_region_path = NULL,
  foss_nc_path = NULL,
  accsp_path = NULL
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
      foss_region_path,

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
      foss_nc_path,
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

    accsp <- read.csv(accsp_path) |>
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
