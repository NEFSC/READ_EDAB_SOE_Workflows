#' Function to create B/Bmsy and F/Fmsy time series from `stocksmart`
#'
#'
#' @param input_path_present_key Character string. Full path to a key indicating which stocks have matching units for the most recent assessment data
#' @param input_path_historic_key Character string. Full path to a key indicating which assessments to drop from the historic time series due to unit mismatches
#'
#' @note
#' The input keys are used to filter the `StockSmart` data based on unit matching between the MSY value and the timeseries unit.
#' Input keys are created with:
#' \dontrun{
#' create_msy_present(type = "for_analysis")
#' create_msy_historic(type = "for_analysis")
#' }
#' and then edited by hand to add columns `units_match` and `can_be_converted`.
#'
#'
#' @examples
#' \dontrun{
#' # create the ecodata::stock_status_timeseries indicator
#' create_stock_status_timeseries(input_path_present_key <- "path/to/input.csv",
#'                          input_path_historic_key <- "path/to/input.csv")
#'
#' }
#'
#'
#' @return ecodata::stock_status_timeseries data frame
#'
#' @export

create_stock_status_timeseries <- function(
  input_path_present_key = here::here(
    # "\\\\nefscdata\\EDAB_Resources\\static_workflow_inputs\\msy_conversions_present_reviewed.csv"
    here::here("data-raw/msy_conversions_present_reviewed.csv")
  ),
  input_path_historic_key = here::here(
    # "\\\\nefscdata\\EDAB_Resources\\static_workflow_inputs\\msy_conversions_historic_reviewed.csv"
    here::here("data-raw/msy_conversions_historic_reviewed.csv")
  )
) {
  ## read in keys
  units_check_present <- read.csv(input_path_present_key) |>
    dplyr::select(
      common_name,
      stock_area,
      metric,
      units_match,
      can_be_converted
    )

  units_check_historic <- read.csv(input_path_historic_key)

  ### easy time series -- most recent interpretation of msys time series ----

  output <- create_msy_present(type = "for_analysis") |>
    # filter out stocks where units don't match
    dplyr::left_join(units_check_present) |>
    ## none of the stocks with unit mismatches can be fixed with a thousand mt conversion
    ## but the can_be_converted column is there in case that changes
    dplyr::filter(units_match == TRUE | can_be_converted) |>
    # calculate msy
    dplyr::mutate(over_msy = value / msy) |>
    dplyr::select(
      common_name,
      stock_area,
      jurisdiction,
      metric,
      year,
      over_msy
    ) |>
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == "Fmort" ~ "f_fmsy",
        metric == "Abundance" ~ "b_bmsy"
      )
    ) |>
    dplyr::rename(value = over_msy)

  stock_status_timeseries <- output |>
    dplyr::mutate(
      Var.Group = "present_perception",
      Var.Stock = paste(common_name, stock_area)
    ) |>
    dplyr::rename(
      Var = metric,
      Time = year,
      Value = value,
      Jurisdiction = jurisdiction
    ) |>
    dplyr::select(Time, Var.Group, Var.Stock, Jurisdiction, Var, Value)

  ### hard time series -- stitch together to understand near-real-time perception ----

  output <- create_msy_historic(type = "for_analysis") |>
    dplyr::filter(!is.na(value), msy != "NA") |>
    ## adjust units if in thousands
    ## this won't fix all unit mismatches
    ## but it will fix some
    dplyr::mutate(
      msy = dplyr::case_when(
        stringr::str_detect(unit, "housand") ~ (as.numeric(msy) * 1000),
        TRUE ~ as.numeric(msy)
      ),
      value = dplyr::case_when(
        stringr::str_detect(timeseries_units, "housand") ~ value * 1000,
        TRUE ~ value
      )
    ) |>
    # filter out stocks where units don't match
    dplyr::left_join(
      units_check_historic |>
        dplyr::select(
          assessment_id,
          metric,
          units_match,
          # some of the unit mismatches can be converted
          can_be_converted
        )
    ) |>
    dplyr::filter(
      units_match == TRUE |
        can_be_converted
    ) |>
    # calculate ratios
    dplyr::mutate(over_msy = value / as.numeric(msy)) |>
    dplyr::select(
      common_name,
      stock_area,
      jurisdiction,
      metric,
      year,
      over_msy
    ) |>
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == "Fmort" ~ "f_fmsy",
        metric == "Abundance" ~ "b_bmsy"
      )
    ) |>
    dplyr::rename(value = over_msy)

  stock_status_timeseries <- stock_status_timeseries |>
    dplyr::bind_rows(
      output |>
        dplyr::mutate(
          Var.Group = "historic_perception",
          Var.Stock = paste(common_name, stock_area)
        ) |>
        dplyr::rename(
          Var = metric,
          Time = year,
          Value = value,
          Jurisdiction = jurisdiction
        ) |>
        dplyr::select(Time, Var.Group, Var.Stock, Jurisdiction, Var, Value)
    )

  return(stock_status_timeseries)
}
