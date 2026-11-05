#' Function to create B/Bmsy and F/Fmsy time series from `stocksmart`
#'
#'
#' @param input_path_present_key Character string. Full path to a key indicating which stocks have matching units for the most recent assessment data
#' @param input_path_historic_key Character string. Full path to a key indicating which assessments to drop from the historic time series due to unit mismatches
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
    "\\\\nefscdata\\EDAB_Resources\\static_workflow_inputs\\msy_conversions_present_reviewed.csv"
  ),
  input_path_historic_key = here::here(
    "\\\\nefscdata\\EDAB_Resources\\static_workflow_inputs\\msy_conversions_historic_reviewed.csv"
  )
) {
  ## read in keys
  units_check_present <- read.csv(input_path_present_key) |>
    dplyr::select(common_name, stock_area, metric, units_match)

  units_check_historic <- read.csv(input_path_historic_key)

  ### easy time series -- most recent interpretation of msys time series ----

  fb_timeseries <- stocksmart::stock_assessment_data |>
    dplyr::filter(
      jurisdiction %in% c("NEFMC", "MAFMC", "NEFMC / MAFMC"),
      metric %in% c("Fmort", "Abundance")
    ) |>
    dplyr::select(
      stock_name,
      year,
      assessment_id,
      value,
      metric,
      description,
      units,
      assessment_year
    ) |>
    dplyr::rename(timeseries_units = units) |>
    dplyr::group_by(stock_name) |>
    dplyr::mutate(last_assessment = max(assessment_year)) |>
    dplyr::ungroup() |>
    dplyr::filter(assessment_year == last_assessment) |>
    dplyr::select(-c(assessment_year, last_assessment))

  msys <- stocksmart::stock_assessment_summary |>
    dplyr::filter(science_center == "NEFSC") |>
    dplyr::select(
      stock_name,
      common_name,
      stock_area,
      jurisdiction,
      assessment_year,
      assessment_id,
      review_result,
      fmsy,
      f_unit,
      bmsy,
      b_unit
    ) |>
    # remove rejected assessments
    dplyr::filter(review_result == "Full acceptance") |>
    dplyr::select(-review_result)

  bmsys <- msys |>
    dplyr::filter(!is.na(bmsy)) |>
    dplyr::select(-fmsy, -f_unit) |>
    dplyr::mutate(metric = "Abundance") |>
    dplyr::rename(msy = bmsy, unit = b_unit)

  fmsys <- msys |>
    dplyr::filter(!is.na(fmsy)) |>
    dplyr::select(-bmsy, -b_unit) |>
    dplyr::mutate(metric = "Fmort") |>
    dplyr::rename(msy = fmsy, unit = f_unit)

  joined_dat <- fb_timeseries |>
    dplyr::left_join(
      dplyr::bind_rows(bmsys, fmsys)
    ) |>
    dplyr::select(-assessment_id, -stock_name) |>
    # na's have no msy value
    tidyr::drop_na(common_name)

  # conversion_table <- joined_dat |>
  #   dplyr::select(-year, -value) |>
  #   dplyr::distinct()
  # write.csv(conversion_table, here::here("data-raw/msy_conversions_present.csv"))

  output <- joined_dat |>
    # filter out stocks where units don't match
    dplyr::left_join(units_check_present) |>
    dplyr::filter(units_match == TRUE) |>
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

  #### all of the time series data ----
  fb_timeseries_all <- stocksmart::stock_assessment_data |>
    dplyr::filter(
      jurisdiction %in% c("NEFMC", "MAFMC", "NEFMC / MAFMC"),
      metric %in% c("Fmort", "Abundance")
    ) |>
    dplyr::select(
      stock_name,
      year,
      assessment_id,
      value,
      metric,
      description,
      units,
      assessment_year
    ) |>
    dplyr::rename(timeseries_units = units)

  ####  pull msys ----
  msys <- stocksmart::stock_assessment_summary |>
    dplyr::filter(science_center == "NEFSC") |>
    dplyr::select(
      stock_name,
      common_name,
      stock_area,
      jurisdiction,
      assessment_year,
      assessment_id,
      last_data_year,
      review_result,
      fmsy,
      f_unit,
      bmsy,
      b_unit
    ) |>
    # remove rejected assessments
    dplyr::filter(review_result == "Full acceptance") |>
    dplyr::select(-review_result) |>
    # make NA's real values to propagate in gap filling
    dplyr::mutate(
      fmsy = ifelse(is.na(fmsy), "NA", fmsy),
      bmsy = ifelse(is.na(bmsy), "NA", bmsy)
    )

  ### pad out to full time series
  filled_msys <- msys |>
    dplyr::mutate(year = assessment_year) |>
    # Group by stock-level metadata to keep them associated with the stock
    dplyr::group_by(
      stock_name,
      common_name,
      stock_area,
      jurisdiction,
    ) |>
    # Generate a continuous sequence of years for each stock up to end_year
    tidyr::complete(
      year = min(fb_timeseries_all$year):max(fb_timeseries_all$year)
    ) |>
    # backward-fill assessment metrics within each group
    tidyr::fill(
      fmsy,
      f_unit,
      bmsy,
      b_unit,
      assessment_year,
      assessment_id,
      last_data_year,
      .direction = "up"
    ) |>
    # when assessment metrics should be filled based on last_data_year, not the assessment_year
    dplyr::mutate(
      assessment_year = dplyr::case_when(
        last_data_year < year ~ NA,
        TRUE ~ assessment_year
      ),
      assessment_id = dplyr::case_when(
        last_data_year < year ~ NA,
        TRUE ~ assessment_id
      ),
      fmsy = dplyr::case_when(last_data_year < year ~ NA, TRUE ~ fmsy),
      f_unit = dplyr::case_when(
        last_data_year < year ~ NA,
        TRUE ~ f_unit
      ),
      bmsy = dplyr::case_when(last_data_year < year ~ NA, TRUE ~ bmsy),
      b_unit = dplyr::case_when(
        last_data_year < year ~ NA,
        TRUE ~ b_unit
      )
    ) |>
    # back fill from the next assessment
    tidyr::fill(
      fmsy,
      f_unit,
      bmsy,
      b_unit,
      assessment_year,
      assessment_id,
      .direction = "up"
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-last_data_year)

  #### join data ----

  bmsys <- filled_msys |>
    dplyr::filter(!is.na(bmsy)) |>
    dplyr::select(-fmsy, -f_unit) |>
    dplyr::mutate(metric = "Abundance") |>
    dplyr::rename(msy = bmsy, unit = b_unit)

  fmsys <- filled_msys |>
    dplyr::filter(!is.na(fmsy)) |>
    dplyr::select(-bmsy, -b_unit) |>
    dplyr::mutate(metric = "Fmort") |>
    dplyr::rename(msy = fmsy, unit = f_unit)

  new_msys <- dplyr::bind_rows(bmsys, fmsys)

  # join into msys because those have the right assessment ID for each year
  joined_dat <- new_msys |>
    dplyr::left_join(
      fb_timeseries_all
    ) |>
    dplyr::select(-stock_name) |>
    # na's have no msy value
    tidyr::drop_na(value)

  # conversion_table2 <- joined_dat |>
  #   dplyr::select(-year, -value) |>
  #   dplyr::distinct()
  # write.csv(
  #   conversion_table2,
  #   here::here("data-raw/msy_conversions_historic.csv")
  # )

  output <- joined_dat |>
    dplyr::filter(!is.na(value), msy != "NA") |>
    ## adjust units if in thousands
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
          # if there's a conversion factor, it was fixed with the thousand mt conversion
          conversion_factor
        )
    ) |>
    dplyr::filter(
      units_match == TRUE |
        !is.na(conversion_factor)
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
