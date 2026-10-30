#' Creates the fuel prices data frame for the SOE
#'
#' This function creates a fuel prices data frame that is formatted for inclusion 
#' in the `ecodata` R package. It pulls legacy and modern diesel prices via the 
#' FRED API and deflates them to a specified base year.
#'
#' @param end_year Integer. The final year of data to include. Defaults to 2026.
#' @param fred_key Character. A valid FRED API key. Defaults to `NULL`.
#' 
#' @return A tibble containing nominal and deflated monthly fuel prices.
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # 1. Fetch the data (ensure your FRED key is set in .Renviron)
#' fuel_final <- create_fuel_prices(end_year = 2026)
#' 
#' # 2. Dynamically extract the deflate year from the data for the plot subtitle
#' deflate_year <- max(fuel_final$YEAR, na.rm = TRUE)
#' 
#' # 3. Visualize the time series
#' fuel_final |>
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = DATA_VALUE)) +
#'   # Plot the raw adjusted data lightly in the background, colored by era
#'   ggplot2::geom_line(ggplot2::aes(color = Series), alpha = 0.4) +
#'   # Add a 12-month Simple Moving Average (SMA) trendline
#'   tidyquant::geom_ma(ma_fun = SMA, n = 12, color = "black", linewidth = 1) +
#'   tidyquant::theme_tq() +
#'   tidyquant::scale_color_tq() +
#'   ggplot2::labs(
#'     title = "Real Wholesale Diesel Prices (New York Harbor)",
#'     subtitle = paste(
#'       "Adjusted to",
#'       deflate_year,
#'       "Constant Dollars (CPI)\nBlack line = 12-Month Simple Moving Average"
#'     ),
#'     y = "Price per Gallon (Constant Dollars)",
#'     x = "",
#'     color = "Fuel Type"
#'   ) +
#'   ggplot2::theme(legend.position = "bottom")
#' }
create_fuel_prices <- function(end_year = lubridate::year(Sys.Date()), fred_key = NULL) {
  
  ## FRED key (Consider moving this to your .Renviron file for security)
  if (!is.null(fred_key)) {
    fredr::fredr_set_key(fred_key)
  }
  
  if (!fredr::fredr_has_key()) {
    stop("No FRED API key found. Please provide `fred_key` or set it in your .Renviron.")
  }
  
  ## parameters
  start_year <- 1970
  current_year <- lubridate::year(Sys.Date())
  
  # Adjust end year if the current year is not yet complete
  if (current_year == end_year) {
    message(sprintf("Note: %s is not yet complete. Deflating to %s instead.", end_year, end_year - 1))
    deflate_year <- end_year - 1
  } else {
    deflate_year <- end_year
  }
  
  # Set the absolute end date for API queries
  end_date <- as.Date(paste0(end_year, "-12-31"))
  start_date <- as.Date(paste0(start_year, "-01-01"))
  
  # 1. CPI Deflator (Monthly - All Urban Consumers)
  message("Fetching CPI Deflator data (CPIAUCSL)...")
  cpi_deflator <- fredr::fredr(
    series_id = "CPIAUCSL",
    observation_start = start_date,
    observation_end = end_date,
    frequency = "m"
  ) |> 
    dplyr::transmute(
      YEAR = lubridate::year(.data$date),
      MONTH = lubridate::month(.data$date),
      CPI = .data$value
    )
  
  ## Calculate base index (Average of all available months in the base year)
  base_index_val <- cpi_deflator |> 
    dplyr::filter(.data$YEAR == deflate_year) |> 
    dplyr::pull(.data$CPI) |> 
    mean(na.rm = TRUE)
  
  # 2. Pull Legacy Diesel (1970 to May 2006)
  message("Fetching Legacy No. 2 Diesel prices (MHOILNYH)...")
  legacy_diesel <- fredr::fredr(
    series_id = "MHOILNYH",
    observation_start = start_date,
    observation_end = as.Date("2006-05-31"),
    frequency = "m"
  ) |>
    dplyr::mutate(Series = "Legacy No. 2 Diesel")
  
  # 3. Pull Modern ULSD (June 2006 to Present)
  message("Fetching Ultra-Low Sulfur Diesel prices (MDFUELNYH)...")
  modern_diesel <- fredr::fredr(
    series_id = "MDFUELNYH",
    observation_start = as.Date("2006-06-01"),
    observation_end = end_date,
    frequency = "m"
  ) |>
    dplyr::mutate(Series = "Ultra-Low Sulfur Diesel")
  
  # Combine both fuel series into one continuous dataset
  message(sprintf("Combining series and deflating to %s constant dollars...", deflate_year))
  fuel_raw <- dplyr::bind_rows(legacy_diesel, modern_diesel) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::transmute(
      YEAR = lubridate::year(.data$date),
      MONTH = lubridate::month(.data$date),
      date = .data$date,
      Series = .data$Series,
      nominal_price = .data$value
    )
  
  # 4. Deflate and Format
  fuel_final <- fuel_raw |>
    dplyr::left_join(cpi_deflator, by = c("YEAR", "MONTH")) |>
    # If fuel data updates faster than CPI, carry the most recent CPI value forward
    tidyr::fill("CPI", .direction = "down") |>
    dplyr::mutate(
      DATA_VALUE = (.data$nominal_price / .data$CPI) * base_index_val,
      INDICATOR_NAME = paste0("AVG_MONTHLY_DIESEL_PRICE_", deflate_year, "_dols")
    ) |>
    dplyr::select(
      "YEAR",
      "MONTH",
      "date",
      "Series",
      "nominal_price",
      "DATA_VALUE",
      "INDICATOR_NAME"
    )
  
  message("Done.")
  
  return(fuel_final)
}

