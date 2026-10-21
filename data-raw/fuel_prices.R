# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidyquant) # For the financial theme and moving average

## FRED key (Consider moving this to your .Renviron file for security)
fredr::fredr_set_key("")

## parameters
START.YEAR <- 1970
END.YEAR <- 2026

# deflate to end year, or to end year - 1 if the year is not complete
deflate_year <- ifelse(
  Sys.Date() |> lubridate::year() == END.YEAR,
  END.YEAR - 1,
  END.YEAR
)

# 1. CPI Deflator (Monthly - All Urban Consumers)
cpi_deflator <- fredr::fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date(paste0(START.YEAR, "-01-01")),
  observation_end = as.Date(paste0(END.YEAR, "-12-31")),
  frequency = "m"
) |>
  dplyr::mutate(
    YEAR = as.numeric(format(date, "%Y")),
    MONTH = lubridate::month(date)
  ) |>
  dplyr::select(YEAR, MONTH, CPI = value)

## Calculate base index (Average of all available months in the base year)
base_index_val <- mean(
  cpi_deflator$CPI[cpi_deflator$YEAR == deflate_year],
  na.rm = TRUE
)

# 2. Pull Legacy Diesel (1970 to May 2006)
legacy_diesel <- fredr::fredr(
  series_id = "MHOILNYH",
  observation_start = as.Date(paste0(START.YEAR, "-01-01")),
  observation_end = as.Date("2006-05-31"),
  frequency = "m"
) |>
  dplyr::mutate(Series = "Legacy No. 2 Diesel")

# 3. Pull Modern ULSD (June 2006 to Present)
modern_diesel <- fredr::fredr(
  series_id = "MDFUELNYH",
  observation_start = as.Date("2006-06-01"),
  observation_end = as.Date(paste0(END.YEAR, "-12-31")),
  frequency = "m"
) |>
  dplyr::mutate(Series = "Ultra-Low Sulfur Diesel")

# Combine both fuel series into one continuous dataset
fuel_raw <- dplyr::bind_rows(legacy_diesel, modern_diesel) |>
  dplyr::mutate(
    YEAR = as.numeric(format(date, "%Y")),
    MONTH = lubridate::month(date)
  ) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::select(YEAR, MONTH, date, Series, nominal_price = value)

# 4. Deflate and Format
fuel_final <- fuel_raw |>
  dplyr::left_join(cpi_deflator, by = c("YEAR", "MONTH")) |>
  # If fuel data updates faster than CPI, carry the most recent CPI value forward
  tidyr::fill(CPI, .direction = "down") |>
  dplyr::mutate(
    DATA_VALUE = (nominal_price / CPI) * base_index_val,
    INDICATOR_NAME = paste0("AVG_MONTHLY_DIESEL_PRICE_", deflate_year, "_dols")
  ) |>
  dplyr::select(
    YEAR,
    MONTH,
    date,
    Series,
    nominal_price,
    DATA_VALUE,
    INDICATOR_NAME
  )

# 5. Plot using tidyquant (optional)
fuel_final |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = DATA_VALUE)) +
  # Plot the raw adjusted data lightly in the background, colored by era
  ggplot2::geom_line(ggplot2::aes(color = Series), alpha = 0.4) +
  # Add a 12-month Simple Moving Average (SMA) trendline
  tidyquant::geom_ma(ma_fun = SMA, n = 12, color = "black", linewidth = 1) +
  tidyquant::theme_tq() +
  tidyquant::scale_color_tq() +
  ggplot2::labs(
    title = "Real Wholesale Diesel Prices (New York Harbor)",
    subtitle = paste(
      "Adjusted to",
      deflate_year,
      "Constant Dollars (CPI)\nBlack line = 12-Month Simple Moving Average"
    ),
    y = "Price per Gallon (Constant Dollars)",
    x = "",
    color = "Fuel Type"
  ) +
  ggplot2::theme(legend.position = "bottom")


## plot (optional)

fuel_final |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = DATA_VALUE)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ecodata::geom_gls() +
  ggplot2::ylab(unique(fuel_final$INDICATOR_NAME))
