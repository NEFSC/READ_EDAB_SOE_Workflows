## FRED key
fredr::fredr_set_key("a09e5d083681605146191f4996992c6e")

## parameters
START.YEAR <- 1970
END.YEAR <- 2026
deflate_year <- 2025

## GDP deflator table
gdp_deflator <- fredr::fredr(
  series_id = "GDPDEF",
  observation_start = as.Date(paste0(START.YEAR, "-01-01")),
  observation_end = as.Date(paste0(END.YEAR, "-12-31")),
  frequency = "a" # Annual
) |>
  dplyr::mutate(YEAR = as.numeric(format(date, "%Y"))) |>
  dplyr::select(YEAR, GDPDEF = value)

## which year to deflate to
base_index_val <- gdp_deflator$GDPDEF[gdp_deflator$YEAR == deflate_year]

## pull monthly fuel prices
fuel_raw <- fredr::fredr(
  series_id = "MDFUELNYH",
  observation_start = as.Date(paste0(START.YEAR, "-01-01")),
  observation_end = as.Date(paste0(END.YEAR, "-12-31")),
  frequency = "m"
) |>
  dplyr::mutate(
    YEAR = as.numeric(format(date, "%Y")),
    MONTH = lubridate::month(date)
  ) |>
  # Stata 'drop if missing(DDFUELNYH)' equivalent:
  dplyr::filter(!is.na(value)) |>
  dplyr::select(YEAR, MONTH, date, MDFUELNYH = value)

# 2. Deflate and Format
fuel_final <- fuel_raw |>
  dplyr::left_join(gdp_deflator, by = "YEAR") |>
  dplyr::mutate(
    DATA_VALUE = (MDFUELNYH /
      # sub in most recent deflator values if not available for current year yet
      ifelse(!is.na(GDPDEF), GDPDEF, base_index_val) *
      base_index_val),
    INDICATOR_NAME = paste0("AVG_MONTHLY_DIESEL_PRICE", deflate_year, "dols")
  ) |>
  dplyr::select(
    YEAR,
    MONTH,
    date,
    DATA_VALUE,
    INDICATOR_NAME
  )

## plot

fuel_final |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = DATA_VALUE)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ecodata::geom_gls() +
  ggplot2::ylab(unique(fuel_final$INDICATOR_NAME))
