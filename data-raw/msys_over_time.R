## create time series of msys ----

### first pull msys
msys <- stocksmart::stock_assessment_summary |>
  dplyr::filter(science_center == "NEFSC") |>
  dplyr::select(
    stock_name,
    common_name,
    stock_area,
    jurisdiction,
    assessment_year,
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
  tidyr::complete(year = min(year):2025) |>
  # Forward-fill assessment metrics within each group
  tidyr::fill(
    fmsy,
    f_unit,
    bmsy,
    b_unit,
    assessment_year,
    .direction = "down"
  ) |>
  dplyr::ungroup()

## get actual f and b data ----
## actual f and b data from the most recent assessment
actual_fb <- stocksmart::stock_assessment_data |>
  dplyr::filter(
    stock_name %in% filled_msys$stock_name,
    metric %in% c("Fmort", "Abundance")
  ) |>
  dplyr::select(
    stock_name,
    year,
    value,
    metric,
    description,
    assessment_year
  ) |>
  dplyr::group_by(stock_name) |>
  dplyr::mutate(last_assessment = max(assessment_year)) |>
  dplyr::ungroup() |>
  dplyr::filter(assessment_year == last_assessment) |>
  dplyr::select(-c(assessment_year, last_assessment))

## join actual f and b data with filled msys data ----
joined_dat <- dplyr::left_join(
  actual_fb,
  filled_msys,
  by = c("stock_name", "year")
) |>
  dplyr::filter(!is.na(common_name)) |>
  dplyr::mutate(
    b_bmsy = dplyr::case_when(
      metric == "Abundance" & bmsy != "NA" ~ value / as.numeric(bmsy),
      TRUE ~ NA_real_
    ),
    f_fmsy = dplyr::case_when(
      metric == "Fmort" & fmsy != "NA" ~ value / as.numeric(fmsy),
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::select(-stock_name)

## create f_fmsy and b_bmsy datasets ----
f_fmsys <- joined_dat |>
  dplyr::filter(!is.na(f_fmsy)) |>
  dplyr::select(-c(bmsy, b_unit, b_bmsy))
b_bmsys <- joined_dat |>
  dplyr::filter(!is.na(b_bmsy)) |>
  dplyr::select(-c(fmsy, f_unit, f_fmsy))

## convert b_bmsy units ----
b_bmsy_converted <- b_bmsys |>
  dplyr::mutate(
    bmsy = as.numeric(bmsy),
    needs_conversion = b_unit %in%
      c(
        "Thousand Metric Tons",
        "thousand mt (female biomass only)",
        "mt (meat weight only)",
        "mt SSB"
      ),
    b_msy_corrected = dplyr::if_else(needs_conversion, bmsy * 1000, bmsy),
    b_unit_corrected = dplyr::if_else(needs_conversion, "Metric Tons", b_unit)
  ) |>
  dplyr::select(-needs_conversion)

## drop years where there is a mismatch in bmsy units (few years as kg/tow, or million pups for spiny dogfish) ----
# List of species with bmsy values in kg/tow that need to be dropped
target_species <- c(
  "Haddock",
  "Pollock",
  "Scup",
  "Sea scallop",
  "Spiny dogfish"
)
target_units <- c("kg/tow", "kg / Tow", "Million Pups")

b_bmsy_dropyears <- b_bmsy_converted |>
  dplyr::mutate(
    b_msy_corrected = dplyr::if_else(
      common_name %in% target_species & b_unit %in% target_units,
      NA_real_, # Set to NA if condition is met
      b_msy_corrected # Keep original value otherwise
    )
  )

## recalculate b/b_msy with converted bmsy ----
b_bmsy_final <- b_bmsy_dropyears |>
  dplyr::mutate(
    b_bmsy_corrected = dplyr::case_when(
      metric == "Abundance" & !is.na(b_msy_corrected) ~ value / b_msy_corrected,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::rename(b_over_bmsy = b_bmsy_corrected, species = common_name) |>
  dplyr::select(-c(bmsy, b_unit, b_bmsy))


##############################################################
## plots ----
joined_dat |>
  tidyr::drop_na(f_fmsy, fmsy) |>
  dplyr::mutate(
    f_fmsy = dplyr::case_when(
      (description == "Catch * 100 / Total Stock Biomass" &
        f_unit == "Catch / Survey-Estimated Biomass") ~
        f_fmsy / 100,
      TRUE ~ f_fmsy
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = f_fmsy, color = stock_name)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

joined_dat |>
  tidyr::drop_na(b_bmsy, bmsy) |>
  dplyr::filter(
    !(stringr::str_detect(description, "Biomass") &
      stringr::str_detect(b_unit, "tow")),
    b_bmsy < 100
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = b_bmsy, color = stock_name)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()


joined_dat |>
  tidyr::drop_na(b_bmsy, bmsy) |>
  dplyr::filter(b_bmsy > 5 & b_bmsy < 30) |>
  dplyr::filter(
    !(stringr::str_detect(description, "Biomass") &
      stringr::str_detect(b_unit, "tow"))
  ) |>
  # dplyr::select(description, b_unit) |>
  # dplyr::distinct()
  View()
