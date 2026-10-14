## create time series of msys
msys <- stocksmart::stock_assessment_summary |>
  dplyr::filter(science_center == "NEFSC") |>
  dplyr::select(
    stock_name,
    jurisdiction,
    itis,
    scientific_name,
    common_name,
    stock_area,
    assessment_year,
    review_result,
    fmsy,
    f_unit,
    bmsy,
    b_unit,
    msy,
    msy_unit
  )

filled_msys <- msys |>
  dplyr::mutate(year = assessment_year) |>
  # Group by stock-level metadata to keep them associated with the stock
  dplyr::group_by(
    stock_name,
    jurisdiction,
    itis,
    scientific_name,
    common_name,
    stock_area
  ) |>
  # Generate a continuous sequence of years for each stock up to end_year
  tidyr::complete(year = min(year):2025) |>
  # Forward-fill assessment metrics within each group
  tidyr::fill(
    review_result,
    fmsy,
    f_unit,
    bmsy,
    b_unit,
    msy,
    msy_unit,
    assessment_year,
    .direction = "down"
  ) |>
  dplyr::ungroup()

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
  dplyr::filter(assessment_year == last_assessment)

## join
joined_dat <- dplyr::left_join(
  actual_fb,
  filled_msys,
  by = c("stock_name", "year")
) |>
  dplyr::filter(year > 2000) |>
  dplyr::mutate(
    b_bmsy = dplyr::case_when(
      metric == "Abundance" ~ value / bmsy,
      TRUE ~ NA_real_
    ),
    f_fmsy = dplyr::case_when(
      metric == "Fmort" ~ value / fmsy,
      TRUE ~ NA_real_
    )
  )

## plots
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
  dplyr::filter(b_bmsy > 3000) |>
  dplyr::filter(
    !(stringr::str_detect(description, "Biomass") &
      stringr::str_detect(b_unit, "tow"))
  ) |>
  # dplyr::select(description, b_unit) |>
  # dplyr::distinct()
  View()
