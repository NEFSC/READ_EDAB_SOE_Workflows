## create time series of msys ----

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

## TODO: some units don't match
## quahog, surfclam
# conversion_table <- joined_dat |>
#   dplyr::select(-year, -value) |>
#   dplyr::distinct()
# write.csv(conversion_table, here::here("data-raw/msy_conversions_present.csv"))

units_check <- read.csv(here::here(
  "data-raw/msy_conversions_present_reviewed.csv"
)) |>
  dplyr::select(common_name, stock_area, metric, units_match)

output <- joined_dat |>
  # filter out stocks where units don't match
  dplyr::left_join(units_check) |>
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

#### test plot ----

output |>
  dplyr::filter(!is.na(value)) |>
  dplyr::group_by(year) |>
  dplyr::mutate(n_stocks = dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = log10(value), color = n_stocks)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::facet_wrap(~metric, ncol = 1, scales = "free") +
  viridis::scale_color_viridis() +
  ggplot2::xlim(c(1970, NA)) +
  ggplot2::theme_bw()

output |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(over1 = ifelse(value > 1, 1, 0)) |>
  dplyr::group_by(year, metric) |>
  dplyr::summarise(number_over_1 = sum(over1), n_stocks = dplyr::n()) |>
  dplyr::mutate(proportion_over_1 = number_over_1 / n_stocks) |>
  ggplot2::ggplot(ggplot2::aes(
    x = year,
    y = proportion_over_1,
    color = metric
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  # ggplot2::facet_wrap(~metric, ncol = 1, scales = "free") +
  viridis::scale_color_viridis(discrete = TRUE) +
  ggplot2::xlim(c(1970, NA)) +
  ggplot2::theme_bw()


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
  tidyr::complete(year = min(fb_timeseries_all$year):2025) |>
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
  # dplyr::ungroup() |>
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

## TODO: some units don't match
## quahog, surfclam
# conversion_table2 <- joined_dat |>
#   dplyr::select(-year, -value) |>
#   dplyr::distinct()
# write.csv(
#   conversion_table2,
#   here::here("data-raw/msy_conversions_historic.csv")
# )

units_check <- read.csv(here::here(
  "data-raw/msy_conversions_historic_reviewed.csv"
))

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
    units_check |> dplyr::select(assessment_id, metric, units_match)
  ) |>
  dplyr::filter(units_match == TRUE) |>
  # calculate ratios
  dplyr::mutate(over_msy = value / as.numeric(msy)) |>
  ## fix just spiny dogfish -- thousand MT vs MT units
  # dplyr::mutate(
  #   over_msy = ifelse(
  #     common_name == "Spiny dogfish" & assessment_year == 2018,
  #     over_msy / 1000,
  #     over_msy
  #   )
  # ) |>
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

#### test plot ----

output |>
  dplyr::filter(!is.na(value)) |>
  dplyr::group_by(year) |>
  dplyr::mutate(n_stocks = dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = log10(value), color = n_stocks)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::facet_wrap(~metric, ncol = 1, scales = "free") +
  viridis::scale_color_viridis() +
  ggplot2::xlim(c(1970, NA)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("near-real-time perception")

output |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(over1 = ifelse(value > 1, 1, 0)) |>
  dplyr::group_by(year, metric) |>
  dplyr::summarise(number_over_1 = sum(over1), n_stocks = dplyr::n()) |>
  dplyr::mutate(proportion_over_1 = number_over_1 / n_stocks) |>
  ggplot2::ggplot(ggplot2::aes(
    x = year,
    y = proportion_over_1,
    color = metric
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  # ggplot2::facet_wrap(~metric, ncol = 1, scales = "free") +
  viridis::scale_color_viridis(discrete = TRUE) +
  ggplot2::xlim(c(1970, NA)) +
  ggplot2::theme_bw()

## plotting functions ----

plot_stock_status_timeseries <- function(
  data,
  varName = c("b_present", "b_historic", "f_present", "f_historic"),
  plottype = c("log", "raw", "percent"),
  region = c("MidAtlantic", "NewEngland")
) {
  ### set variables
  filter_vars <- dplyr::case_when(
    varName == "b_present" ~ c("b_bmsy", "present_perception"),
    varName == "b_historic" ~ c("b_bmsy", "historic_perception"),
    varName == "f_present" ~ c("f_fmsy", "present_perception"),
    varName == "f_historic" ~ c("f_fmsy", "historic_perception")
  )
  juris <- dplyr::case_when(
    region == "MidAtlantic" ~ "MAFMC",
    region == "NewEngland" ~ "NEFMC"
  )
  title_region <- ifelse(
    region == "MidAtlantic",
    "Mid-Atlantic",
    "New England"
  )

  ### wrangle data
  plt_dat <- data |>
    dplyr::filter(
      Var == filter_vars[1],
      Var.Group == filter_vars[2],
      stringr::str_detect(Jurisdiction, juris)
    )

  if (plottype == "log") {
    plt_dat <- plt_dat |>
      dplyr::mutate(Value = log10(Value))
  }

  ### plot based on plottype
  if (plottype == "log" | plottype == "raw") {
    msy <- paste0(
      ifelse(filter_vars[1] == "b_bmsy", "B/BMSY", "F/FMSY"),
      paste0(" (", plottype, " scale)")
    )
    title <- paste(
      title_region,
      unique(plt_dat$Var.Group) |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_lower(),
      "of",
      msy
    )

    plt <- plt_dat |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::group_by(Time) |>
      dplyr::mutate(n_stocks = dplyr::n()) |>
      ggplot2::ggplot(ggplot2::aes(
        x = Time,
        y = Value,
        color = n_stocks
      )) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0) +
      viridis::scale_color_viridis() +
      ggplot2::xlim(c(1970, NA)) +
      ggplot2::ggtitle(title) +
      ggplot2::ylab(msy) +
      ggplot2::guides(
        color = ggplot2::guide_colorbar(title = "Number\nof stocks")
      ) +
      ecodata::theme_facet()
  }

  if (plottype == "percent") {
    msy <- ifelse(filter_vars[1] == "b_bmsy", "B/BMSY", "F/FMSY")

    title <- paste(
      title_region,
      unique(plt_dat$Var.Group) |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_lower(),
      "of stocks meeting",
      msy,
      "targets"
    )

    plt_dat <- plt_dat |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::mutate(
        over1 = ifelse(Value > 1, 1, 0),
        over0.5 = ifelse(Value > 0.5, 1, 0)
      ) |>
      dplyr::group_by(Time, Var) |>
      dplyr::summarise(
        number_over_1 = sum(over1),
        number_over_0.5 = sum(over0.5),
        n_stocks = dplyr::n()
      ) |>
      dplyr::mutate(
        proportion_over_1 = number_over_1 / n_stocks,
        proportion_over_0.5 = number_over_0.5 / n_stocks
      )

    if (plt_dat$Var[1] == "b_bmsy") {
      plt <- plt_dat |>
        tidyr::pivot_longer(
          cols = c("proportion_over_1", "proportion_over_0.5")
        ) |>
        ggplot2::ggplot(ggplot2::aes(
          x = Time,
          y = value,
          color = name |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_to_sentence()
        )) +
        ggplot2::guides(
          color = ggplot2::guide_legend(title = ggplot2::element_blank())
        )
    } else {
      plt <- plt_dat |>
        ggplot2::ggplot(ggplot2::aes(
          x = Time,
          y = 1 - proportion_over_1,
          color = Var
        )) +
        ggplot2::theme(legend.position = "none")
    }

    plt <- plt +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      viridis::scale_color_viridis(discrete = TRUE) +
      ggplot2::xlim(c(1970, NA)) +
      ggplot2::ylim(c(0, 1)) +
      ggplot2::ylab("Proportion") +
      ggplot2::ggtitle(title) +
      ecodata::theme_facet()
  }

  return(plt)
}

## Mid

params <- expand.grid(
  plottype = c("log", "percent"),
  varName = c("b_present", "b_historic", "f_present", "f_historic"),
  region = c("MidAtlantic", "NewEngland")
)

dat <- create_stock_status_timeseries()
plts_out <- list()
purrr::pwalk(
  .l = params,
  .f = function(region, varName, plottype) {
    plt <- plot_stock_status_timeseries(
      data = dat,
      region = region,
      varName = varName,
      plottype = plottype
    )
    plts_out <<- append(plts_out, list(plt))
  }
)

ggpubr::ggarrange(plotlist = plts_out, ncol = 4, nrow = 4)
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "b_present",
#   plottype = "log"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "b_historic",
#   plottype = "log"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "b_present",
#   plottype = "percent"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "b_historic",
#   plottype = "percent"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "f_present",
#   plottype = "log"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "f_historic",
#   plottype = "log"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "f_present",
#   plottype = "percent"
# )
#
# plot_stock_status_timeseries(
#   data = stock_status_timeseries,
#   region = "MidAtlantic",
#   varName = "f_historic",
#   plottype = "percent"
# )
