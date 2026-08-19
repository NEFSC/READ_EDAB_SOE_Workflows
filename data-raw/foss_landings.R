### menhaden ----

menhaden <- read.csv(here::here("data-raw/FOSS_landings.csv"), skip = 1) |>
  janitor::clean_names() |>
  dplyr::mutate(
    metric_tons = stringr::str_remove(metric_tons, ",") |>
      as.numeric()
  )

menhaden |>
  dplyr::group_by(year) |>
  dplyr::summarise(metric_tons = sum(metric_tons)) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = metric_tons)) +
  ggplot2::geom_col(color = "white", fill = "cyan4") +
  ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, 700000)) +
  ggplot2::theme_bw()

#### menhaden by state ----

menhaden <- read.csv(
  here::here("data-raw/FOSS_landings_state.csv"),
  skip = 1
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    metric_tons = stringr::str_remove(metric_tons, ",") |>
      as.numeric(),
    EPU = dplyr::case_when(
      state %in%
        c(
          "MAINE",
          "NEW HAMPSHIRE",
          "MASSACHUSETTS",
          "RHODE ISLAND",
          "CONNECTICUT"
        ) ~
        "GOM",
      state %in%
        c(
          "NEW YORK",
          "NEW JERSEY",
          "DELAWARE",
          "MARYLAND",
          "PENNSYLVANIA",
          "VIRGINIA",
          "NORTH CAROLINA"
        ) ~
        "MAB",
      TRUE ~ NA_character_
    )
  )

menhaden |>
  dplyr::group_by(year, EPU) |>
  dplyr::summarise(metric_tons = sum(metric_tons, na.rm = TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = metric_tons, color = EPU)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::theme_bw()

## these numbers are too low, looks like VA data isn't included

#### create menhaden by region and add NC separately ----
menhaden_region <- read.csv(
  here::here("data-raw/FOSS_landings.csv"),
  skip = 1
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    metric_tons = stringr::str_remove(metric_tons, ",") |>
      as.numeric(),
    dollars = stringr::str_remove_all(dollars, ",") |>
      as.numeric()
  ) |>
  dplyr::filter(region_name != "South Atlantic") |>
  dplyr::select(year, region_name, dollars, metric_tons)

menhaden_nc <- read.csv(
  here::here("data-raw/FOSS_landings_NC.csv"),
  skip = 1
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    metric_tons = stringr::str_remove(metric_tons, ",") |>
      as.numeric(),
    dollars = stringr::str_remove_all(dollars, ",") |>
      as.numeric(),
    region_name = "Middle Atlantic"
  ) |>
  dplyr::select(year, region_name, dollars, metric_tons)

all_menhaden <- dplyr::bind_rows(menhaden_region, menhaden_nc) |>
  dplyr::group_by(region_name, year) |>
  dplyr::summarise(
    metric_tons = sum(metric_tons, na.rm = TRUE),
    dollars = sum(dollars, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    EPU = dplyr::case_when(
      region_name == "New England" ~ "GOM",
      region_name == "Middle Atlantic" ~ "MAB"
    )
  ) |>
  dplyr::select(-region_name)

all_menhaden |>
  dplyr::group_by(year) |>
  dplyr::summarise(metric_tons = sum(metric_tons)) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = metric_tons)) +
  ggplot2::geom_col(color = "white", fill = "cyan4") +
  ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, 700000)) +
  ggplot2::theme_bw()


#### compare to menhaden data from last year

menhaden2025 <- readRDS(
  "C:\\Users\\abigail.tyrell\\Downloads\\menhadenEOF.rds"
) |>
  dplyr::select(-NEUScatch, -units) |>
  tidyr::pivot_longer(
    cols = c(GOMcatch, MABcatch),
    names_to = "EPU",
    values_to = "metric_tons"
  ) |>
  dplyr::mutate(
    source = "ecodata2025",
    EPU = dplyr::case_when(EPU == "GOMcatch" ~ "GOM", EPU == "MABcatch" ~ "MAB")
  )

all_menhaden |>
  dplyr::mutate(source = "foss") |>
  dplyr::bind_rows(menhaden2025) |>
  # dplyr::filter(year <= 2000) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = metric_tons, color = source)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::facet_wrap(~EPU, ncol = 1, scales = "free") +
  ggplot2::theme_bw()

#### menhaden revenue ----

all_menhaden |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = dollars, color = EPU)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::theme_bw()


### total catch -- NE

total <- read.csv(
  here::here("data-raw/FOSS_landings_NE_total.csv"),
  skip = 1
) |>
  janitor::clean_names() |>
  tibble::as_tibble() |>
  dplyr::mutate(
    metric_tons = stringr::str_remove(metric_tons, ",") |>
      as.numeric(),
    year = as.numeric(year)
  )

total |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = metric_tons)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_x_continuous(limits = c(1982, NA)) +

  ggplot2::scale_y_continuous(labels = scales::comma, limits = c(NA, 350000)) +
  ggplot2::theme_bw()

ecodata_comdat <- ecodata::plot_comdat(report = "NewEngland")$data

ecodata_comdat |>
  dplyr::filter(Var == "Total") |>
  dplyr::group_by(Time) |>
  dplyr::summarise(metric_tons = sum(Value) * 10^3) |>
  dplyr::mutate(source = "ecodata") |>
  dplyr::bind_rows(
    total |>
      dplyr::mutate(source = "foss") |>
      dplyr::select(year, metric_tons, source) |>
      dplyr::rename(Time = year)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = metric_tons, color = source)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_x_continuous(limits = c(1982, NA)) +

  ggplot2::scale_y_continuous(labels = scales::comma, limits = c(NA, 350000)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Landings in New England")
