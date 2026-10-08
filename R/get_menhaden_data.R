# Metadata ----

### Project name: Menhaden data update
### Code purpose: Update menhaden data as a precursor to comdat workflow

### Author: AST
### Date started: 2026-10-08

### Code reviewer:
### Date reviewed:

# Analysis ----

get_menhaden_data <- function(api = FALSE) {
  if (api == TRUE) {
    message(
      "You set api = TRUE but the API methods don't exist yet. USing local files instead."
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
    # Report format = Totals by year
    foss_region <- read.csv(
      here::here("data-raw\\FOSS_landings.csv"),
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
      here::here("data-raw\\FOSS_landings_NC.csv"),
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

    accsp <- read.csv(here::here("data-raw\\accsp_menhaden_SOE2027.csv")) |>
      janitor::clean_names() |>
      dplyr::mutate(
        metric_tons = pounds / 2204.62
      ) |>
      dplyr::filter(year > 2015)
  }

  output <- foss_region |>
    dplyr::select(year, metric_tons, dollars) |>
    dplyr::bind_rows(
      foss_nc |>
        dplyr::select(year, metric_tons, dollars)
    ) |>
    dplyr::mutate(source = "foss") |>
    dplyr::bind_rows(
      accsp |>
        dplyr::select(year, metric_tons, dollars) |>
        dplyr::mutate(source = "accsp")
    ) |>
    dplyr::group_by(year, source) |>
    dplyr::summarise(
      metric_tons = sum(metric_tons, na.rm = TRUE),
      dollars = sum(dollars, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-source) |>
    dplyr::mutate(species = "Menhadens")

  return(output)
}
