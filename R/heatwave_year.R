#' Calculates the heatwave_year indicator
#'
#' Combines the surface SST, and the bottom temperature components
#' into a single indicator
#'
#'
#' @param input_path_bot Character string. Full path to GLORYS Daily Bottom Temperature input file (from GLORYS automation)
#' @param input_path_surf Character string. Full path to OISST Daily Bottom Temperature input file (from OISS automation)
#' @param detrend logical. If TRUE does a linear detrending of the data
#'
#' @return ecodata::heatwave data frame
#'
#' @examples
#' \dontrun{
#' # create the ecodata::heatwave_year indicator
#' create_heatwave_year(input_path_bot = "path/to/input/GLORYS_daily_bt_data.csv",
#'                 input_path_surf = "path/to/input/OISST_daily_data.csv",
#'                 detrend = T
#'                )
#'
#' }
#'
#' @importFrom dplyr `|>`
#'
#' @export

create_heatwave_year <- function(
  input_path_bot = NA,
  input_path_surf = NA,
  detrend = F
) {
  # create surface temperature portion of heatwave_year
  surface <- create_heatwave_year_surface(
    input_path_surf = input_path_surf,
    detrend = detrend
  )

  # create bottom temperature portion of heatwave_year
  bottom <- create_heatwave_year_bottom(
    input_path_bot = input_path_bot,
    detrend = detend
  )

  # combine into one data frame
  heatwave_year <- rbind(surface, bottom)

  return(heatwave_year)
}

#' Calculates surface temperature portion of heatwave data set for automated workflow
#'
#' This uses a static input file from Kim Hyde
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_surf Character string. Full path to OISST Daily Bottom Temperature input file (from OISS automation)
#' @param detrend logical. If TRUE does a linear detrending of the data
#'
#' @examples
#' \dontrun{
#' # create the surface temperature portion of the ecodata::heatwave indicator for 2025
#' create_heatwave_surface(input_path_surf = "path/to/input/sst_data.csv",
#'                         detrend = T)

#'
#' }
#'
#'
#' @return surface temperature portion of ecodata::heatwave data frame
#'
#'
#' @noRd

create_heatwave_year_surface <- function(
  input_path_surf
) {
  data <- read.csv(file.path(input_path_surf)) |>
    dplyr::rename(t = date, temp = sst.mean) |>
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = '%Y-%m-%d')
    ) |>
    dplyr::select(t, EPU, temp) |>
    tidyr::drop_na()

  if (detrend) {
    data <- data |>
      dplyr::group_by(EPU) |>
      dplyr::mutate(
        temp.detrend = residuals(lm(temp ~ t, na.action = na.exclude)) +
          mean(temp, na.rm = T)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-temp) |>
      dplyr::rename(temp = 'temp.detrend')
  }

  #heatwave events
  epu.names <- c('GB', 'GOM', 'MAB')
  epu.hw <- lapply(epu.names, function(x) {
    dat <- data |>
      dplyr::filter(EPU == x)
    end.date <- '2024-12-31'
    start.date <- '1982-01-01'
    ts <- heatwaveR::ts2clm(dat, climatologyPeriod = c(start.date, end.date))
    mhw <- heatwaveR::detect_event(ts)
    return(mhw)
  })

  ### Take just clim

  mhw <- lapply(epu.hw, function(x) {
    dat <- x$clim |>
      dplyr::mutate(Year = format(t, "%Y")) |>
      dplyr::filter(Year == max(Year))
    return(dat)
  })

  heatwave_year_surface <- dplyr::bind_rows(mhw) |>
    dplyr::mutate(Var = ifelse(detrend, "SurfaceDetrended", 'SurfaceTrend')) |>
    dplyr::as_tibble()

  return(heatwave_year_surface)
}


#' Calculates bottom temperature portion of heatwave_year data set for automated workflow
#'
#' This uses a static input file from Joe Caracappa
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_bot Character string. Full path to GLORYS Daily Bottom Temperature input file (from GLORYS automation)
#' @param detrend logical. If TRUE does a linear detrending of the data
#'
#' @examples
#' \dontrun{
#' # create the bottom temperature portion of the ecodata::heatwave_year indicator for 2025
#' create_heatwave_year_bottom(input_path_bot = "path/to/input/bottom_temp_data.csv",
#'                        detrend = T)
#'
#' }
#'
#'
#' @return bottom temperature portion of ecodata::heatwave_year data frame
#'
#'
#' @importFrom dplyr `|>`
#'
#' @noRd

create_heatwave_year_bottom <- function(
  input_path_bot,
  detrend = F
) {
  #Bottom heatwave detrended
  data <- read.csv(file.path(input_path_bot), header = T) |>
    dplyr::rename(t = date, temp = sst.mean) |>
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = '%Y-%m-%d')
    ) |>
    dplyr::select(t, EPU, temp) |>
    tidyr::drop_na()

  if (detrend) {
    data <- data |>
      dplyr::group_by(EPU) |>
      dplyr::mutate(
        temp.detrend = residuals(lm(temp ~ t, na.action = na.exclude)) +
          mean(temp, na.rm = T)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-temp) |>
      dplyr::rename(temp = 'temp.detrend')
  }

  #heatwave events
  epu.names <- c('GB', 'GOM', 'MAB')
  epu.hw <- lapply(epu.names, function(x) {
    dat <- data |>
      dplyr::filter(EPU == x)
    end.date <- '2024-12-31'
    start.date <- '1982-01-01'
    ts <- heatwaveR::ts2clm(dat, climatologyPeriod = c(start.date, end.date))
    mhw <- heatwaveR::detect_event(ts, minDuration = 30)
    return(mhw)
  })

  ### Take just clim

  mhw <- lapply(epu.hw, function(x) {
    dat <- x$clim |>
      dplyr::mutate(Year = format(t, "%Y")) |>
      dplyr::filter(Year == max(Year))
    return(dat)
  })

  heatwave_year_bottom <- dplyr::bind_rows(mhw) |>
    dplyr::mutate(Var = ifelse(detrend, "BottomDetrended", 'BottomTrend')) |>
    dplyr::as_tibble()

  return(heatwave_year_bottom)
}
