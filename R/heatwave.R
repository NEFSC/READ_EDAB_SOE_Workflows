#' Calculates the heatwave indicator
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
#'# create the ecodata::heatwave indicator
#' create_heatwave(input_path_bot = "path/to/input/GLORYS_daily_bt_data.csv",
#'                 input_path_surf = "path/to/input/OISST_daily_data.csv"
#'                )
#'
#' }
#'
#'
#' @export

create_heatwave <- function(
  input_path_bot = NA,
  input_path_surf = NA,
  detrend = F
) {
  # create surface temperature portion of heatwave
  if (!is.na(input_path_surf)) {
    surface <- create_heatwave_surface(
      input_path_surf = input_path_surf,
      detrend = detrend
    )
  } else {
    bottom <- NULL
  }

  # create bottom temperature portion of heatwave
  if (!is.na(input_path_bot)) {
    bottom <- create_heatwave_bottom(
      input_path_bot = input_path_bot,
      detrend = detrend
    )
  } else {
    bottom <- NULL
  }

  heatwave <- rbind(surface, bottom)

  # Expand grid and join to dataset
  heatwave_zeros <- expand.grid(
    Time = unique(heatwave$Time),
    Var = unique(heatwave$Var),
    EPU = unique(heatwave$EPU)
  ) |>
    dplyr::mutate(Value2 = 0)

  heatwave <- heatwave |>
    dplyr::right_join(heatwave_zeros) |>
    dplyr::mutate(
      Value = dplyr::case_when(is.na(Value) ~ Value2, TRUE ~ Value)
    ) |>
    dplyr::select(!Value2)

  # Add Units label to "padded" data

  return(heatwave)
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
#' create_heatwave_surface(input_path_surf = "path/to/input/OISST_daily_data.csv")
#'
#' }
#'
#'
#' @return surface temperature portion of ecodata::heatwave data frame
#'
#' @noRd

create_heatwave_surface <- function(
  input_path_surf,
  detrend = F
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
      dplyr::ungroup()
  }

  # Define Climatology, detect events

  epu.names <- c('GB', 'GOM', 'MAB')
  epu.hw <- lapply(epu.names, function(x) {
    dat <- data |>
      dplyr::filter(EPU == x)
    end.date <- as.character(max(dat$t))
    start.date <- max(c(as.Date('1982-01-01'), min(dat$t)))
    ts <- heatwaveR::ts2clm(dat, climatologyPeriod = c(start.date, end.date))
    mhw <- heatwaveR::detect_event(ts, minDuration = 30)
    hw <- mhw$event |>
      dplyr::select(
        event_no,
        duration,
        date_start,
        date_peak,
        intensity_max,
        intensity_cumulative
      ) |>
      dplyr::mutate(EPU = x)
  }) |>
    dplyr::bind_rows()

  # Cumulative intensity
  cum.intensity <- epu.hw |>
    dplyr::mutate(
      Time = as.numeric(format(as.Date(date_start, format = "%Y-%m-%d"), "%Y")),
      intensity_cumulative = as.numeric(intensity_cumulative)
    ) |>
    dplyr::group_by(Time, EPU) |>
    dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) |>
    dplyr::mutate(Var = "cumulative intensity") |>
    dplyr::ungroup()

  # Max intensity
  max.intensity <- epu.hw |>
    dplyr::mutate(
      Time = as.numeric(format(as.Date(date_start, format = "%Y-%m-%d"), "%Y")),
      intensity_max = as.numeric(intensity_max)
    ) |>
    dplyr::rename(Value = intensity_max) |>
    dplyr::mutate(Var = "maximum intensity") |>
    dplyr::select(Time, EPU, Value, Var) |>
    dplyr::group_by(Time, EPU, Var) |>
    dplyr::summarise(Value = max(Value)) |>
    dplyr::ungroup()

  # Duration
  duration <- epu.hw |>
    dplyr::mutate(
      Time = as.numeric(format(as.Date(date_start, format = "%Y-%m-%d"), "%Y")),
      duration = as.numeric(duration)
    ) |>
    dplyr::rename(Value = duration) |>
    dplyr::mutate(Var = "duration") |>
    dplyr::select(Time, EPU, Value, Var) |>
    dplyr::group_by(Time, EPU, Var) |>
    dplyr::summarise(Value = max(Value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Units = "N days",
      Time = as.numeric(Time),
      Var = paste0(Var, "-SurfaceDetrended")
    )

  # Create SOE data object
  heatwave_surface <- rbind(cum.intensity, max.intensity) |>
    dplyr::mutate(
      Units = "degrees C",
      Time = as.numeric(Time),
      Var = paste0(Var, "-SurfaceDetrended")
    ) |>
    rbind(duration) |>
    dplyr::select(Time, Var, Value, EPU, Units)

  # # Expand grid and join to dataset
  # heatwave_zeros <- expand.grid(Time = unique(heatwave_surface$Time),
  #                               Var = unique(heatwave_surface$Var),
  #                               EPU = unique(heatwave_surface$EPU)) |>
  #   dplyr::mutate(Value2 = 0)
  #
  # heatwave_surface<- heatwave_surface |>
  #   dplyr::right_join(heatwave_zeros) |>
  #   dplyr::mutate(Value = dplyr::case_when(is.na(Value)~Value2,
  #                                          TRUE ~ Value)) |>
  #   dplyr::select(!Value2)

  return(heatwave_surface)
}


#' Calculates bottom temperature portion of heatwave data set for automated workflow
#'
#' This uses a static input file from Joe Caracappa
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_bot Character string. Full path to GLORYS Daily Bottom Temperature input file (from GLORYS automation)
#' @param detrend logical. If TRUE does a linear detrending of the data
#'
#' @examples
#' \dontrun{
#' # create the bottom temperature portion of the ecodata::heatwave indicator for 2025
#' create_heatwave_bottom(input_path_bot = "path/to/input/GLORYS_daily_bt_data.csv")
#'
#' }
#'
#'
#' @return bottom temperature portion of ecodata::heatwave data frame
#'
#'
#' @noRd

create_heatwave_bottom <- function(input_path_bot, detrend = F) {
  # Read in inputs and format
  data <- read.csv(file.path(input_path_bot)) |>
    dplyr::rename(t = date, temp = BottomT.mean) |>
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
      dplyr::ungroup()
  }

  # ggplot(data, aes(x = t, y = temp,color = EPU ))+geom_line()
  # ggplot(data2, aes(x = t, y = temp.detrend,color = EPU ))+geom_line()

  #define climatology, detect events
  epu.names <- c('GB', 'GOM', 'MAB')
  epu.hw <- lapply(epu.names, function(x) {
    dat <- data |>
      dplyr::filter(EPU == x)
    end.date <- as.character(max(dat$t))
    ts <- heatwaveR::ts2clm(dat, climatologyPeriod = c("1982-01-01", end.date))
    mhw <- heatwaveR::detect_event(ts, minDuration = 30)
    hw <- mhw$event |>
      dplyr::select(
        event_no,
        duration,
        date_start,
        date_peak,
        intensity_max,
        intensity_cumulative
      ) |>
      dplyr::mutate(EPU = x)
  }) |>
    dplyr::bind_rows()

  # Cumulative intensity
  cum.intensity <- epu.hw |>
    dplyr::mutate(
      Time = as.numeric(format(as.Date(date_start, format = "%Y-%m-%d"), "%Y"))
    ) |>
    dplyr::group_by(Time, EPU) |>
    dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) |>
    dplyr::mutate(Var = "cumulative intensity") |>
    dplyr::ungroup()

  # Max intensity
  max.intensity <- epu.hw |>
    dplyr::mutate(
      Time = as.numeric(format(as.Date(date_start, format = "%Y-%m-%d"), "%Y"))
    ) |>
    dplyr::rename(Value = intensity_max) |>
    dplyr::mutate(Var = "maximum intensity") |>
    dplyr::select(Time, EPU, Value, Var) |>
    dplyr::group_by(Time, EPU, Var) |>
    dplyr::summarise(Value = max(Value)) |>
    dplyr::ungroup()

  # Duration
  duration <- epu.hw |>
    dplyr::mutate(
      Time = as.numeric(format(as.Date(date_start, format = "%Y-%m-%d"), "%Y"))
    ) |>
    dplyr::rename(Value = duration) |>
    dplyr::mutate(Var = "duration") |>
    dplyr::select(Time, EPU, Value, Var) |>
    dplyr::group_by(Time, EPU, Var) |>
    dplyr::summarise(Value = max(Value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Units = "N days",
      Time = as.numeric(Time),
      Var = paste0(Var, "-BottomDetrended")
    )

  # Create SOE data object
  heatwave_bottom <- rbind(cum.intensity, max.intensity) |>
    dplyr::mutate(
      Units = "degrees C",
      Time = as.numeric(Time),
      Var = paste0(Var, "-BottomDetrended")
    ) |>
    rbind(duration) |>
    dplyr::select(Time, Var, Value, EPU, Units)

  # # Expand grid and join to dataset
  # heatwave_zeros <- expand.grid(Time = unique(heatwave_bottom$Time),
  #                               Var = unique(heatwave_bottom$Var),
  #                               EPU = unique(heatwave_bottom$EPU)) |>
  #   dplyr::mutate(Value2 = 0)
  #
  # heatwave_bottom<- heatwave_bottom |>
  #   dplyr::right_join(heatwave_zeros) |>
  #   dplyr::mutate(Value = dplyr::case_when(is.na(Value)~Value2,
  #                                          TRUE ~ Value)) |>
  #   dplyr::select(!Value2)

  return(heatwave_bottom)
}
