#' Calculates the heatwave_year indicator
#'
#' Combines the surface SST, and the bottom temperature components
#' into a single indicator
#'
#'
#' @param input_path_gb_bot Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param input_path_gom_bot Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param input_path_mab_bot Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#' @param input_path_gb_surf Character string. Full path to the GB OISST input file from Kim Hyde
#' @param input_path_gom_surf Character string. Full path to the GOM OISST input file from Kim Hyde
#' @param input_path_mab_surf Character string. Full path to the MAB OISST input file from Kim Hyde
#'
#' @return ecodata::heatwave data frame
#'
#' @examples
#' \dontrun{
#' # create the ecodata::heatwave_year indicator
#' create_heatwave_year(input_path_gb_bot = "path/to/input/GBdata.csv",
#'                         input_path_gom_bot = "path/to/input/GOMdata.csv",
#'                         input_path_mab_bot = "path/to/input/MABdata.csv",
#'                         input_path_gb_surf = "path/to/input/GBdata.csv",
#'                         input_path_gom_surf = "path/to/input/GOMdata.csv",
#'                         input_path_mab_surf = "path/to/input/MABdata.csv")
#'
#' }
#'
#' @importFrom dplyr `%>%`
#'
#' @export

create_heatwave_year <- function(
  input_path_gb_bot,
  input_path_gom_bot,
  input_path_mab_bot,
  input_path_gb_surf,
  input_path_gom_surf,
  input_path_mab_surf
) {
  # create surface temperature portion of heatwave_year
  surface <- create_heatwave_year_surface(
    input_path_gb_surf = input_path_gb_surf,
    input_path_gom_surf = input_path_gom_surf,
    input_path_mab_surf = input_path_mab_surf
  )

  # create bottom temperature portion of heatwave_year
  bottom <- create_heatwave_year_bottom(
    input_path_gb_bot = input_path_gb_bot,
    input_path_gom_bot = input_path_gom_bot,
    input_path_mab_bot = input_path_mab_bot
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
#' @param input_path_gb_surf Character string. Full path to the GB GLORYS input file from Kim Hyde
#' @param input_path_gom_surf Character string. Full path to the GOM GLORYS input file from Kim Hyde
#' @param input_path_mab_surf Character string. Full path to the MAB GLORYS input file from Kim Hyde
#'
#' @examples
#' \dontrun{
#' # create the surface temperature portion of the ecodata::heatwave indicator for 2025
#' create_heatwave_surface(input_path_gb_surf = "path/to/input/GBdata.csv",
#'                         input_path_gom_surf = "path/to/input/GOMdata.csv",
#'                         input_path_mab_surf = "path/to/input/MABdata.csv")
#'
#' }
#'
#'
#' @return surface temperature portion of ecodata::heatwave data frame
#'
#' @importFrom dplyr `%>%`
#'
#' @noRd

create_heatwave_year_surface <- function(
  input_path_gb_surf,
  input_path_gom_surf,
  input_path_mab_surf
) {
  ## Surface Detrended
  heatwave_gbd <- input_path_gb_surf
  heatwave_gomd <- input_path_gom_surf
  heatwave_mabd <- input_path_mab_surf

  # SURFACE DETRENDED
  gom <- read.csv(file.path(heatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = "%m/%d/%Y")
    ) %>%
    tidyr::drop_na()

  gb <- read.csv(file.path(heatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = "%m/%d/%Y")
    ) %>%
    tidyr::drop_na()

  mab <- read.csv(file.path(heatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = "%m/%d/%Y")
    ) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)

  #GOM
  ts <- heatwaveR::ts2clm(
    gom,
    climatologyPeriod = c("1982-01-01", "2024-12-31")
  )
  gom.mhw <- heatwaveR::detect_event(ts)

  #MAB
  ts <- heatwaveR::ts2clm(
    mab,
    climatologyPeriod = c("1982-01-01", "2024-12-31")
  )
  mab.mhw <- heatwaveR::detect_event(ts)

  ### Take just clim
  #GB
  mhw <- gb.mhw$clim %>%
    dplyr::mutate(EPU = c("GB"), Year = format(gb.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.gb.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  #GOM
  mhw <- gom.mhw$clim %>%
    dplyr::mutate(EPU = c("GOM"), Year = format(gom.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.gom.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  #MAB
  mhw <- mab.mhw$clim %>%
    dplyr::mutate(EPU = c("MAB"), Year = format(mab.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.mab.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  heatwave_year_surface <- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "SurfaceDetrended") |>
    dplyr::as_tibble()

  return(heatwave_year_surface)
}


#' Calculates bottom temperature portion of heatwave_year data set for automated workflow
#'
#' This uses a static input file from Joe Caracappa
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_gb_bot Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param input_path_gom_bot Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param input_path_mab_bot Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#'
#' @examples
#' \dontrun{
#' # create the bottom temperature portion of the ecodata::heatwave_year indicator for 2025
#' create_heatwave_year_bottom(input_path_gb_bot = "path/to/input/GBdata.csv",
#'                        input_path_gom_bot = "path/to/input/GOMdata.csv",
#'                        input_path_mab_bot = "path/to/input/MABdata.csv")
#'
#' }
#'
#'
#' @return bottom temperature portion of ecodata::heatwave_year data frame
#'
#'
#' @importFrom dplyr `%>%`
#'
#' @noRd

create_heatwave_year_bottom <- function(
  input_path_gb_bot,
  input_path_gom_bot,
  input_path_mab_bot
) {
  ## Define inputs
  bheatwave_gbd <- input_path_gb_bot
  bheatwave_gomd <- input_path_gom_bot
  bheatwave_mabd <- input_path_mab_bot

  #Bottom heatwave detrended
  gom <- read.csv(file.path(bheatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = "%m/%d/%Y")
    ) %>%
    tidyr::drop_na()

  gb <- read.csv(file.path(bheatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = "%m/%d/%Y")
    ) %>%
    tidyr::drop_na()

  mab <- read.csv(file.path(bheatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(
      temp = as.numeric(temp),
      t = as.Date(t, format = "%m/%d/%Y")
    ) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gb.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  #GOM
  ts <- heatwaveR::ts2clm(
    gom,
    climatologyPeriod = c("1982-01-01", "2024-11-26")
  )
  gom.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  #MAB
  ts <- heatwaveR::ts2clm(
    mab,
    climatologyPeriod = c("1982-01-01", "2024-11-26")
  )
  mab.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  ### Take just clim
  #GB
  mhw <- gb.mhw$clim %>%
    dplyr::mutate(EPU = c("GB"), Year = format(gb.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.gb.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  #GOM
  mhw <- gom.mhw$clim %>%
    dplyr::mutate(EPU = c("GOM"), Year = format(gom.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.gom.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  #MAB
  mhw <- mab.mhw$clim %>%
    dplyr::mutate(EPU = c("MAB"), Year = format(mab.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.mab.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  heatwave_year_bottom <- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "BottomDetrended") |>
    dplyr::as_tibble()

  return(heatwave_year_bottom)
}
