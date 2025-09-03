#' Calculates bottom temperature portion of heatwave_year data set for automated workflow
#'
#' This uses a static input file from Joe Caracappa
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathGB Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param inputPathGOM Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param inputPathMAB Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#'
#' @examples
#' \dontrun{
#' # create the bottom temperature portion of the ecodata::heatwave_year indicator for 2025
#' create_heatwave_year_bottom(inputPathGB = "path/to/input/GBdata.csv",
#'                        inputPathGOM = "path/to/input/GOMdata.csv",
#'                        inputPathMAB = "path/to/input/MABdata.csv")
#'
#' }
#'
#'
#' @return bottom temperature portion of ecodata::heatwave_year data frame
#'
#' @export

create_heatwave_year_bottom <- function(inputPathGB, inputPathGOM, inputPathMAB) {
  
  ## Define inputs
  bheatwave_gbd <- read.csv(inputPathGB)
  bheatwave_gomd <- read.csv(inputPathGOM)
  bheatwave_mabd <- read.csv(inputPathMAB)
  
  #Bottom heatwave detrended
  gom<-read.csv(file.path(raw.dir,bheatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  gb<-read.csv(file.path(raw.dir,bheatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  mab<-read.csv(file.path(raw.dir,bheatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gb.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gom.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  mab.mhw <- heatwaveR::detect_event(ts, minDuration = 30)

  ### Take just clim
  #GB
  mhw<- gb.mhw$clim %>%
    mutate(EPU = c("GB"),
           Year = format(gb.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.gb.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  #GOM
  mhw<- gom.mhw$clim %>%
    mutate(EPU = c("GOM"),
           Year = format(gom.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.gom.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  #MAB
  mhw<- mab.mhw$clim %>%
    mutate(EPU = c("MAB"),
           Year = format(mab.mhw$clim$t, "%Y")) #extract year from date column; create year and epu columns
  mhw.mab.year <- dplyr::filter(mhw, mhw$Year == max(mhw$Year)) #subset most recent year

  heatwave_year_bottom<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "BottomDetrended")
  
  return(heatwave_year_bottom)
  
}