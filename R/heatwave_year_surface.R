#' Calculates surface temperature portion of heatwave data set for automated workflow
#'
#' This uses a static input file from Kim Hyde
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathGB Character string. Full path to the GB GLORYS input file from Kim Hyde
#' @param inputPathGOM Character string. Full path to the GOM GLORYS input file from Kim Hyde
#' @param inputPathMAB Character string. Full path to the MAB GLORYS input file from Kim Hyde
#'
#' @examples
#' \dontrun{
#' # create the surface temperature portion of the ecodata::heatwave indicator for 2025
#' create_heatwave_surface(inputPathGB = "path/to/input/GBdata.csv",
#'                         inputPathGOM = "path/to/input/GOMdata.csv",
#'                         inputPathMAB = "path/to/input/MABdata.csv")
#'
#' }
#'
#'
#' @return surface temperature portion of ecodata::heatwave data frame
#'
#' @export

create_heatwave_year_surface <- function(inputPathGB, inputPathGOM, inputPathMAB) {
  
  ## Load dplyr to use %>% pipes (will be removed later)
  library(dplyr)
  
  ## Surface Detrended
  heatwave_gbd <- inputPathGB
  heatwave_gomd <- inputPathGOM
  heatwave_mabd <- inputPathMAB

  # SURFACE DETRENDED
  gom<-read.csv(file.path(heatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  gb<-read.csv(file.path(heatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  mab<-read.csv(file.path(heatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(t, detrended) %>%
    dplyr::rename(temp = detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()

  #GB
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)

  #GOM
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)

  #MAB
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)

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

  heatwave_year_surface<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year) %>%
    dplyr::mutate(Var = "SurfaceDetrended")
  
  return(heatwave_year_surface)
  
}