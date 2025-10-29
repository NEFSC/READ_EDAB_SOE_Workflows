#' Calculates the heatwave indicator 
#' 
#' Combines the surface SST, and the bottom temperature components
#' into a single indicator
#' 
#' 
#' @param inputPathGBBot Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param inputPathGOMBot Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param inputPathMABBot Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#' @param inputPathGBSurf Character string. Full path to the GB OISST input file from Kim Hyde
#' @param inputPathGOMSurf Character string. Full path to the GOM OISST input file from Kim Hyde
#' @param inputPathMABSurf Character string. Full path to the MAB OISST input file from Kim Hyde
#'
#' @return ecodata::heatwave data frame 
#'
#' @examples
#' \dontrun{
#' }
#' 
#' @importFrom dplyr `%>%`
#' 
#' @export

create_heatwave <- function(inputPathGBBot,
                            inputPathGOMBot, 
                            inputPathMABBot,
                            inputPathGBSurf,
                            inputPathGOMSurf, 
                            inputPathMABSurf) {
  
  # create surface temperature portion of heatwave
  surface <- create_heatwave_surface(inputPathGB = inputPathGBSurf,
                                     inputPathGOM = inputPathGOMSurf, 
                                     inputPathMAB = inputPathMABSurf)

  # create bottom temperature portion of heatwave
  bottom <- create_heatwave_bottom(inputPathGB = inputPathGBBot,
                                   inputPathGOM = inputPathGOMBot, 
                                   inputPathMAB = inputPathMABBot)
  
  heatwave <- rbind(surface,bottom)
  
  # Expand grid and join to dataset
  heatwave_zeros <- expand.grid(Time = unique(heatwave$Time),
                                Var = unique(heatwave$Var),
                                EPU = unique(heatwave$EPU)) |> 
    dplyr::mutate(Value2 = 0)
  
  heatwave<- heatwave %>% 
    dplyr::right_join(heatwave_zeros) %>%
    dplyr::mutate(Value = dplyr::case_when(is.na(Value)~Value2,
                                    TRUE ~ Value)) %>%
    dplyr::select(!Value2)
  
  # Add Units label to "padded" data


  return(heatwave)
}



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
#' @importFrom dplyr `%>%`
#' @export

create_heatwave_surface <- function(inputPathGB, inputPathGOM, inputPathMAB) {
  
  
  ## Define inputs
  heatwave_gbd <- inputPathGB
  heatwave_gomd <- inputPathGOM
  heatwave_mabd <- inputPathMAB
  
  # SURFACE DETRENDED
  # Process input files
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
  
  # GB - define climatology, detect events
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gb.mhw <- heatwaveR::detect_event(ts)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")
  
  # GOM - define climatology, detect events
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  gom.mhw <- heatwaveR::detect_event(ts)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")
  
  # MAB - define climatology, detect events
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-12-31"))
  mab.mhw <- heatwaveR::detect_event(ts)
  mab.hw<- mab.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "MAB")
  
  # Cumulative intensity
  cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")),
                  intensity_cumulative = as.numeric(intensity_cumulative)) %>%
    dplyr::group_by(Time, EPU) %>%
    dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
    dplyr::mutate(Var = "cumulative intensity") %>%
    dplyr::ungroup()
  
  # Max intensity
  max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")),
                  intensity_max = as.numeric(intensity_max))  %>%
    dplyr::rename(Value = intensity_max) %>%
    dplyr::mutate(Var = "maximum intensity")%>%
    dplyr::select(Time, EPU, Value, Var)%>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()
  
  # Duration
  duration <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")),
                  duration = as.numeric(duration))  %>%
    dplyr::rename(Value = duration) %>%
    dplyr::mutate(Var = "duration")%>%
    dplyr::select(Time, EPU, Value, Var) %>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Units = "N days",
                  Time = as.numeric(Time),
                  Var  = paste0(Var, "-SurfaceDetrended"))
  
  # Create SOE data object
  heatwave_surface<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time),
                   Var  = paste0(Var, "-SurfaceDetrended")) %>%
    rbind(duration) %>%
    dplyr::select(Time, Var, Value, EPU, Units)
  
  # # Expand grid and join to dataset
  # heatwave_zeros <- expand.grid(Time = unique(heatwave_surface$Time),
  #                               Var = unique(heatwave_surface$Var),
  #                               EPU = unique(heatwave_surface$EPU)) %>%
  #   dplyr::mutate(Value2 = 0)
  # 
  # heatwave_surface<- heatwave_surface %>% 
  #   dplyr::right_join(heatwave_zeros) %>%
  #   dplyr::mutate(Value = dplyr::case_when(is.na(Value)~Value2,
  #                                          TRUE ~ Value)) %>%
  #   dplyr::select(!Value2)
  
  return(heatwave_surface)
  
}



















#' Calculates bottom temperature portion of heatwave data set for automated workflow
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
#' # create the bottom temperature portion of the ecodata::heatwave indicator for 2025
#' create_heatwave_bottom(inputPathGB = "path/to/input/GBdata.csv",
#'                        inputPathGOM = "path/to/input/GOMdata.csv",
#'                        inputPathMAB = "path/to/input/MABdata.csv")
#'
#' }
#'
#'
#' @return bottom temperature portion of ecodata::heatwave data frame
#'
#' @importFrom dplyr `%>%`
#'
#' @export

create_heatwave_bottom <- function(inputPathGB, inputPathGOM, inputPathMAB) {
  
  ## Define inputs
  bheatwave_gbd <- inputPathGB
  bheatwave_gomd <- inputPathGOM
  bheatwave_mabd <- inputPathMAB
  
  # BOTTOM DETRENDED
  # Process input files
  gom<-read.csv(file.path(bheatwave_gomd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()
  
  gb<-read.csv(file.path(bheatwave_gbd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()
  
  mab<-read.csv(file.path(bheatwave_mabd), header = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::select(date, Detrended) %>%
    dplyr::rename(t = date, temp = Detrended) %>%
    dplyr::filter(!temp == "temp") %>%
    dplyr::mutate(temp = as.numeric(temp),
                  t = as.Date(t, format = "%m/%d/%Y")) %>%
    tidyr::drop_na()
  
  # GB - define climatology, detect events
  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gb.mhw <- heatwaveR::detect_event(ts, minDuration = 30)
  gb.hw<- gb.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
    dplyr::mutate(EPU = "GB")
  
  # GOM - define climatology, detect events
  ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  gom.mhw <- heatwaveR::detect_event(ts, minDuration = 30)
  gom.hw<- gom.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "GOM")
  
  # MAB - define climatology, detect events
  ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2024-11-26"))
  mab.mhw <- heatwaveR::detect_event(ts, minDuration = 30)
  mab.hw<- mab.mhw$event %>%
    dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
    dplyr::mutate(EPU = "MAB")
  
  # Cumulative intensity
  cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
    dplyr::group_by(Time, EPU) %>%
    dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
    dplyr::mutate(Var = "cumulative intensity") %>%
    dplyr::ungroup()
  
  # Max intensity
  max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
    dplyr::rename(Value = intensity_max) %>%
    dplyr::mutate(Var = "maximum intensity")%>%
    dplyr::select(Time, EPU, Value, Var)%>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup()
  
  # Duration
  duration <- rbind(gb.hw, gom.hw, mab.hw) %>%
    dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
    dplyr::rename(Value = duration) %>%
    dplyr::mutate(Var = "duration")%>%
    dplyr::select(Time, EPU, Value, Var) %>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = max(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Units = "N days",
                  Time = as.numeric(Time),
                  Var  = paste0(Var, "-BottomDetrended"))
  
  # Create SOE data object
  heatwave_bottom<- rbind(cum.intensity, max.intensity) %>%
    dplyr:: mutate(Units = "degrees C",
                   Time = as.numeric(Time),
                   Var  = paste0(Var, "-BottomDetrended")) %>%
    rbind(duration) %>%
    dplyr::select(Time, Var, Value, EPU, Units)
  
  # # Expand grid and join to dataset
  # heatwave_zeros <- expand.grid(Time = unique(heatwave_bottom$Time),
  #                               Var = unique(heatwave_bottom$Var),
  #                               EPU = unique(heatwave_bottom$EPU)) %>%
  #   dplyr::mutate(Value2 = 0)
  # 
  # heatwave_bottom<- heatwave_bottom %>% 
  #   dplyr::right_join(heatwave_zeros) %>%
  #   dplyr::mutate(Value = dplyr::case_when(is.na(Value)~Value2,
  #                                          TRUE ~ Value)) %>%
  #   dplyr::select(!Value2)
  
  return(heatwave_bottom)
  
}