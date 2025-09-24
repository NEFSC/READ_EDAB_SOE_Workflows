#' Function to analyze Massachusetts inshore bottom trawl survey for automated workflow
#'
#' Data include aggregated time series of inshore fishery-independent trawl survey data from Massachusetts waters.
#' MA inshore surveys have been performed biannually in the spring and fall since 1978.
#'
#' @param inputPathMassSurvey Character string. Full path to the survdat data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#' @param staticPath Character string. Path to folder for static files
#' 
#'
#' @examples
#' \dontrun{
#' # create the ecodata::ma_inshore_survey indicator
#' create_mass_inshore_survey(inputPathSurvey <- here::here("MassSurvey.rds"), #### this gets created by SOEworkflows::get_survey_data, it doesn't
#'                          inputPathSpecies <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds",
#'                          outputPath <- here::here())
#'
#' }
#'
#'
#' @return ecodata::ma_inshore_survey data frame
#'
#' @export

######### To do --> update shapefile filepath, see create_ma survey script adjacent here (andy edited 10 mo ago)
### make any changes to match syntax
# what to do about static path (probably don't need, except for maybe shapefile) 
#input path mass survey data not pulled by get_survey function...maybe shouyld have two functions then one to get data from channel pull and one to do the rest

# packages --> data.table, survdat, sf,

# Helper function
sqltext <- function(x){
  out <- x[1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      out <- paste(out, x[i], sep = "','")
    }
  }
  out <- paste("'", out, "'", sep = '')
  return(out)
}

# Main function
create_mass_inshore_survey <- function(inputPathSurvey, inputPathSpecies, end.year) {
  
  end.year <- format(Sys.Date(),"%Y")

  # Read survey data & species-------------------------------------------
  survdat.mass <- readRDS(inputPathSurvey)
  
  # Read species list -------------
  Spp.list <- readRDS(inputPathSpecies) 
  
  # Pull Data----
  #All of this to be replaced with input path to the data pull
  
  channel <- dbutils::connect_to_database("NEFSC_pw_oraprod","amolina")  
  cruise.qry <- "select unique year, cruise6, svvessel, season
                 from svdbs.mstr_cruise
                 where purpose_code = 11
                 and year >= 1963
                 order by year, cruise6"
  
  
  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)
  
  #Use cruise codes to select other data
  cruise6 <- sqltext(cruise$CRUISE6)
  
  #Station data
  station.qry <- "select unique cruise6, svvessel, station, stratum, tow,
                        beglat as lat, beglon as lon,
                        begin_est_towdate as est_towdate, avgdepth as depth,
                        surftemp, bottemp,surfsalinity,botsalinity
                        from svdbs.fscs_svsta
                        where cruise6 = cruise6
                        and SHG <= 136
                        order by cruise6, station"

  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))
  
  #merge cruise and station
  survdat.mass <- merge(cruise, station)
  
  #Catch data
  catch.qry <- "select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from svdbs.FSCS_SVCAT
                     where cruise6 = cruise6
                     and stratum not like 'YT%'
                     order by cruise6, station, svspp"
  
  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  
  #merge with survdat
  data.table::setkey(survdat.mass, CRUISE6, STATION, STRATUM, TOW)
  survdat.mass <- merge(survdat.mass, catch, by = data.table::key(survdat.mass))
  
  #Convert number fields from chr to num
  numberCols <- c('CRUISE6', 'STATION', 'STRATUM', 'TOW', 'SVSPP', 'CATCHSEX', 'YEAR')
  survdat.mass[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols][]
  
  #survdat.mass <- survdat.mass |>dplyr::filter(YEAR <= end.year)
  
  #survdat.mass <- readRDS(inputPath1)$survdat
  
  # Aggregate species----
  
  #Merge species list with to get species group
  survdat.mass <- merge(survdat.mass, unique(Spp.list[, list(SVSPP, SOE.24)]),
                        by = 'SVSPP', all.x = T)
  
  #Grab strata 
  #strata <- sf::st_read(dsn = here::here('gis', 'RA_STRATA_POLY_MC.shp'), quiet = T)
  strata <- sf::st_read(dsn = "C:/Users/Adelle.molina/Documents/GitHub/SOE_data/gis/RA_STRATA_POLY_MC.shp", quiet = TRUE)
  strata <- sf::st_read(dsn = system.file("data", "EPU.shp", package = "NEFSCspatial"), quiet = T) 
  
  #fix strata numbers
  strata$massstratum <- as.numeric(paste0(9, strata$stratum, 0))
  #Give extra stratum column to survdat
  survdat.mass[, massstratum := STRATUM]
  
  #Generate area table
  strat.area <- survdat::get_area(strata, 'massstratum') # this pings the following: Spherical geometry (s2) switched off
  
  #Station data - Finds list of distinct stations sampled through time
  data.table::setkey(survdat.mass, CRUISE6, STRATUM, STATION)
  stations <- unique(survdat.mass, by = data.table::key(survdat.mass))
  stations <- stations[, list(YEAR, CRUISE6, STRATUM, STATION)]
  
  # Count the number of stations in each year for each Region
  data.table::setkey(stations, YEAR, CRUISE6, STRATUM)
  stations[, ntows := length(STATION), by = data.table::key(stations)]
  
  #Merge stations and area
  stations <- base::merge(stations, strat.area, by = 'STRATUM', all.x = T)
  
  #Calculate stratum weight
  data.table::setkeyv(stations, c('YEAR', 'CRUISE6', 'STRATUM'))
  strat.year <- unique(stations, by = data.table::key(stations))
  strat.year[, c('STATION', 'ntows') := NULL]
  data.table::setnames(strat.year, 'Area', 'S.Area')
  strat.year[, W.h := S.Area / sum(S.Area, na.rm = T), by = c('YEAR', 'CRUISE6')]
  strat.year[, W.h := as.vector(W.h)] #Drops the units from the area
  strat.year[is.na(W.h), W.h := 0]
  
  #Merge back
  stations <- merge(stations, strat.year, by = data.table::key(stations))
  
  #Merge catch with station data
  prepData <- merge(survdat.mass, stations, by = c('YEAR', 'CRUISE6', 'STRATUM',
                                                   'STATION'))
  
  prepData[, S.Area := NULL]
  
  #Calculate stratified mean
  stratmeanData <- survdat:::strat_mean(prepData, groupDescription = 'SOE.24', ##### see how other scripts in workflow example handle terminal year hardcoding
                                        mergesexFlag = T, seasonFlag = T,
                                        areaDescription = 'STRATUM',
                                        poststratFlag = F)
  
  
  # Prepare for SOE ------

    #Remove missing values
  stratmeanData <- stratmeanData[!is.na(stratmeanData$SOE.24), ]
  
  # select biomass and biomass SE for both spring and fall for each guild
  mass.survey <- stratmeanData |>
    dplyr::mutate(YEAR = as.integer(YEAR),
                  Units = "kg tow^-1") |>
    dplyr::rename(Time = YEAR,
                  Guild = SOE.24,
                  #Biomass.Index=strat.biomass,
                  #Biomass SE = biomass.SE
    ) |> 
    tidyr::pivot_longer(cols = c(strat.biomass, biomass.SE),
    names_to = "Variable_Type",
    values_to = "Value"
  )|>
    dplyr::mutate(Var =  dplyr::case_when(
      Variable_Type == "strat.biomass" ~ paste0(Guild, " ", SEASON, " Biomass Index - MA"),
      Variable_Type == "biomass.SE" ~ paste0(Guild, " ", SEASON, " Biomass SE - MA")
      )
    )|>
    dplyr::select(Time, Var, Value, Units)

  # fill in Time, Var, EPUs that are missing
  expanded <- expand.grid(Time = min(mass.survey$Time):end.year,Var = unique(mass.survey$Var))
  mass.survey <- expanded |>
    dplyr::left_join(mass.survey, by = c("Time", "Var"))
    #dplyr::as_tibble() |> 
    #dplyr::relocate(Time,Var,Value,EPU,Units)

  return(mass.survey)
  
}

