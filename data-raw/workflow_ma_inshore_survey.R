#' Calculates mass inshore survey data set for automated workflow
#'
#' This uses the survdat data pull from the survey package
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathBigelow Character string. Full path to the Bigelow data pull rds file
#' @param inputPathAlbatross Character string. Full path to the Albatross data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::survey_shannon indicator
#' workflow_survey_shannon(inputPathBigelow = "path/to/Bigelow/data.rds",
#'                       inputPathAlbatros = "path/to/Albatross/data.rds",
#'                       outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::survey_shannon data frame
#'
#' @section Dependencies:
#' 
#' This assumes that the survey data has been pulled and resides in the path `inputPathBigelow` and `inputPathAlbatross`
#'
#' @export


#' Massachusetts inshore bottom trawl survey
#'

#'
#' @format 880 rows and 5 columns
#'
#' \itemize{
#'     \item Var: Specifies variable type, including SOE species groupings and sampling season with stratified mean biomass per tow
#'     \item Value: Value of variable \code{Var}.
#'     \item Time: Year.
#'     \item Units: Units of variable \code{Var}.
#'     \item EPU: Ecological Production Unit. These data reflect inshore sampling in MA state waters.
#' }
#'
#' @details
#' Technical details related to the MA inshore survey are available at \url{https://www.mass.gov/files/documents/2016/08/tm/tr-38.pdf}.
"mass_inshore_survey"





create_mass_inshore_survey <- function(channel, SOEreportYear, end.year,saveToFile =F) {
  
  #-------------------------------------------------------------------------------
  #Pull Data----
  
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
  station.qry <- paste("select unique cruise6, svvessel, station, stratum,
                        tow, decdeg_beglat as lat, decdeg_beglon as lon,
                        begin_est_towdate as est_towdate, avgdepth as depth,
                        surftemp, surfsalin, bottemp, botsalin
                        from Union_fscs_svsta
                        where cruise6 in (", cruise6, ")
                        and SHG <= 136
                        order by cruise6, station", sep='')
  
  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))
  
  #merge cruise and station
  survdat.mass <- merge(cruise, station)
  
  #Catch data
  catch.qry <- paste("select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from UNION_FSCS_SVCAT
                     where cruise6 in (", cruise6, ")
                     and stratum not like 'YT%'
                     order by cruise6, station, svspp", sep='')
  
  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  
  #merge with survdat
  setkey(survdat.mass, CRUISE6, STATION, STRATUM, TOW)
  survdat.mass <- merge(survdat.mass, catch, by = key(survdat.mass))
  
  #Convert number fields from chr to num
  numberCols <- c('CRUISE6', 'STATION', 'STRATUM', 'TOW', 'SVSPP', 'CATCHSEX', 'YEAR')
  survdat.mass[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols][]
  
  survdat.mass <- survdat.mass |>
    dplyr::filter(YEAR <= end.year)
  
  #Aggregate species----
  #Grab species list
  load(here::here('data-raw', 'SOE_species_list_24.RData'))
  
  #Merge to get species group
  survdat.mass <- merge(survdat.mass, unique(species[, list(SVSPP, SOE.24)]),
                        by = 'SVSPP', all.x = T)
  
  #Grab strata
  strata <- sf::st_read(dsn = here::here('gis', 'RA_STRATA_POLY_MC.shp'), quiet = T)
  #fix strata numbers
  strata$massstratum <- as.numeric(paste0(9, strata$stratum, 0))
  #Give extra stratum column to survdat
  survdat.mass[, massstratum := STRATUM]
  
  #Generate area table
  strat.area <- survdat::get_area(strata, 'massstratum')
  
  #Station data - Finds list of distinct stations sampled through time
  data.table::setkey(survdat.mass, CRUISE6, STRATUM, STATION)
  stations <- unique(survdat.mass, by = key(survdat.mass))
  stations <- stations[, list(YEAR, CRUISE6, STRATUM, STATION)]
  
  # Count the number of stations in each year for each Region
  data.table::setkey(stations, YEAR, CRUISE6, STRATUM)
  stations[, ntows := length(STATION), by = key(stations)]
  
  #Merge stations and area
  stations <- base::merge(stations, strat.area, by = 'STRATUM', all.x = T)
  
  #Calculate stratum weight
  data.table::setkeyv(stations, c('YEAR', 'CRUISE6', 'STRATUM'))
  strat.year <- unique(stations, by = key(stations))
  strat.year[, c('STATION', 'ntows') := NULL]
  data.table::setnames(strat.year, 'Area', 'S.Area')
  strat.year[, W.h := S.Area / sum(S.Area, na.rm = T), by = c('YEAR', 'CRUISE6')]
  strat.year[, W.h := as.vector(W.h)] #Drops the units from the area
  strat.year[is.na(W.h), W.h := 0]
  
  #Merge back
  stations <- merge(stations, strat.year, by = key(stations))
  
  #Merge catch with station data
  prepData <- merge(survdat.mass, stations, by = c('YEAR', 'CRUISE6', 'STRATUM',
                                                   'STATION'))
  
  prepData[, S.Area := NULL]
  
  #Calculate stratified mean
  stratmeanData <- survdat:::strat_mean(prepData, groupDescription = 'SOE.24',
                                        mergesexFlag = T, seasonFlag = T,
                                        areaDescription = 'STRATUM',
                                        poststratFlag = F)
  
  
  #------
  stratmeanData <- stratmeanData[!is.na(SOE.24), ]
  
  #Get in correct long format for SOE
  fall.out <- stratmeanData[SEASON == 'FALL', list(YEAR, strat.biomass, SOE.24)]
  fall.out[, Var := paste(SOE.24, 'Fall Biomass Index - MA')]
  fall.out[, Units  := 'kg tow^-1']
  fall.out[, Source := 'Mass inshore bottom trawl survey (survdat.mass)']
  setnames(fall.out, c('YEAR', 'strat.biomass'), c('Time', 'Value'))
  fall.out[, SOE.24 := NULL]
  
  spring.out <- stratmeanData[SEASON == 'SPRING', list(YEAR, strat.biomass, SOE.24)]
  spring.out[, Var := paste(SOE.24, 'Spring Biomass Index - MA')]
  spring.out[, Units  := 'kg tow^-1']
  spring.out[, Source := 'Mass inshore bottom trawl survey (survdat.mass)']
  setnames(spring.out, c('YEAR', 'strat.biomass'), c('Time', 'Value'))
  spring.out[, SOE.24 := NULL]
  
  fall.out.var <- stratmeanData[SEASON == 'FALL', list(YEAR, biomass.SE, SOE.24)]
  fall.out.var[, Var := paste(SOE.24, 'Fall Biomass SE - MA')]
  fall.out.var[, Units  := 'kg tow^-1']
  fall.out.var[, Source := 'Mass inshore bottom trawl survey (survdat.mass)']
  setnames(fall.out.var, c('YEAR', 'biomass.SE'), c('Time', 'Value'))
  fall.out.var[, SOE.24 := NULL]
  
  spring.out.var <- stratmeanData[SEASON == 'SPRING', list(YEAR, biomass.SE, SOE.24)]
  spring.out.var[, Var := paste(SOE.24, 'Spring Biomass SE - MA')]
  spring.out.var[, Units  := 'kg tow^-1']
  spring.out.var[, Source := 'Mass inshore bottom trawl survey (survdat.mass)']
  setnames(spring.out.var, c('YEAR', 'biomass.SE'), c('Time', 'Value'))
  spring.out.var[, SOE.24 := NULL]
  
  #Merge into one data set
  mass.survey <- rbindlist(list(fall.out, spring.out, fall.out.var, spring.out.var))
  
  if(saveToFile) {
    yr <- substring(SOEreportYear,3,4)
    save(mass.survey, file = here::here("data", paste0("Aggregate_Mass_Survey_biomass_",yr,".RData")))
  }
  
  # fill in Time, Var, EPUs that are missing
  expanded <- expand.grid(Time = min(mass.survey$Time):end.year,Var = unique(mass.survey$Var))
  
  mass.survey <- expanded |>
    dplyr::left_join(mass.survey, by = c("Time", "Var"))
  
  
  return(mass.survey)
  
}




