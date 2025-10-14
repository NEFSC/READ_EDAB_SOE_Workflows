#' Get commercial data set to store somewhere
#'
#' @description
#' Pulls commercial data in multiple formats required for indicator generation
#' Menhaden data are removed from the commercial data pull since they are incomplete
#'
#' @param channel an Object inherited from DBIConnection-class.
#'
#'@return A list of commercial data pulls
#'
#'@examples
#'\dontrun{
#'channel <- dbutils::connect_to_database("server","user")
#'rawData <- get_commercial_data(channel)
#'}
#'
#'@export

get_commercial_data <- function(channel) {
  # commercial data lags by an additional year.
  # SOE 2025 uses data through 2023
  # Bennet then uses a reference year of January 2023
  # Since cycle should be over by May any time the workflow is run prior to May we assume it is for existing cycle
  end.year <- as.numeric(format(Sys.Date(), "%Y"))
  currentMonth <- lubridate::month(Sys.Date())
  if (currentMonth > 4) {
    end.year <- end.year
  } else {
    end.year <- end.year - 1
  }

  ## These EPU definitions should/could be in an external file or a data package
  #Set up EPU definitions
  gom <- data.table::data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
  gb <- data.table::data.table(
    AREA = c(521:526, 551, 552, 561, 562),
    EPU = 'GB'
  )
  mab <- data.table::data.table(
    AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
    EPU = 'MAB'
  )
  ss <- data.table::data.table(AREA = c(463:467, 511), EPU = 'SS')

  epuAreas <- data.table::rbindlist(list(gom, gb, mab, ss))
  epuAreas[, NESPP3 := 1]
  epuAreas[, MeanProp := 1]

  message("Pulling Commercial data by EPU ...")
  # Get the commercial data for comdat and bennet
  comdat <- comlandr::get_comland_data(
    channel,
    filterByYear = 1964:(end.year-1),
    refYear = end.year - 1,
    refMonth = 1,
    aggArea = T,
    userAreas = epuAreas,
    applyProp = F,
    aggGear = F,
    unkVar = c('MONTH', 'NEGEAR', 'AREA'),
    knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2', 'AREA')
  )
  # Remove menhaden
  comdat$comland <- comdat$comland |>
    dplyr::filter(!(NESPP3 == 221))
  
  # Bennet indicator
  bennet <- comdat

  # create a list of data sets
  commercial_data <- list(
    comdat = comdat,
    bennet = bennet
  )

  return(commercial_data)
}
