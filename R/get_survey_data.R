#' Get survey data using survdat
#'
#' @description
#' Pulls survey data in format required for indicator generation
#'
#' @param channel an Object inherited from DBIConnection-class. .
#' This object is used to connect to communicate with the database engine.
#'
#'@return A list of survey data pulls
#'
#'@examples
#'\dontrun{
#'channel <- dbutils::connect_to_database("server",user)
#'rawData <- get_survey_data(channel)
#'}
#'
#'@export

get_survey_data <- function(channel) {
  end.year <- format(Sys.Date(), "%Y")
  # Get the survey data for aggregate biomass
  message("##############################################################")
  message("Getting base survey data without Lengths ")
  survey1 <- survdat::get_survdat_data(channel, getLengths = F)

  # Get the survey data for exp_n, survey_shannon
  # Grab Albatross time series (< 2009)
  message("##############################################################")
  message("Getting albatross survey data (<= 2008) without Lengths ")
  al.data <- survdat::get_survdat_data(
    channel,
    filterByYear = 1963:2008,
    getLengths = FALSE
  )

  #Grab data without Bigelow conversions (>= 2009)
  message("##############################################################")
  message("Getting bigelow survey data (> 2008) without Lengths ")
  big.data <- survdat::get_survdat_data(
    channel,
    filterByYear = 2009:end.year,
    conversion.factor = FALSE,
    getLengths = FALSE
  )

  # Get the survey data for condition indicator.
  # Individual lengths and weights are required
  message("##############################################################")
  message("Getting condition survey data with biological data ")
  condition <- survdat::get_survdat_data(
    channel,
    all.season = TRUE,
    getBio = TRUE
  )

  # Get Biological data
  message("##############################################################")
  message("Getting bottom trawl survey with biological info")
  bio <- survdat::get_survdat_data(channel,
                            filterByYear = NA, 
                            all.season = F,
                            shg.check = T,
                            conversion.factor = T, 
                            use.SAD = F, 
                            getBio = T,
                            getLengths = T,
                            getWeightLength = F)
  # Assign EPUs to biological data pull.
  # Note that records in Strata not defined by an EPU will be omitted
  bio_epu <- dplyr::left_join(bio$survdat,SOEworkflows::epu_strata , by = c("STRATUM")) |> 
    dplyr::filter(!is.na(EPU))
  
  
  # Get mass inshore survey data
  message("##############################################################")
  message("Getting the Massachusetts Inshore Survey Data")
  mass_inshore <- survdat::get_mass_inshore_survey(channel,
                                                   filterByYear = 1963:end.year)
  
  # create a list of three datasets
  survey_data <- list(
    survey1 = survey1,
    al.data = al.data,
    big.data = big.data,
    condition = condition,
    bio = bio,
    bio_epu = bio_epu,
    mass_inshore = mass_inshore
  )
  message("##############################################################")
  message("Done pulling Survey data")

  return(survey_data)
}
