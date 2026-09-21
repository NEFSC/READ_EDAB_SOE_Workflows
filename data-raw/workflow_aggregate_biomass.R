#' Creates aggregate_biomass indicator for SOE
#'
#' @param input_path_survey Character string. Full path to the survdat data pull rds file
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @return List
#'
#' \item{aggregate_biomass}{The `ecodata::aggregate_biomass` data frame}
#' \item{aggregate_biomass_species}{Stratified mean for each species/Season at EPU level that make up the aggregate}
#'
#'
#' @section Dependencies:
#'
#' This assumes that the survey data has been pulled and resides in the path `input_path_survey` and that
#' the species data resides in `input_path_species`
#'
#' @examples
#' \dontrun{
#'   output_path_indicators <- here::here()
#'   input_path_survey <- here::here("surveyNoLengths.rds")
#'   input_path_species <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   workflow_aggregate_biomass(output_path_indicators,input_path_survey,input_path_species)
#' }
#'

workflow_aggregate_biomass <- function(
  input_path_survey,
  input_path_species,
  output_path_indicators
) {
  # Assumes that survey data has been pulled and is located in input_path_survey
  #get_survey_data(channel,output_path_indicators = output_path_indicators)

  # Add check to skip running workflow if data not present or error in creating indicator
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_survey),
          file.exists(input_path_species)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_aggregate_biomass(
        input_path_survey = input_path_survey,
        input_path_species = input_path_species
      )

      # Write data to file
      saveRDS(
        indicatorData$aggregate_biomass,
        paste0(output_path_indicators, "/aggregate_biomass.rds")
      )
      saveRDS(
        indicatorData$aggregate_biomass_species,
        paste0(output_path_indicators, "/aggregate_biomass_species.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
