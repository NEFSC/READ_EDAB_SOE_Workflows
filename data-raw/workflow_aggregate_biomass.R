#' Creates aggregate_biomass indicator for SOE
#'
#' @param input_path_survey Character string. Full path to the survdat data pull rds file
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
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
#'   outputPath <- here::here()
#'   input_path_survey <- here::here("surveyNoLengths.rds")
#'   input_path_species <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   workflow_aggregate_biomass(outputPath,input_path_survey,input_path_species)
#' }
#'

workflow_aggregate_biomass <- function(
  outputPath,
  input_path_survey,
  input_path_species
) {
  # Assumes that survey data has been pulled and is located in input_path_survey
  #get_survey_data(channel,outputPath = outputPath)

  # Add check to skip running workflow if data not present or error in creating indicator
  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
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
        paste0(outputPath, "/aggregate_biomass.rds")
      )
      saveRDS(
        indicatorData$aggregate_biomass_species,
        paste0(outputPath, "/aggregate_biomass_species.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
