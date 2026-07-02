#' Creates mass_inshore_survey indicator for SOE
#'
#' @param input_path_mass_survey Character string. Full path to the mass inshore data pull rds file created by workflow_pull_survey_data
#' @param input_path_species Character string. Full path to the species list rds file
#' @param output_path_indicators Character string. Path to folder where processed data should be saved
#'
#' @return Nothing. rds file exported

#' @examples
#' \dontrun{
#'   input_path_species <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   input_path_mass_survey <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   output_path_indicators = "path/to/output/folder"
#'   workflow_ma_inshore_survey(input_path_mass_survey, input_path_species, output_path_indicators)
#' }
#'

workflow_mass_inshore_survey <- function(
  input_path_mass_survey,
  input_path_species,
  output_path_indicators = NULL
) {
  # Assumes that survey data has been pulled and is located in input_path_mass_survey
  # workflow_pull_survey_data(channel,output_path_indicators = output_path_indicators)

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_mass_survey),
          file.exists(input_path_species)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_mass_inshore_survey(
        input_path_mass_survey = input_path_mass_survey,
        input_path_species = input_path_species
      )

      # Write data to file
      saveRDS(
        indicatorData,
        paste0(output_path_indicators, "/mass_inshore_survey.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
