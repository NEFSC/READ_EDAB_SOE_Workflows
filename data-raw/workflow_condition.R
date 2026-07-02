#' Create species condition indicator for SOE
#'
#' @description
#' Creates condition data set for automated workflow.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to the condition data pull rds file.
#' @param input_path_lw_coeffs Character string. Full path to the LWparams rda file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param input_path_species Character string. Full path to the species.codes rda file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param outputPath Character string. Path to folder where data pull should be saved.
#'
#' @example
#' \dontrun{
#' # create the ecodata::condition indicator
#' workflow_condition(inputPath = "path/to/conditionData.rds",
#'  input_path_lw_coeffs = "path/to/hms_key.rda,
#'  input_path_species = "path/to/species.codes.rda",
#'  outputPath = "path/to/output/folder")
#'
#' }
#'
#' @return ecodata::condition data frame
#'
#' @export
#'

workflow_condition <- function(
  inputPath,
  input_path_lw_coeffs,
  input_path_species,
  outputPath = NULL
) {
  # Assumes that survey data has been pulled
  #get_survey_data(channel,outputPath = outputPath)

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
          file.exists(inputPath),
          file.exists(input_path_lw_coeffs),
          file.exists(input_path_species)
        )
      ) {
        stop("Incorrect file path or file missing")
      }
      # calculate indicator
      indicatorData <- SOEworkflows::create_condition(
        inputPath = inputPath,
        input_path_lw_coeffs = input_path_lw_coeffs,
        input_path_species = input_path_species
      )
      # write data to file
      saveRDS(indicatorData, paste0(outputPath, "/condition.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
