#' Create species condition indicator for SOE
#'
#' @description
#' Creates condition data set for automated workflow.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to the condition data pull rds file.
#' @param inputPathLW Character string. Full path to the LWparams rda file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param inputPathSpecies Character string. Full path to the species.codes rda file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param outputPath Character string. Path to folder where data pull should be saved.
#'
#' @example
#' \dontrun{
#' # create the ecodata::condition indicator
#' workflow_condition(inputPath = "path/to/conditionData.rds",
#'  inputPathLW = "path/to/hms_key.rda,
#'  inputPathSpecies = "path/to/species.codes.rda",
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
  inputPathLW,
  inputPathSpecies,
  outputPath = NULL
) {
  # Assumes that survey data has been pulled
  #get_survey_data(channel,outputPath = outputPath)

  # Add check to skip running workflow if data not present
  if (
    file.exists(inputPath) &&
      file.exists(inputPathLW) &&
      file.exists(inputPathSpecies) &&
      (!is.null(outputPath))
  ) {
    indicatorData <- SOEworkflows::create_condition(
      inputPath = inputPath,
      inputPathLW = inputPathLW,
      inputPathSpecies = inputPathSpecies
    )
    # write data to file
    saveRDS(indicatorData, paste0(outputPath, "/condition.rds"))
  } else {
    #
    message(
      "One or more of the input files are not present in the location specified"
    )
  }
}
