#' Create species condition indicator for SOE
#'
#' @description
#' Creates condition data set for automated workflow.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to the condition data pull rds file.
#' @param inputpathLW Character string. Full path to the LWparams rda file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param inputpathSpecies Character string. Full path to the species.codes rda file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param outputPath Character string. Path to folder where data pull should be saved.
#'
#' @example
#' \dontrun{
#' # create the ecodata::condition indicator
#' workflow_condition(inputPath = "path/to/condition.rds",
#'  inputpathLW = "path/to/hms_key.rda,
#'  inputpathSpecies = "path/to/species.codes.rda",
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
  inputpathLW,
  inputpathSpecies,
  outputPath = NULL
) {
  # Assumes that survey data has been pulled
  #get_survey_data(channel,outputPath = outputPath)

  # Add check to skip running workflow if data not present
  if (
    file.exists(inputPath) &&
      file.exists(inputpathLW) &&
      file.exists(inputpathSpecies) &&
      (!is.null(outputPath))
  ) {
    indicatorData <- SOEworkflows::create_species_condition(
      inputPath = inputPath,
      inputpathLW = inputpathLW,
      inputpathSpecies = inputpathSpecies,
      by_EPU = TRUE,
      by_sex = FALSE,
      length_break = NULL,
      more_than_20_years = TRUE,
      record_outliers = FALSE,
      output = "soe"
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
