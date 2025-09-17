#' Create recreational HMS indicator for SOE
#'
#' @description
#' Creates rec_hms data set (recreational Highly Migratory Species) for automated workflow.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to the data from the 'pull_rec_hms' csv file in EDAB_Dev.
#' @param inputKey Character string. Full path to the hms_key file in EDAB_Resources ("EDAB_Resources/workflow_resources/soe_workflows/hms_key.csv").
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::rec_hms indicator
#' workflow_rec_hms(inputPath = "path/to/hms_mrip_2025-08-26.csv",
#'  inputKey = "path/to/hms_key.rda",
#'  outputPath = "path/to/output/folder")
#'
#' }
#'
#' @return ecodata::rec_hms data frame
#'
#' @section Dependencies:
#'
#' This assumes that the rec_hms has been pulled and resides in the path 'inputPath'
#'
#' @export
#'

workflow_rec_hms <- function(inputPath, inputKey, outputPath) {
  # Assumes that rec HMS data has been pulled and is located in inputPath
  #pull_rec_hms(channel,outputDir = outPutDir)

  # Add check to skip running workflow if data not present
  if (file.exists(inputPath) && file.exists(inputKey)) {
    indicatorData <- SOEworkflows::create_rec_hms(
      inputPath = inputPath,
      inputKey = inputKey
    )
    # write data to file
    saveRDS(indicatorData, paste0(outputPath, "/rec_hms.rds"))
  } else {
    #
    message(
      "One or more of the input files are not present in the location specified"
    )
  }
}
