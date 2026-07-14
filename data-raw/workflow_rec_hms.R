#' Create recreational HMS indicator for SOE
#'
#' @description
#' Creates rec_hms data set (recreational Highly Migratory Species) for automated workflow.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_rec Character string. Full path to the data from the 'pull_rec_hms' csv file in EDAB_Dev.
#' @param input_path_rec_key Character string. Full path to the hms_key file in EDAB_Resources ("EDAB_Resources/workflow_resources/soe_workflows/hms_key.csv").
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::rec_hms indicator
#' workflow_rec_hms(input_path_rec = "path/to/hms_mrip_2025-08-26.csv",
#'  input_path_rec_key = "path/to/hms_key.rda",
#'  output_path_indicators = "path/to/output/folder")
#'
#' }
#'
#' @return ecodata::rec_hms data frame
#'
#' @section Dependencies:
#'
#' This assumes that the rec_hms has been pulled and resides in the path 'input_path_rec'
#'
#' @export
#'

workflow_rec_hms <- function(
  input_path_rec,
  input_path_rec_key,
  output_path_indicators
) {
  # Assumes that rec HMS data has been pulled and is located in input_path_rec
  #get_recreational_data(output_path_indicators = output_path_indicators)

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_rec),
          file.exists(input_path_rec_key)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_rec_hms(
        input_path_rec = input_path_rec,
        input_path_rec_key = input_path_rec_key
      )
      # write data to file
      saveRDS(indicatorData, paste0(output_path_indicators, "/rec_hms.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
