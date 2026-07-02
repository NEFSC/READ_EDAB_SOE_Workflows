#' Calculates stock status for automated workflow
#'
#' This uses stock assessment data from the `stocksmart` R package.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to a csv lookup table that joins species abbreviations with their formal stock names
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::stock_status indicator
#' workflow_stock_status(inputPath = "path/to/decoder.csv",
#'                       output_path_indicators = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::stock_status data frame
#'
#' @section Dependencies:
#'
#' This assumes that the `stocksmart` R package has been updated with recent assessment data
#'
#' @export

workflow_stock_status <- function(inputPath, output_path_indicators = NULL) {
  # Add check to skip inputPath workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(inputPath)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator

      indicatorData <- SOEworkflows::create_stock_status(
        data = stocksmart::stockAssessmentSummary,
        decode = utils::read.csv(inputPath)
      )
      # write data to file
      saveRDS(indicatorData, paste0(output_path_indicators, "/stock_status.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
