#' Calculates stock status for automated workflow
#'
#' This uses stock assessment data from the `stocksmart` R package.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_decoder Character string. Full path to a csv lookup table that joins species abbreviations with their formal stock names
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

workflow_stock_status <- function(
  input_path_decoder,
  output_path_indicators = NULL
) {
  # Add check to skip inputPath workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_decoder)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator

      indicatorData <- SOEworkflows::create_stock_status(
        input_path_decoder = input_path_decoder
      )
      # write data to file
      saveRDS(
        indicatorData,
        paste0(output_path_indicators, "/stock_status.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
