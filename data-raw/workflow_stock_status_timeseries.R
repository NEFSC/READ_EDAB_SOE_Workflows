#' Calculates stock status timeseries for automated workflow
#'
#' This uses stock assessment data from the `stocksmart` R package.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_present_key Character string. Full path to a key indicating which stocks have matching units for the most recent assessment data
#' @param input_path_historic_key Character string. Full path to a key indicating which assessments to drop from the historic time series due to unit mismatches
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::stock_status_timeseries indicator
#' workflow_stock_status_timeseries(input_path_present_key <- "path/to/input.csv",
#'                          input_path_historic_key <- "path/to/input.csv",
#'                          output_path_indicators <- "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::stock_status_timeseries data frame
#'
#' @section Dependencies:
#'
#' This assumes that the `stocksmart` R package has been updated with recent assessment data
#'
#' @export

workflow_stock_status <- function(
  input_path_present_key,
  input_path_historic_key,
  output_path_indicators = NULL
) {
  # Add check to skip input_path_decoder workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_present_key),
          file.exists(input_path_historic_key)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator

      indicatorData <- SOEworkflows::create_stock_status_timeseries(
        input_path_present_key = input_path_present_key,
        input_path_historic_key = input_path_historic_key
      )
      # write data to file
      saveRDS(
        indicatorData,
        paste0(output_path_indicators, "/stock_status_timeseries.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
