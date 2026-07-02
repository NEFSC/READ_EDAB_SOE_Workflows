#' Calculates trans_dates data set for automated workflow
#'
#' This uses a static input file from Kevin Friedland.
#' This is expected to be replaced by a dynamically-produced SST input file that
#'  Kim H is tasked with. Scripts included here will be refactored to accept the
#'  new SST input when available.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_sst Character string. Full path to the SST input file from Kevin Friedland
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::trans_dates indicator
#' workflow_trans_dates(input_path_sst = "path/to/input/data.csv",
#'                      output_path_indicators = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::trans_dates data frame
#'
#' @section Dependencies:
#'
#' This assumes that the input data file from Kevin Friedland has been provided and resides in the path `input_path_sst`
#'
#' @export

workflow_trans_dates <- function(input_path_sst, output_path_indicators = NULL) {
  # Assumes that input data has been provided

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_sst)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator

      indicatorData <- SOEworkflows::create_trans_dates(
        input_path_sst = input_path_sst
      )

      # write data to file
      saveRDS(indicatorData, paste0(output_path_indicators, "/trans_dates.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
