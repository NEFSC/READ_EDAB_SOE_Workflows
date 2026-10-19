#' Calculates the heatwave data set for automated workflow
#'
#' Bottom temperature - This uses a static input file from Joe Caracappa.
#' This is expected to be replaced by a workflow-generated GLORYS input file that
#' Joe C is tasked with. Scripts included here will be refactored to accept the
#' new GLORYS input when available.
#' Surface temperature - This uses a static input file from Kim Hyde.
#' This is expected to be replaced by a workflow-generated OISST input file that
#' Kim H is tasked with. Scripts included here will be refactored to accept the
#' new OISST input when available.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_bot Character string. Full path to the GB GLORYS input file from EDABGLORYS
#' @param input_path_surf Character string. Full path to the GB OISST input file from ecopull
#' @param detrend Logical. If TRUE, performs linear detrending
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::heatwave dataset
#' workflow_heatwave(input_path_bot = "path/to/input/bottom_data.csv",
#'                          input_path_surf = "path/to/input/surface_data.csv")
#'
#' }
#'
#'
#' @return ecodata::heatwave data frame
#'
#' @section Dependencies:
#'
#' This assumes that the input data files from Joe Caracappa and Kim Hyde has been provided and resides in the appropriate directory
#'
#' @export

workflow_heatwave <- function(
  input_path_bot,
  input_path_surf,
  detrend = T,
  output_path_indicators = NULL
) {
  # Assumes that input data has been provided

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_surf)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_heatwave(
        input_path_bot = input_path_bot,
        input_path_surf = input_path_surf,
        detrend = detrend
      )
      # write data to file
      saveRDS(indicatorData, paste0(output_path_indicators, "/heatwave.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
