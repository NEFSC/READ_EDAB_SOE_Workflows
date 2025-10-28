#' Calculates bottom temperature portion of the heatwave data set for automated workflow
#'
#' This uses a static input file from Joe Caracappa.
#' This is expected to be replaced by a workflow-generated GLORYS input file that
#' Joe C is tasked with. Scripts included here will be refactored to accept the 
#' new GLORYS input when available.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathGB Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param inputPathGOM Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param inputPathMAB Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the bottom temperature portion of ecodata::heatwave
#' workflow_heatwave_bottom(inputPathGB = "path/to/input/GBdata.csv",
#'                          inputPathGOM = "path/to/input/GOMdata.csv",
#'                          inputPathMAB = "path/to/input/MABdata.csv",
#'                          outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return bottom temperature portion of the ecodata::heatwave data frame
#'
#' @section Dependencies:
#' 
#' This assumes that the input data files from Joe Caracappa has been provided and resides in the paths `inputPathREGION`
#'
#' @export



workflow_heatwave_bottom <- function(inputPathGB, inputPathGOM, inputPathMAB, outputPath = NULL) {
  
  # Assumes that input data has been provided
  
  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
          file.exists(inputPathGB),
          file.exists(inputPathGOM),
          file.exists(inputPathMAB)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_heatwave_bottom(inputPathGB = inputPathGB,
                                                          inputPathGOM = inputPathGOM,
                                                          inputPathMAB = inputPathMAB)
      # write data to file
      saveRDS(indicatorData,paste0(outputPath,"/heatwave_bottom.rds"))
      return(indicatorData)
      
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
