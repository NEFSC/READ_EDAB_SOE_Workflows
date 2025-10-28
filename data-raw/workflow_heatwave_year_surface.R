#' Calculates surface temperature portion of the heatwave_year data set for automated workflow
#'
#' This uses a static input file from Kim Hyde.
#' This is expected to be replaced by a workflow-generated OISST input file that
#' Kim H is tasked with. Scripts included here will be refactored to accept the 
#' new OISST input when available.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathGB Character string. Full path to the GB OISST input file from Kim Hyde
#' @param inputPathGOM Character string. Full path to the GOM OISST input file from Kim Hyde
#' @param inputPathMAB Character string. Full path to the MAB OISST input file from Kim Hyde
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the surface temperature portion of ecodata::heatwave_year
#' workflow_heatwave_year_surface(inputPathGB = "path/to/input/GBdata.csv",
#'                                inputPathGOM = "path/to/input/GOMdata.csv",
#'                                inputPathMAB = "path/to/input/MABdata.csv",
#'                                outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return surface temperature portion of the ecodata::heatwave_year data frame
#'
#' @section Dependencies:
#' 
#' This assumes that the input data file from Kim Hyde has been provided and resides in the paths `inputPathREGION`
#'
#' @export



workflow_heatwave_year_surface <- function(inputPathGB, inputPathGOM, inputPathMAB, outputPath = NULL) {
  
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
      indicatorData <- SOEworkflows::create_heatwave_year_surface(inputPathGB = inputPathGB,
                                                                 inputPathGOM = inputPathGOM,
                                                                 inputPathMAB = inputPathMAB)
      # write data to file
      saveRDS(indicatorData,paste0(outputPath,"/heatwave_year_surface.rds"))
      return(indicatorData)
      
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
