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
#' @param inputPathGBBot Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param inputPathGOMBot Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param inputPathMABBot Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#' @param inputPathGBSurf Character string. Full path to the GB OISST input file from Kim Hyde
#' @param inputPathGOMSurf Character string. Full path to the GOM OISST input file from Kim Hyde
#' @param inputPathMABSurf Character string. Full path to the MAB OISST input file from Kim Hyde
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::heatwave dataset
#' workflow_heatwave(inputPathGBBot = "path/to/input/GBdata.csv",
#'                          inputPathGOMBot = "path/to/input/GOMdata.csv",
#'                          inputPathMABBot = "path/to/input/MABdata.csv",
#'                          inputPathGBSurf = "path/to/input/GBdata.csv",
#'                          inputPathGOMSurf = "path/to/input/GOMdata.csv",
#'                          inputPathMABSurf = "path/to/input/MABdata.csv",
#'                          outputPath = "path/to/output/folder")
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


workflow_heatwave <- function(inputPathGBBot,
                              inputPathGOMBot, 
                              inputPathMABBot,
                              inputPathGBSurf,
                              inputPathGOMSurf, 
                              inputPathMABSurf, 
                              outputPath = NULL) {
  
  # Assumes that input data has been provided
  
  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
          file.exists(inputPathGBSurf),
          file.exists(inputPathGOMSurf),
          file.exists(inputPathMABSurf),
          file.exists(inputPathGBBot),
          file.exists(inputPathGOMBot),
          file.exists(inputPathMABBot)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_heatwave(inputPathGBBot = inputPathGBBot,
                                                     inputPathGOMBot = inputPathGOMBot,
                                                     inputPathMABBot = inputPathMABBot,
                                                     inputPathGBSurf = inputPathGBSurf,
                                                     inputPathGOMSurf = inputPathGOMSurf,
                                                     inputPathMABSurf = inputPathMABSurf)
      # write data to file
      saveRDS(indicatorData,paste0(outputPath,"/heatwave.rds"))
      return(indicatorData)
      
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
