#' Creates species_dist indicator for SOE
#' 
#' @description
#' Creates a data frame of species distribution on the Northeast Shelf (NES) based on survey data.
#' 
#' 
#' 
#' @param inputPathSurvey Character string. Full path to the survdat data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param staticPath Character string. Path to folder for static files for depth and coast shape are saved
#' @param outputPath Character string. Path to folder where data pull should be saved
#' 
#' @return species_dist data frame used in ecodata 
#' 
#' @section Dependencies:
#' 
#' This assumes that the survey data has been pulled and resides in the path `inputPathSurvey` and that
#' the species data resides in `inputPathSpecies`
#' 
#' @examples
#' \dontrun{
#'   outputPath <- here::here()
#'   inputPathSurvey <- here::here("surveyNoLengths.rds")
#'   inputPathSpecies <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   staticPath <-  "/home/<user>/EDAB_Resources/"
#'   workflow_species_dist(inputPathSurvey, inputPathSpecies, staticPath, outputPath)
#' }
#' 

workflow_species_dist <- function(inputPathSurvey, inputPathSpecies, staticPath, outputPath) {
  
  # Assumes that survey data has been pulled and is located in inputPathSurvey
  #get_survey_data(channel,outputPath = outputPath)
  
  # Check if static files are present
  required_static_files <- c("nes_bath_data.nc", "diag.csv", "nes_coast_2.csv", "stratareas.rdata")
  missing_files <- required_static_files[!file.exists(file.path(staticPath, required_static_files))]
  if (length(missing_files) > 0) {
    message("Missing static files: ", paste(missing_files, collapse = ", "))
    return(NULL)
  }
  
  # Add check to skip running workflow if data not present
  if(file.exists(inputPathSpecies) && file.exists(inputPathSurvey)) {
    
    message("Generating species distribution indicator...")
    indicatorData <- SOEworkflows::create_species_dist(inputPathSurvey = inputPathSurvey,
                                           inputPathSpecies = inputPathSpecies,
                                           staticPath = staticPath,
                                           outputPath = outputPath)
    # write data to file
    saveRDS(indicatorData,paste0(outputPath,"/species_dist.rds"))
    message("species_dist.rds saved to ", file.path(outputPath, "species_dist.rds"))
    
  } else {
    # 
    message("One or more of the input files are not present in the location specified")
  }
}