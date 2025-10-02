#' Creates species_dist indicator for SOE
#' 
#' @description
#' Creates a data frame of species distribution on the Northeast Shelf (NES) based on survey data.
#' 
#' 
#' 
#' @param inputPathSurvey Character string. Full path to the survdat data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param static_depth Character string. Path to file with depth data for NE shelf
#' @param static_diagonal Character string. Path to file with along shelf diagonal data
#' @param static_coast_coord Character string. Path to file with lat lon coordinates defining the coastline
#' @param static_strat_areas Character string. Path to file defining NEFSC trawl survey strata
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
#'   static_depth <-  "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_bath_data.nc"
#'   static_diagonal <- "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/diag.csv"
#'   static_coast_coord <- "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_coast_2.csv"
#'   static_strat_areas <- "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/stratareas.rdata"
#'   workflow_species_dist(inputPathSurvey, inputPathSpecies, static_depth, static_diagonal, static_coast_coord, static_strat_areas, outputPath)
#' }
#' 

workflow_species_dist <- function(inputPathSurvey, 
                                  inputPathSpecies, 
                                  static_depth, 
                                  static_diagonal, 
                                  static_coast_coord, 
                                  static_strat_areas, 
                                  outputPath = NULL) { 
  # Assumes that survey data has been pulled and is located in inputPathSurvey
  #get_survey_data(channel,outputPath = outputPath)

  # Check if static files are present
  required_files <- list(
    static_depth     = static_depth,
    static_diagonal  = static_diagonal,
    static_coast     = static_coast_coord,
    static_strat     = static_strat_areas
  )
  
  missing_files <- names(required_files)[!file.exists(unlist(required_files))]
  
  if (length(missing_files) > 0) {
    message("Missing static files: ", paste(missing_files, collapse = ", "))
    return(NULL)
  }
  
  # Add check to skip running workflow if data not present
  if(file.exists(inputPathSpecies) && file.exists(inputPathSurvey)) {
    
    message("Generating species distribution indicator... this could take up to 15 minutes")
    indicatorData <- SOEworkflows::create_species_dist(
      inputPathSurvey   = inputPathSurvey,
      inputPathSpecies  = inputPathSpecies,
      static_depth      = static_depth,
      static_diagonal   = static_diagonal,
      static_coast_coord= static_coast_coord,
      static_strat_areas= static_strat_areas
      )
    
    # write data to file
    saveRDS(indicatorData, file.path(outputPath, "species_dist.rds"))
    message("species_dist.rds saved to ", file.path(outputPath, "species_dist.rds"))
    
  } else {
    # 
    message("One or more of the input files are not present in the location specified")
  }
  
}