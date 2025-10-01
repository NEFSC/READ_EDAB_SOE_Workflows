#' Creates mass_inshore_survey indicator for SOE
#' 
#' @param inputPathMassSurvey Character string. Full path to the mass inshore data pull rds file created by workflow_pull_survey_data
#' @param inputPathSpecies Character string. Full path to the species list rds file
#' @param outputPath Character string. Path to folder where processed data should be saved
#' 
#' @return Nothing. rds file exported

#' @examples
#' \dontrun{
#'   inputPathSpecies <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   inputPathMassSurvey <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   outputPath = "path/to/output/folder"
#'   workflow_ma_inshore_survey(inputPathMassSurvey, inputPathSpecies, outputPath)
#' }
#' 

workflow_mass_inshore_survey <- function(inputPathMassSurvey,  
                                         inputPathSpecies, 
                                         outputPath = NULL) {
  
  # Assumes that survey data has been pulled and is located in inputPathMassSurvey
  # workflow_pull_survey_data(channel,outputPath = outputPath)
  
  # Add check to skip running workflow if data not present
  if(file.exists(inputPathSpecies) && file.exists(inputPathMassSurvey) && (!is.null(outputPath))) {
    
    indicatorData <- create_mass_inshore_survey(inputPathMassSurvey = inputPathMassSurvey,
                                                inputPathSpecies = inputPathSpecies)
    
    # Write data to file
    saveRDS(indicatorData,paste0(outputPath,"/mass_inshore_survey.rds"))
    
  } else {
    # 
    message("One or more of the input files are not present in the location specified")
  }

}



