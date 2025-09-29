#' Creates mass inshore survey indicator for SOE
#' 
#' This uses both functions from ma_inshore_survey
#' @param channel Character string. 
#' @param inputPathSpecies Character string. Full path to the species list  rds file
#' @param outputPath Character string. Path to folder where processed data should be saved
#' 
#' @return Nothing. rds file exported

#' @examples
#' \dontrun{
#'   channel <- dbutils::connect_to_database("server",user)
#'   inputPathSpecies <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   outputPath = "path/to/output/folder"
#'   workflow_ma_inshore_survey(channel, inputPathSpecies, outputPath)
#' }
#' 

# To do: sort out syntax, where does first functions sve out....and how does second function find that
workflow_ma_inshore_survey <- function(channel,  inputPathSpecies, outputPath = NULL) {
  
  # pull survey data
  survdat.mass <- SOEworkflows::get_mass_survey(channel)
  
  # Save out --> where?
  saveRDS(survdat.mass, paste0(outputPath, "/raw.inshorsurveyData.rds"))
  
  # Run second function
  indicatorData <- SOEworkflows::create_mass_inshore_survey(inputPathSurvey = paste0(outputPath, "/raw.inshorsurveyData.rds"),
                                                            inputPathSpecies = inputPathSpecies)
    
  # Write data to file
  saveRDS(indicatorData,paste0(outputPath,"/ma_inshore_survey.rds"))

}



