#' Calculates productivity anomaly for automated workflow
#'
#' This uses the survey data pull from pull_survey_data.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_survey_bio_epu Character string. Full path to the survey data with bio and epu rds file
#' @param input_survey_bio Character string. Full path to the survey data with bio rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::productivity_anomaly indicator
#' workflow_productivity_anomaly(
#'   input_survey_bio_epu <- "path/to/survey/bio/epu/.rds"
#'   input_survey_bio <- "path/to/survey/bio/.rds"
#'   inputPathSpecies = "path/to/species/data/.rds",
#'   outputPath = "path/to/output/folder"
#'   )
#' }
#'
#'
#' @return productivity_anomaly
#' @export

workflow_productivity_anomaly <- function(input_survey_bio_epu,
                                          input_survey_bio,
                                          inputPathSpecies,
                                          outputPath) {
  
  prod_anom <- create_productivity_anomaly(input_survey_bio_epu, input_survey_bio, inputPathSpecies)
  
  saveRDS(prod_anom,
          file = file.path(outputPath, "productivity_anomaly.rds"))
}