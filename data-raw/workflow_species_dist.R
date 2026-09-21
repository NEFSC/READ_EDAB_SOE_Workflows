#' Creates species_dist indicator for SOE
#'
#' @description
#' Creates a data frame of species distribution on the Northeast Shelf (NES) based on survey data.
#'
#'
#'
#' @param input_path_survey Character string. Full path to the survdat data pull rds file
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param input_path_static_depth Character string. Path to file with depth data for NE shelf
#' @param input_path_static_diagonal Character string. Path to file with along shelf diagonal data
#' @param input_path_static_coast_coord Character string. Path to file with lat lon coordinates defining the coastline
#' @param input_path_static_strat_areas Character string. Path to file defining NEFSC trawl survey strata
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @return species_dist data frame used in ecodata
#'
#' @section Dependencies:
#'
#' This assumes that the survey data has been pulled and resides in the path `input_path_survey` and that
#' the species data resides in `input_path_species`
#'
#' @examples
#' \dontrun{
#'   output_path_indicators <- here::here()
#'   input_path_survey <- here::here("surveyNoLengths.rds")
#'   input_path_species <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   input_path_static_depth <-  "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_bath_data.nc"
#'   input_path_static_diagonal <- "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/diag.csv"
#'   input_path_static_coast_coord <- "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_coast_2.csv"
#'   input_path_static_strat_areas <- "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/stratareas.rdata"
#'   workflow_species_dist(input_path_survey, input_path_species, input_path_static_depth, input_path_static_diagonal, input_path_static_coast_coord, input_path_static_strat_areas, output_path_indicators)
#' }
#'

workflow_species_dist <- function(
  input_path_survey,
  input_path_species,
  input_path_static_depth,
  input_path_static_diagonal,
  input_path_static_coast_coord,
  input_path_static_strat_areas,
  output_path_indicators = NULL
) {
  # Assumes that survey data has been pulled and is located in input_path_survey
  #get_survey_data(channel,output_path_indicators = output_path_indicators)

  # Check if static files are present
  required_files <- list(
    input_path_static_depth = input_path_static_depth,
    input_path_static_diagonal = input_path_static_diagonal,
    static_coast = input_path_static_coast_coord,
    static_strat = input_path_static_strat_areas
  )

  missing_files <- names(required_files)[!file.exists(unlist(required_files))]

  if (length(missing_files) > 0) {
    message("Missing static files: ", paste(missing_files, collapse = ", "))
    return(NULL)
  }

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_survey),
          file.exists(input_path_species)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      message(
        "Generating species distribution indicator... this could take up to 15 minutes"
      )
      indicatorData <- SOEworkflows::create_species_dist(
        input_path_survey = input_path_survey,
        input_path_species = input_path_species,
        input_path_static_depth = input_path_static_depth,
        input_path_static_diagonal = input_path_static_diagonal,
        input_path_static_coast_coord = input_path_static_coast_coord,
        input_path_static_strat_areas = input_path_static_strat_areas
      )

      # write data to file
      saveRDS(
        indicatorData,
        file.path(output_path_indicators, "species_dist.rds")
      )
      message(
        "species_dist.rds saved to ",
        file.path(output_path_indicators, "species_dist.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
