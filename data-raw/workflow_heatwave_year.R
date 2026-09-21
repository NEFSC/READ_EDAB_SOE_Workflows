#' Calculates the heatwave_year data set for automated workflow
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
#' @param input_path_gb_bot Character string. Full path to the GB GLORYS input file from Joe Caracappa
#' @param input_path_gom_bot Character string. Full path to the GOM GLORYS input file from Joe Caracappa
#' @param input_path_mab_bot Character string. Full path to the MAB GLORYS input file from Joe Caracappa
#' @param input_path_gb_surf Character string. Full path to the GB OISST input file from Kim Hyde
#' @param input_path_gom_surf Character string. Full path to the GOM OISST input file from Kim Hyde
#' @param input_path_mab_surf Character string. Full path to the MAB OISST input file from Kim Hyde
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::heatwave dataset
#' workflow_heatwave_year(input_path_gb_bot = "path/to/input/GBdata.csv",
#'                          input_path_gom_bot = "path/to/input/GOMdata.csv",
#'                          input_path_mab_bot = "path/to/input/MABdata.csv",
#'                          input_path_gb_surf = "path/to/input/GBdata.csv",
#'                          input_path_gom_surf = "path/to/input/GOMdata.csv",
#'                          input_path_mab_surf = "path/to/input/MABdata.csv",
#'                          output_path_indicators = "path/to/output/folder")
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

workflow_heatwave_year <- function(
  input_path_gb_bot,
  input_path_gom_bot,
  input_path_mab_bot,
  input_path_gb_surf,
  input_path_gom_surf,
  input_path_mab_surf,
  output_path_indicators = NULL
) {
  # Assumes that input data has been provided

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_gb_surf),
          file.exists(input_path_gom_surf),
          file.exists(input_path_mab_surf),
          file.exists(input_path_gb_bot),
          file.exists(input_path_gom_bot),
          file.exists(input_path_mab_bot)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_heatwave_year(
        input_path_gb_bot = input_path_gb_bot,
        input_path_gom_bot = input_path_gom_bot,
        input_path_mab_bot = input_path_mab_bot,
        input_path_gb_surf = input_path_gb_surf,
        input_path_gom_surf = input_path_gom_surf,
        input_path_mab_surf = input_path_mab_surf
      )
      # write data to file
      saveRDS(
        indicatorData,
        paste0(output_path_indicators, "/heatwave_year.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
