#' Calculates bennet data set for automated workflow
#'
#' This uses the commercial data pull from the comlandr package.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_bennet Character string. Full path to the commercial data rds file for bennet indicator
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::bennet indicator
#' workflow_bennet(input_path_bennet = "path/to/commerical_bennet.rds",
#'                       input_path_species = "path/to/species/data/.rds",
#'                       outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::bennet data frame
#'
#' @section Dependencies:
#'
#' This assumes that the commercial data has been pulled and resides in the path `input_path_bennet`
#'
#' @export

workflow_bennet <- function(
  input_path_bennet,
  input_path_species,
  outputPath = NULL
) {
  # Assumes that commercial data has been pulled
  #get_commercial_data(channel,outputPathDatasets = outputPath)

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          file.exists(input_path_bennet),
          file.exists(input_path_species),
          (!is.null(outputPath))
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_bennet(
        input_path_bennet = input_path_bennet,
        input_path_species = input_path_species
      )
      # write data to file
      saveRDS(indicatorData, paste0(outputPath, "/bennet.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
