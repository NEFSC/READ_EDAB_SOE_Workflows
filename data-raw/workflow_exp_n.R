#' Calculates exp_n data set (expected number of species) for automated workflow
#'
#' This uses the survdat data pull from the survey package.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathBigelow Character string. Full path to the Bigelow data pull rds file
#' @param inputPathAlbatross Character string. Full path to the Albatross data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::exp_n indicator
#' workflow_exp_n(inputPathBigelow = "path/to/Bigelow/data.rds",
#'                       inputPathAlbatros = "path/to/Albatross/data.rds",
#'                       outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::exp_n data frame
#'
#' @section Dependencies:
#'
#' This assumes that the survey data has been pulled and resides in the path `inputPathBigelow` and `inputPathAlbatross`
#'
#' @export

workflow_exp_n <- function(
  inputPathBigelow,
  inputPathAlbatross,
  outputPath = NULL
) {
  # Assumes that survey data has been pulled
  #get_survey_data(channel,outputPath = outputPath)

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
          file.exists(inputPathBigelow),
          file.exists(inputPathAlbatross)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_exp_n(
        inputPathBigelow = inputPathBigelow,
        inputPathAlbatross = inputPathAlbatross
      )
      # write data to file
      saveRDS(indicatorData, paste0(outputPath, "/exp_n.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
