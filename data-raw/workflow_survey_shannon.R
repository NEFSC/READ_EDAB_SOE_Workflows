#' Calculates survey_shannon data set for automated workflow
#'
#' This uses the survdat data pull from the survey package.
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_bigelow Character string. Full path to the Bigelow data pull rds file
#' @param input_path_albatross Character string. Full path to the Albatross data pull rds file
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::survey_shannon indicator
#' workflow_survey_shannon(input_path_bigelow = "path/to/Bigelow/data.rds",
#'                       inputPathAlbatros = "path/to/Albatross/data.rds",
#'                       output_path_indicators = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::survey_shannon data frame
#'
#' @section Dependencies:
#'
#' This assumes that the survey data has been pulled and resides in the path `input_path_bigelow` and `input_path_albatross`
#'
#' @export

workflow_survey_shannon <- function(
  input_path_bigelow,
  input_path_albatross,
  output_path_indicators = NULL
) {
  # Assumes that survey data has been pulled
  #get_survey_data(channel,output_path_indicators = output_path_indicators)

  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_bigelow),
          file.exists(input_path_albatross)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_survey_shannon(
        input_path_bigelow = input_path_bigelow,
        input_path_albatross = input_path_albatross
      )
      # write data to file
      saveRDS(indicatorData, paste0(output_path_indicators, "/survey_shannon.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
