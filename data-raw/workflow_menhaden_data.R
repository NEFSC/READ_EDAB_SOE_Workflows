#' Create Atlantic menhaden commercial landings indicator for SOE
#'
#' @description
#' Creates Atlantic menhaden commercial landings data set for automated workflow.
#' Loads, cleans, and aggregates publicly-available commercial landings from NOAA FOSS and ACCSP.
#'
#' @param input_path_foss Character string. Full path to the FOSS regional landings CSV file.
#' @param input_path_foss_nc Character string. Full path to the FOSS NC landings CSV file.
#' @param input_path_accsp Character string. Full path to the ACCSP landings CSV file.
#' @param output_path_indicators Character string. Path to folder where data pull should be saved.
#' @param api Logical. If `TRUE`, attempts to query online APIs (defaults to `FALSE`).
#'
#' @example
#' \dontrun{
#' # create the menhaden indicator
#' workflow_menhaden(
#'   input_path_foss = "path/to/FOSS_landings.csv",
#'   input_path_foss_nc = "path/to/FOSS_landings_NC.csv",
#'   input_path_accsp = "path/to/accsp_menhaden_SOE2027.csv",
#'   output_path_indicators = "path/to/output/folder"
#' )
#' }
#'
#' @return Menhaden commercial landings data frame
#'
#' @export
#'

workflow_menhaden <- function(
  input_path_foss,
  input_path_foss_nc,
  input_path_accsp,
  output_path_indicators = NULL,
  api = FALSE
) {
  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_foss),
          file.exists(input_path_foss_nc),
          file.exists(input_path_accsp)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator / process data
      indicatorData <- SOEworkflows::get_menhaden_data(
        api = api,
        input_path_foss = input_path_foss,
        input_path_foss_nc = input_path_foss_nc,
        input_path_accsp = input_path_accsp
      )

      # write data to file
      saveRDS(indicatorData, paste0(output_path_indicators, "/menhaden.rds"))
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
