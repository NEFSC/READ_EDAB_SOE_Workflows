#' Pull recreational HMS required for indicator creation
#'
#' @param output_path_indicators Character string. Path to folder where data pull should be saved.
#'
#' @return rec_hms_data, a csv file with landings data from MRIP
#' @return list of data objects. rds files exported
#'
#' @examples
#' \dontrun{
#'   workflow_pull_recreational_data(output_path_indicators)
#' }
#'
workflow_pull_recreational_data <- function(output_path_indicators = NULL) {
  # check to skip running workflow
  tryCatch(
    {
      if (is.null(output_path_indicators)) {
        stop("output file path file missing")
      }
      # pull MRIP data
      rec_hms_data <- SOEworkflows::get_recreational_data(
        output_path_indicators
      )

      # Save these to a specific location
      #write.csv(rec_hms_data, paste0(output_path_indicators, "/hms_mrip_", Sys.Date(), ".csv"))
      saveRDS(
        rec_hms_data,
        paste0(output_path_indicators, "/hms_mrip_", Sys.Date(), ".rds")
      )
      return(rc_hms_data)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
