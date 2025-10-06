#' Pull recreational HMS required for indicator creation
#'
#' @param outputPath Character string. Path to folder where data pull should be saved.
#'
#' @return rec_hms_data, a csv file with landings data from MRIP
#' @return list of data objects. rds files exported
#'
#' @examples
#' \dontrun{
#'   workflow_pull_recreational_data(outputPath)
#' }
#'
workflow_pull_recreational_data <- function(outputPath = NULL) {
  # pull MRIP data
  rec_hms_data <- SOEworkflows::get_recreational_data(outputPath)

  # Save these to a specific location
  if (!is.null(outputPath)) {
    #write.csv(rec_hms_data, paste0(outputPath, "/hms_mrip_", Sys.Date(), ".csv"))
    saveRDS(rec_hms_data, paste0(outputPath, "/hms_mrip_", Sys.Date(), ".rds"))
  }
}
