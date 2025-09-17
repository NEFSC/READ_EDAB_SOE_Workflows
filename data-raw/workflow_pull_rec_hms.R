#' Pull recreational HMS required for indicator creation
#'
#' @param outputDir Character string. Path to folder where data pull should be saved.
#'
#' @return rec_hms_data, a csv file with landings data from MRIP
#' @return list of data objects. rds files exported
#'
#' @examples
#' \dontrun{
#'   workflow_pull_rec_hms(outputDir)
#' }
#'
workflow_pull_rec_hms <- function(outputDir = NULL) {
  # pull MRIP data
  rec_hms_data <- SOEworkflows::pull_rec_hms(
    outputDir = paste0(outputDir)
  )

  # Save these to a specific location
  if (!is.null(outputDir)) {
    write.csv(rec_hms_data, paste0(outputDir, "/hms_mrip_", Sys.Date(), ".csv"))
  }
}
