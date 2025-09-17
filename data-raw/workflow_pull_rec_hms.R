#' Pull recreational HMS required for indicator creation
#'
#' @param outputDir Character string. Path to folder where data pull should be saved.
#' If not NULL the pull will be saved to the
#' folder `//nefscdata/EDAB_Dev/atyrell/hms_mrip_data/`
#' @param channel Oracle connection channel created with `dbutils`
#'
#' @return rec_hms_data, a csv file with landings data from MRIP
#' @return list of data objects. rds files exported
#'
#' @examples
#' \dontrun{
#'   workflow_pull_rec_hms(outputDir)
#' }
#'
workflow_pull_rec_hms <- function(outputDir = NULL, channel) {
  # pull MRIP data
  rec_hms_data <- SOEworkflows::pull_rec_hms(channel)

  # Save these to a specific location
  if (!is.null(outputDir)) {
    saveRDS(rec_hms_data, paste0(outputDir, "hms_mrip_", Sys.Date(), ".csv"))
  }

  return(rec_hms_data)
}
