#' Calculates fuel prices for automated workflow
#'
#' It is formatted exactly like the ecodata data object
#'
#' @param output_path_indicators Character string. Path to folder where data pull should be saved.
#'
#' @examples
#' \dontrun{
#' # create the ecodata::fuel_prices indicator
#' workflow_fuel_prices(output_path_indicators = "path/to/output/folder")
#' }
#'
#' @return A tibble of the `ecodata::fuel_prices` data frame (returned invisibly).
#'
#' @export
workflow_fuel_prices <- function(output_path_indicators = NULL) {
  tryCatch(
    {
      # 1. Validate output directory
      if (is.null(output_path_indicators) || !dir.exists(output_path_indicators)) {
        stop("The output path is missing or the directory does not exist: ", output_path_indicators)
      }
      
      # 2. Calculate indicator
      message("Starting fuel prices workflow...")
      
      # We call this without arguments to comply with the SOP rule. 
      # Defaults to current year via Sys.Date() and fred_key pulling from .Renviron
      indicatorData <- SOEworkflows::create_fuel_prices()
      
      # 3. Write data to file
      out_file <- file.path(output_path_indicators, "fuel_prices.rds")
      message(sprintf("Saving indicator data to: %s", out_file))
      saveRDS(indicatorData, out_file)
      
      message("Workflow complete.")
      return(invisible(indicatorData))
    },
    error = function(e) {
      message("An error occurred in workflow_fuel_prices: ", conditionMessage(e))
      return(NULL)
    }
  )
}