#' Calculates fuel prices for automated workflow
#'
#' It is formatted exactly like the ecodata data object
#'
#' @param end_year Integer. The final year of data to include. Defaults to 2026.
#' @param fred_key Character. A valid FRED API key. Defaults to `NULL`.
#' @param output_path_indicators Character string. Path to folder where data pull should be saved
#'
#' @examples
#' \dontrun{
#' # create the ecodata::fuel_prices indicator
#' workflow_fuel_prices(output_path_indicators = "path/to/output/folder")
#'
#' }
#'
#'
#' @return A tibble of the `ecodata::fuel_prices` data frame (returned invisibly).
#'
#'
#' @export

workflow_fuel_prices <- function(end_year = 2026,
                                 fred_key = NULL,
                                 output_path_indicators = NULL) {
  tryCatch(
    {
      # 1. Validate output directory
      if (is.null(output_path_indicators) || !dir.exists(output_path_indicators)) {
        stop("The output path is missing or the directory does not exist: ", output_path_indicators)
      }
      
      # 2. Calculate indicator
      message("Starting fuel prices workflow...")
      indicatorData <- SOEworkflows::create_fuel_prices(
        end_year = end_year,
        fred_key = fred_key
      )
      
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