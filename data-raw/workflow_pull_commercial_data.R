#' Pull commercial data required for indicator creation
#'
#' @param channel an Object inherited from DBIConnection-class. .
#' This object is used to connect to communicate with the database engine.
#' @param output_path_indicators Character string. Path to folder where data pull should be saved.
#' If not NULL the pull will be saved to the
#' folder `output_path_indicators` with names (`comdat.rds`,`bennet.rds`)
#'
#' @return list of data objects. rds files exported
#'
#'
#' @examples
#' \dontrun{
#'   channel <- dbutils::connect_to_database("server","user")
#'   output_path_indicators <- here::here()
#'   workflow_data_pull(channel,output_path_indicators)
#' }
#'

workflow_pull_commercial_data <- function(
  channel,
  output_path_indicators = NULL
) {
  # check to skip running workflow
  tryCatch(
    {
      if (is.null(output_path_indicators)) {
        stop("output file path file missing")
      }
      # pull commercial data
      commercial_data <- SOEworkflows::get_commercial_data(channel)
      # Save these to a specific location
      saveRDS(
        commercial_data$comdat,
        paste0(output_path_indicators, "/commercial_comdat_data.rds")
      )
      saveRDS(
        commercial_data$bennet,
        paste0(output_path_indicators, "/commercial_bennet_data.rds")
      )

      return(commercial_data)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
