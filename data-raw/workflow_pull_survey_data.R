#' Pull survey data required for indicator creation
#'
#' @param channel an Object inherited from DBIConnection-class. .
#' This object is used to connect to communicate with the database engine.
#' @param output_path_indicators Character string. Path to folder where data pull should be saved.
#' If not NULL the pull will be saved to the
#' folder `output_path_indicators` with names (`albatrossData.rds`,
#' `bigelowData.rds`,`surveyNoLengths.rds`, `condition.rds`,`massInshoreData.rds`)
#'
#' @return list of data objects. rds files exported
#'
#'
#' @examples
#' \dontrun{
#'   channel <- dbutils::connect_to_database("server","user")
#'   output_path_indicators <- here::here()
#'   workflow_pull_survey_data(channel,output_path_indicators)
#' }
#'

workflow_pull_survey_data <- function(channel, output_path_indicators = NULL) {
  # check to skip running workflow
  tryCatch(
    {
      if (is.null(output_path_indicators)) {
        stop("output file path file missing")
      }
      # pull survey data
      survey_data <- SOEworkflows::get_survey_data(channel)

      # Save these to a specific location

      saveRDS(
        survey_data$al.data,
        paste0(output_path_indicators, "/albatrossData.rds")
      )
      saveRDS(
        survey_data$big.data,
        paste0(output_path_indicators, "/bigelowData.rds")
      )
      saveRDS(
        survey_data$survey1,
        paste0(output_path_indicators, "/surveyNoLengthsData.rds")
      )
      saveRDS(
        survey_data$condition,
        paste0(output_path_indicators, "/conditionData.rds")
      )
      saveRDS(
        survey_data$bio,
        paste0(output_path_indicators, "/surveyBiologicalData.rds")
      )
      saveRDS(
        survey_data$bio_epu,
        paste0(output_path_indicators, "/surveyBiologicalByEPUData.rds")
      )
      saveRDS(
        survey_data$mass_inshore,
        paste0(output_path_indicators, "/massInshoreData.rds")
      )
      return(survey_data)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
