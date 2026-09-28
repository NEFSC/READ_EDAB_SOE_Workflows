#' Creates species_groupings object for ecodata
#'
#' @param input_path_species_list Character string. Full path to the original species list table
#' @param input_path_functional_group Character string. Full path to the functional group file (mapping SVSPP to group)
#' @param output_path_indicators Character string. Path to folder where processed data should be saved
#'
#' @return Nothing. rds file exported

#' @examples
#' \dontrun{
#'   input_path_species_list <- "path/to/SOE_species_list_old.rds"
#'   input_path_functional_group <- "path/to/functional_groups_list.rds"
#'   output_path_indicators = "path/to/output/folder"
#'   workflow_species_groupings(input_path_species_list, input_path_functional_group, output_path_indicators)
#' }
#'

workflow_species_groupings <- function(
  input_path_species_list,
  input_path_functional_group,
  output_path_indicators = NULL
) {
  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (
        !all(
          !is.null(output_path_indicators),
          file.exists(input_path_species_list),
          file.exists(input_path_functional_group)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_species_groupings(
        input_path_species_list = input_path_species_list,
        input_path_functional_group = input_path_functional_group
      )

      # Write data to file
      saveRDS(
        indicatorData,
        paste0(output_path_indicators, "/species_groupings.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
