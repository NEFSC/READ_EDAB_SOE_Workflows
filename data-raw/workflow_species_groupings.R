#' Creates species_groupings object for ecodata
#'
#' @param input_path_species_list Character string. Full path to the original rds file from 2024 (`type = "SOE2026"`) or full path to the updated csv file from 2026 (`type = "SOE2027"`)
#' @param input_path_functional_group Character string. Full path to the functional group file (mapping SVSPP to group)
#' @param output_path_indicators Character string. Path to folder where processed data should be saved
#' @param type One of c("SOE2026", "SOE2027"). Specifies whether to build the species_groupings data set with the methods used in the 2026 or 2027 SOE.
#'
#' @return Nothing. rds file exported

#' @examples
#' \dontrun{
#'   input_path_species_list <- "path/to/SOE_species_list_old.rds"
#'   input_path_functional_group <- "path/to/functional_groups_list.rds"
#'   output_path_indicators = "path/to/output/folder"
#'   workflow_species_groupings(input_path_species_list, input_path_functional_group, output_path_indicators, type = "SOE2027")
#' }
#'

workflow_species_groupings <- function(
  input_path_species_list,
  input_path_functional_group,
  type = "SOE2027",
  output_path_indicators = NULL
) {
  # Add check to skip running workflow if data not present
  tryCatch(
    {
      if (type == "SOE2027") {
        if (
          !all(
            !is.null(output_path_indicators),
            file.exists(input_path_species_list)
          )
        ) {
          stop("Incorrect file path or file missing")
        }

        indicatorData <- SOEworkflows::create_species_groupings(
          input_path_species_list = input_path_species_list,
          type = type
        )
      }

      if (type == "SOE2026") {
        if (
          !all(
            !is.null(output_path_indicators),
            file.exists(input_path_functional_group),
            file.exists(input_path_species_list)
          )
        ) {
          stop("Incorrect file path or file missing")
        }

        # calculate indicator
        indicatorData <- SOEworkflows::create_species_groupings(
          input_path_species_list = input_path_species_list,
          input_path_functional_group = input_path_functional_group,
          type = type
        )
      }

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
