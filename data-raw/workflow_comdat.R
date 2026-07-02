#' Calculates comdat data set for automated workflow
#'
#' This uses the commercial data pull from the comlandr package.
#'
#' @param input_path_comdat Character string. Full path to the commercial data rds file for comdat indicator
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#' @param menhaden_path Character string. Full path to the menhaden data .rds file
#'
#' @return list
#' \item{comdat}{`ecodata::comdat` data frame}
#' \item{comdat_species}{species data used to create the `comdat` indicator}
#'
#' @example
#' \dontrun{
#' workflow_comdat(
#'    input_path_comdat = "path/to/commerical_comdat.rds",
#'    input_path_species = "path/to/species/data/.rds",
#'    menhaden_path = "path/to/menhaden/data/.rds",
#'    outputPath = "path/to/output/folder"
#'    )
#' }
#'
#' @section Dependencies:
#'
#' This assumes that the commercial data has been pulled and resides in the path `input_path_comdat`
#' and that create_menhaden_input.R has been run and outputs saved to `menhaden_path`
#'
#' @export

workflow_comdat <- function(
  input_path_comdat,
  input_path_species,
  menhaden_path,
  outputPath
) {
  # Add check to skip running workflow if data not present

  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
          file.exists(input_path_comdat),
          file.exists(input_path_species),
          file.exists(menhaden_path)
        )
      ) {
        stop("Incorrect file path or file missing")
      }

      # calculate indicator
      indicatorData <- SOEworkflows::create_comdat(
        input_path_comdat = input_path_comdat,
        input_path_species = input_path_species,
        menhaden_path = menhaden_path,
        outputPathDataSets = outputPath
      )

      saveRDS(indicatorData$comdat, paste0(outputPath, "/comdat.rds"))
      saveRDS(
        indicatorData$comdat_species,
        paste0(outputPath, "/comdat_species.rds")
      )
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}
