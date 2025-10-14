#' Create species condition indicator
#'
#' This function calculates mean relative condition calculated from the NEFSC bottom trawl survey
#' Methods derived from Laurel Smith (https://github.com/Laurels1/Condition/blob/master/R/RelConditionEPU.R)
#'
#' @param inputPath Character string. Full path to the condition data pull rds file.
#' @param inputPathLW Character string. Full path to the LWparams csv file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param inputPathSpecies Character string. Full path to the species.codes csv file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' #create the ecodata::condition indicator
#' create_condition(
#   inputPath = "path/to/conditionData.rds",
#'  inputPathLW = "path/to/LWparams.csv",
#'  inputPathSpecies = "path/to/species.codes.csv")
#'
#' }
#'
#' @return condition, ecodata::condition data frame
#'
#' @export

create_condition <- function(
  inputPath,
  inputPathLW,
  inputPathSpecies
) {
  
  dat <- readRDS(inputPath)
  # pull out dataframe from survdat list output
  if(is.list(dat)) {
    dat <- dat$survdat
  }
  
  output <- NEesp2::species_condition(
    data = dat,
    LWparams = read.csv(inputPathLW),
    species.codes = read.csv(inputPathSpecies),
    by_EPU = TRUE,
    by_sex = FALSE,
    length_break = NULL,
    output = "soe"
  )
  
  # Remove all NAs
  output <- output |> 
    dplyr::filter(!(is.na(EPU)| is.na(Var))) 

  return(output)
}

