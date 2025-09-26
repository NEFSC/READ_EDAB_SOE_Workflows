#' Create species condition indicator
#'
#' This function calculates mean relative condition calculated from the NEFSC bottom trawl survey
#' Methods derived from Laurel Smith (https://github.com/Laurels1/Condition/blob/master/R/RelConditionEPU.R)
#'
#' @param inputPath Character string. Full path to the condition data pull rds file.
#' @param inputpathLW Character string. Full path to the LWparams csv file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#' @param inputpathSpecies Character string. Full path to the species.codes csv file in 'EDAB_Resources/workflow_resources/soe_workflows'.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' #create the ecodata::condition indicator
#' species_condition(
#   inputPath = "path/to/condition.rds",
#'  inputpathLW = "path/to/LWparams.csv",
#'  inputpathSpecies = "path/to/species.codes.csv")
#'
#' }
#'
#' @return condition, ecodata::condition data frame
#'
#' @export

create_species_condition <- function(
  inputPath,
  inputpathLW,
  inputpathSpecies
) {
  
  dat <- readRDS(inputPath)
  # pull out dataframe from survdat list output
  if(is.list(dat)) {
    dat <- dat$survdat
  }
  
  output <- NEesp2::species_condition(
    data = dat,
    LWparams = read.csv(inputpathLW),
    species.codes = read.csv(inputpathSpecies),
    by_EPU = TRUE,
    by_sex = FALSE,
    length_break = NULL,
    more_than_20_years = TRUE,
    record_outliers = FALSE,
    output = "soe"
  )

  return(output)
}
