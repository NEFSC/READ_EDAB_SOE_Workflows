#' Creates recreational HMS indicator
#'
#' This uses the output file from 'pull_rec_hms' as an input parameter
#' The function formats the data to be used in the SOE
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_rec Character string. Full path to the data from the 'pull_rec_hms' csv file in EDAB_Dev.
#' @param input_path_rec_key Character string. Full path to the hms_key file in EDAB_Resources ("EDAB_Resources/workflow_resources/soe_workflows/hms_key.csv")
#'
#' @examples
#' \dontrun{
#' #create the ecodata::rec_hms indicator
#' create_rec_hms(
#   input_path_rec = "path/to/hms_mrip_2025-08-26.csv",
#'  input_path_rec_key = "path/to/hms_key.csv")
#'
#' }
#'
#' @return rec_hms, ecodata::rec_hms data frame
#'
#' @export

create_rec_hms <- function(
  input_path_rec,
  input_path_rec_key
) {
  # data wrangling ----
  rec_hms <- readRDS(input_path_rec) |>
    dplyr::left_join(
      read.csv(input_path_rec_key) |>
        dplyr::select(COMMON_NAME, SP_CATEGORY),
      by = c("SPECIES" = "COMMON_NAME")
    ) |>
    dplyr::group_by(YEAR, SP_CATEGORY, REGION) |>
    dplyr::summarise(
      Value = sum(DATA_VALUE)
    ) |>
    dplyr::rename(
      Var = SP_CATEGORY,
      Time = YEAR,
      EPU = REGION
    ) |>
    dplyr::mutate(
      EPU = dplyr::case_when(
        EPU == "MID-ATLANTIC" ~ "MAB",
        EPU == "NORTH ATLANTIC" ~ "NE"
      )
    ) |>
    dplyr::mutate(
      Var = paste0(Var, "-", EPU)
    ) |>
    dplyr::select(Time, Var, Value, EPU) |>
    dplyr::filter(!stringr::str_detect(Var, "Pelagic")) # Remove pelagics from this dataset per HMS request

  return(rec_hms)
}
