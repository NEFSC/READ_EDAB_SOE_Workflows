#' Calculates species_groupings data set for automated workflow
#'
#' @description
#' This data object is used in many of the other workflows in table joins
#' It is formatted exactly like the ecodata data object
#'
#' @param input_path_species_list Character string. Full path to the original rds file from 2024
#' @param input_path_functional_group Character string. Full path to the species functional group csv file. SVSPP codes are mapped to functional group
#'
#' @examples
#' \dontrun{
#' # create the ecodata::species_groupings table
#' create_species_groupings(input_path_soe_species_list = "path/to/SOE_species_list_old.rds",
#'                          input_path_species = "path/to/functional_groups_list.csv")
#'
#' }
#'
#' @import data.table
#'
#' @return ecodata::species_groupings data frame
#'
#' @export

create_species_groupings <- function(
  input_path_species_list,
  input_path_functional_group
) {
  end.year <- format(Sys.Date(), "%Y")
  # Add some checks (maybe create a check function to be used by other functions)

  #Old/base species list
  species <- readRDS(input_path_species_list)

  #New functional groups
  newspp <- data.table::as.data.table(readr::read_csv(
    input_path_functional_group,
    comment = "#",
    show_col_types = FALSE
  ))

  data.table::setnames(newspp, c('svspp', 'Grp'), c('SVSPP', 'SOE.24'))
  newspp[, c('Grp Num', 'com_name', 'Sci_name') := NULL]

  species <- merge(species, newspp, by = 'SVSPP', all.x = T)
  species[is.na(SOE.24) & !is.na(SOE.20), SOE.24 := SOE.20]
  species[is.na(SOE.24), SOE.24 := 'Other']
  species[SOE.20 == 'Benthos', SOE.24 := 'Benthos']

  #Fix species without SVSPP codes
  species[ITISSPP %in% c(159753, 167196, 167686, 98276), SOE.24 := 'Benthivore']
  species[ITISSPP %in% c(172564, 162028), SOE.24 := 'Planktivore']
  species[COMNAME %like% 'PORGY', SOE.24 := 'Benthivore']
  species[COMNAME %like% 'TRIGGER', SOE.24 := 'Benthivore']

  #Set fish unclassified
  species[NESPP3 == 526 & COMNAME != 'FISH, OTHER', NESPP3 := NA]

  #Fix duplicate common names
  species <- species[!COMNAME %in% c('MACKEREL,CHUB', 'SPADEFISH'), ]

  return(species)
}
