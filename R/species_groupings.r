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
#' create_species_groupings(input_path_species_list = "path/to/SOE_species_list_old.rds",
#'                          input_path_functional_group = "path/to/functional_groups_list.csv")
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

  # Original code to generate species_groupings from Sean Lucey
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

  # Code from Sarah Gaichas used to add Fed.Managed info and FMP column
  # check for Council managed, not used
  CouncilSPP <- species |>
    dplyr::filter(!is.na(Fed.Managed))

  # need to add chub mackerel as MAFMC managed species
  # and Atlantic wolffish as NEFMC managed species
  updategroupings <- species |>
    dplyr::mutate(
      Fed.Managed = dplyr::if_else(
        COMNAME == "CHUB MACKEREL",
        "MAFMC",
        Fed.Managed
      )
    ) |>
    dplyr::mutate(
      Fed.Managed = dplyr::if_else(
        COMNAME == "ATLANTIC WOLFFISH",
        "NEFMC",
        Fed.Managed
      )
    )

  # just checking, not used
  CouncilSPP <- updategroupings |>
    dplyr::filter(!is.na(Fed.Managed))

  # Jointly managed

  FMPdogfish <- data.frame(SVSPP = c(15), FMP = c("Spiny Dogfish"))

  FMPmonkfish <- data.frame(SVSPP = c(197), FMP = c("Monkfish"))

  # MAFMC

  FMPflkscupbsb <- data.frame(
    SVSPP = c(
      103, # summer flounder
      143, # scup
      141
    ), # black sea bass
    FMP = rep("Summer Flounder Scup Black Sea Bass")
  )

  FMPbluefish <- data.frame(SVSPP = c(135), FMP = c("Bluefish"))

  FMPmacksquidbutt <- data.frame(
    SVSPP = c(
      121, # Atlantic mackerel
      124, # chub mackerel
      502, # Illex squid
      503, # longfin squid
      131
    ), # butterfish
    FMP = rep("Mackerel Squid Butterfish")
  )

  FMPtilefish <- data.frame(
    SVSPP = c(
      151, # golden tilefish
      621
    ), # blueline tilefish
    FMP = rep("Tilefish")
  )

  FMPscoq <- data.frame(
    SVSPP = c(
      403, # Atlantic surfclam
      409
    ), # ocean quahog
    FMP = rep("Surfclam Ocean Quahog")
  )

  # NEFMC

  FMPnems <- data.frame(
    SVSPP = c(
      #69, # offshore hake*
      #72, # silver hake*
      73, # Atlantic cod
      74, # haddock
      75, # pollock
      76, # white hake
      #77, # red hake*
      101, # Atlantic halibut
      102, # American plaice
      105, # yellowtail flounder
      106, # winter flounder
      107, # witch flounder
      155, # Acadian redfish
      193, # ocean pout
      192
    ), # Atlantic wolffish
    FMP = rep("Northeast Multispecies")
  ) # *small mesh

  # the whitings are in NE Multispecies officially, but
  # NEFMC may want small mesh separated because they manage that way?
  # ask for feedback and recombine if necessary

  FMPsmallmesh <- data.frame(
    SVSPP = c(
      69, # offshore hake*
      72, # silver hake*
      77
    ), # red hake*
    FMP = rep("Northeast Multispecies Small Mesh")
  ) # *small mesh

  FMPscallop <- data.frame(SVSPP = c(401), FMP = c("Sea scallop"))

  FMPherring <- data.frame(SVSPP = c(32), FMP = c("Atlantic herring"))

  FMPskates <- data.frame(
    SVSPP = c(
      22, # barndoor
      23, # winter
      24, # clearnose
      25, # rosette
      26, # little
      27, # smooth
      28
    ), # thorny
    FMP = rep("Skates")
  )

  FMPredcrab <- data.frame(SVSPP = c(310), FMP = c("Red crab"))

  FMPsalmon <- data.frame(SVSPP = c(894), FMP = c("Atlantic salmon"))

  FMPs <- dplyr::bind_rows(
    FMPdogfish,
    FMPmonkfish,
    FMPflkscupbsb,
    FMPbluefish,
    FMPmacksquidbutt,
    FMPtilefish,
    FMPscoq,
    FMPnems,
    FMPsmallmesh,
    FMPscallop,
    FMPherring,
    FMPskates,
    FMPredcrab,
    FMPsalmon
  )

  speciesgroupingsFMP <- updategroupings |>
    dplyr::left_join(FMPs)

  speciesgroupingsFMP <- speciesgroupingsFMP |>
    dplyr::mutate(
      Fed.Managed = replace(Fed.Managed, COMNAME == "WINDOWPANE", "NEFMC")
    )

  species <- speciesgroupingsFMP

  # Format data.table for ecodata integration
  # Convert data.table to tibble
  species <- tibble::as_tibble(species)

  return(species)
}
