#' Wrapper to Run ALL workflows
#'
#' If running locally:
#'  * paths will need to be changed
#'  * VPN connection
#'  * a connection to the file server required


# suite of paths to input and output files
outputPath <- "~/EDAB_Indicators/"
outputPathDatasets <- "~/EDAB_Datasets/Workflows/"
inputPathSurvey <- "~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds"
inputPathMassSurvey <- "~/EDAB_Datasets/Workflows/massInshoreData.rds"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
inputPathAlbatross <- "~/EDAB_Datasets/Workflows/albatrossData.rds"
inputPathBigelow <- "~/EDAB_Datasets/Workflows/bigelowData.rds"
inputRecHMSPath <- "~/EDAB_Datasets/Workflows/hms_mrip_2025-10-03.rds"
inputPathCondition <- "~/EDAB_Datasets/Workflows/conditionData.rds"
inputPathBennet <- "~/EDAB_Datasets/Workflows/commercial_bennetData.rds"
inputPathComdat <- "~/EDAB_Datasets/Workflows/commercial_comdatData.rds"
menhadenPath <- "~/EDAB_Datasets/Workflows/menhadenEOF.rds"
static_depth <-  "~/EDAB_Datasets/Workflows/nes_bath_data.nc"
static_diagonal <- "~/EDAB_Datasets/Workflows/diag.csv"
static_coast_coord <- "~/EDAB_Datasets/Workflows/nes_coast_2.csv"
static_strat_areas <- "~/EDAB_Datasets/Workflows/stratareas.rds"
inputPathDecoder <- "~/EDAB_Datasets/Workflows/decoder.csv"
inputPathSST <- "~/EDAB_Datasets/Workflows/TS_SHP_adv_rep_MAB_GOM_GBK_NES_SCSPoly.csv"
inputKey <- "~/EDAB_Datasets/Workflows/hms_key.csv"
inputPathLW <- "~/EDAB_Datasets/Workflows/LWparams.csv"
inputPathConditionSpecies <- "~/EDAB_Datasets/Workflows/species.codes.csv"



# source workflow functions from data-raw since they are not accessible from the package installation
source(here::here("data-raw/workflow_pull_survey_data.R"))
source(here::here("data-raw/workflow_pull_commercial_data.R"))
source(here::here("data-raw/workflow_pull_recreational_data.R"))
# indicator workflows
source(here::here("data-raw/workflow_aggregate_biomass.R"))
source(here::here("data-raw/workflow_bennet.R"))
source(here::here("data-raw/workflow_comdat.R"))
source(here::here("data-raw/workflow_condition.R"))
source(here::here("data-raw/workflow_exp_n.R"))
source(here::here("data-raw/workflow_mass_inshore_survey.R"))
source(here::here("data-raw/workflow_rec_hms.R"))
source(here::here("data-raw/workflow_species_dist.R"))
source(here::here("data-raw/workflow_stock_status.R"))
source(here::here("data-raw/workflow_survey_shannon.R"))
source(here::here("data-raw/workflow_trans_dates.R"))

## Connects to the data base.
# you'll need to add the server and your user id
channel <- dbutils::connect_to_database("server","user")

# workflows for pulling data
# This is required to be run first. All indicators rely on these
# pull and write survey data
workflow_pull_survey_data(channel,outputPath = outputPathDatasets)
# pull and write commercial data
workflow_pull_commercial_data(channel,outputPath = outputPathDatasets)
# pull and write recreational data
workflow_pull_recreational_data(outputPathDatasets)

# calculate the aggregate biomass index
indicator_aggegegate_biomass <- workflow_aggregate_biomass(outputPath,
                                                           inputPathSurvey,
                                                           inputPathSpecies)
# calculate the bennet index
indicator_bennet <- workflow_bennet(inputPathBennet,
                                   inputPathSpecies,
                                   outputPath)

# calculate the comdat index
indicator_comdat <- workflow_comdat(comdat_path = inputPathComdat,
                                    input_path_species = inputPathSpecies,
                                    menhaden_path = menhadenPath,
                                    outputPathDataSets = outputPath)


# calculate condition index
indicator_condition <- workflow_condition(inputPath = inputPathCondition,
                                          inputPathLW,
                                          inputPathSpecies = inputPathConditionSpecies,
                                          outputPath)

# calculate the exp_n index
indicator_exp_n <- workflow_exp_n(inputPathBigelow,
                                  inputPathAlbatross,
                                  outputPath)


indicator_rec_hms <- workflow_rec_hms(outputPath, 
                                      inputPath = inputRecHMSPath,
                                      inputKey)

# calculate the mass_inshore_survey index
indicator_mass_inshore_survey <- workflow_mass_inshore_survey(outputPath = outputPath,
                                                           inputPathMassSurvey = inputPathMassSurvey,
                                                           inputPathSpecies = inputPathSpecies)

# calculate the species_dist index
indicator_species_dist <- workflow_species_dist(inputPathSurvey, 
                                               inputPathSpecies, 
                                               static_depth, 
                                               static_diagonal, 
                                               static_coast_coord, 
                                               static_strat_areas, 
                                               outputPath)

# calculate the stock_status index
indicator_stock_status <- workflow_stock_status(inputPath = inputPathDecoder,
                                                 outputPath)

# calculate the survey_shannon index
indicator_survey_shannon <- workflow_survey_shannon(outputPath = outputPath,
                                                    inputPathBigelow = inputPathBigelow,
                                                    inputPathAlbatross = inputPathAlbatross)

# calculate the trans_dates index
indicator_trans_dates <- workflow_trans_dates(inputPath = inputPathSST,
                                              outputPath)

