#' Wrapper to Run ALL workflows
#'
#' If running locally:
#'  * paths will need to be changed
#'  * VPN connection
#'  * a connection to the file server required


pullRawData <- FALSE
rootPath <- "~/EDAB_Datasets/Workflows/" 
# suite of paths to input and output files
outputPath <- "~/EDAB_Indicators/"
outputPathDatasets <- rootPath
inputPathSurvey <- paste0(rootPath,"surveyNoLengthsData.rds")
inputPathMassSurvey <- paste0(rootPath,"massInshoreData.rds")
inputPathSpecies <- paste0(rootPath,"SOE_species_list_24.rds")
inputPathAlbatross <- paste0(rootPath,"albatrossData.rds")
inputPathBigelow <- paste0(rootPath,"bigelowData.rds")
inputRecHMSPath <- paste0(rootPath,"hms_mrip_2025-10-03.rds")
inputPathCondition <- paste0(rootPath,"conditionData.rds")
inputPathBennet <- paste0(rootPath,"commercial_bennetData.rds")
inputPathComdat <- paste0(rootPath,"commercial_comdatData.rds")
menhadenPath <- paste0(rootPath,"menhadenEOF.rds")
static_depth <-  paste0(rootPath,"nes_bath_data.nc")
static_diagonal <- paste0(rootPath,"diag.csv")
static_coast_coord <- paste0(rootPath,"nes_coast_2.csv")
static_strat_areas <- paste0(rootPath,"stratareas.rds")
inputPathDecoder <- paste0(rootPath,"decoder.csv")
inputPathSST <- paste0(rootPath,"TS_SHP_adv_rep_MAB_GOM_GBK_NES_SCSPoly.csv")
inputKey <- paste0(rootPath,"hms_key.csv")
inputPathLW <- paste0(rootPath,"LWparams.csv")
inputPathConditionSpecies <- paste0(rootPath,"species.codes.csv")
inputPathGBSurf <- paste0(rootPath,"GB_SST_1982_to_2024_detrended.csv")
inputPathGBBot <- paste0(rootPath,"daily_bottomT_GB_1959_2024_detrended.csv")
inputPathGOMSurf <- paste0(rootPath,"GOM_SST_1982_to_2024_detrended.csv")
inputPathGOMBot <- paste0(rootPath,"daily_bottomT_GOM_1959_2024_detrended.csv")
inputPathMABSurf <- paste0(rootPath,"MAB_SST_1982_to_2024_detrended.csv")
inputPathMABBot <- paste0(rootPath,"daily_bottomT_MAB_1959_2024_detrended.csv")

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
source(here::here("data-raw/workflow_heatwave.R"))
source(here::here("data-raw/workflow_heatwave_year.R"))

if (pullRawData) {
  ## Connects to the data base.
  # you'll need to add the server and your user id
  # This is only needed to pull the data
  channel <- dbutils::connect_to_database("server","user")
  # workflows for pulling data
  # This is required to be run first. All indicators rely on these
  # pull and write survey data
  workflow_pull_survey_data(channel,outputPath = outputPathDatasets)
  # pull and write commercial data
  workflow_pull_commercial_data(channel,outputPath = outputPathDatasets)
  # pull and write recreational data
  workflow_pull_recreational_data(outputPathDatasets)
}

# calculate the aggregate biomass index
message("Running aggregate_biomass ...")
indicator_aggegegate_biomass <- workflow_aggregate_biomass(outputPath,
                                                           inputPathSurvey,
                                                           inputPathSpecies)
# calculate the bennet index
message("Running bennet ...")
indicator_bennet <- workflow_bennet(inputPathBennet,
                                   inputPathSpecies,
                                   outputPath)

# calculate the comdat index
message("Running comdat ...")
indicator_comdat <- workflow_comdat(comdat_path = inputPathComdat,
                                    input_path_species = inputPathSpecies,
                                    menhaden_path = menhadenPath,
                                    outputPathDataSets = outputPath)


# calculate condition index
message("Running condition ...")
indicator_condition <- workflow_condition(inputPath = inputPathCondition,
                                          inputPathLW,
                                          inputPathSpecies = inputPathConditionSpecies,
                                          outputPath)

# calculate the exp_n index
message("Running exp_n ...")
indicator_exp_n <- workflow_exp_n(inputPathBigelow,
                                  inputPathAlbatross,
                                  outputPath)

# calculate rec_hms index
message("Running rec_hms ...")
indicator_rec_hms <- workflow_rec_hms(outputPath, 
                                      inputPath = inputRecHMSPath,
                                      inputKey)

# calculate the mass_inshore_survey index
message("Running mass_inshore_survey ...")
indicator_mass_inshore_survey <- workflow_mass_inshore_survey(outputPath = outputPath,
                                                           inputPathMassSurvey = inputPathMassSurvey,
                                                           inputPathSpecies = inputPathSpecies)

# calculate the species_dist index
message("Running species_dist ...")
indicator_species_dist <- workflow_species_dist(inputPathSurvey, 
                                               inputPathSpecies, 
                                               static_depth, 
                                               static_diagonal, 
                                               static_coast_coord, 
                                               static_strat_areas, 
                                               outputPath)

# calculate the stock_status index
message("Running stock_status ...")
indicator_stock_status <- workflow_stock_status(inputPath = inputPathDecoder,
                                                 outputPath)

# calculate the survey_shannon index
message("Running survey_shannon ...")
indicator_survey_shannon <- workflow_survey_shannon(outputPath = outputPath,
                                                    inputPathBigelow = inputPathBigelow,
                                                    inputPathAlbatross = inputPathAlbatross)

# calculate the trans_dates index
message("Running trans_dates ...")
indicator_trans_dates <- workflow_trans_dates(inputPath = inputPathSST,
                                              outputPath)

# calculate the heatwave index
message("Running heatwave ...")
indicator_heatwave <- workflow_heatwave(inputPathGBBot = inputPathGBBot,
                                        inputPathGOMBot = inputPathGOMBot,
                                        inputPathMABBot = inputPathMABBot,
                                        inputPathGBSurf = inputPathGBSurf,
                                        inputPathGOMSurf = inputPathGOMSurf,
                                        inputPathMABSurf = inputPathMABSurf,
                                        outputPath)




# calculate the heatwave_year index
message("Running heatwave_year ...")
indicator_heatwave_year <- workflow_heatwave_year(inputPathGBBot = inputPathGBBot,
                                                  inputPathGOMBot = inputPathGOMBot,
                                                  inputPathMABBot = inputPathMABBot,
                                                  inputPathGBSurf = inputPathGBSurf,
                                                  inputPathGOMSurf = inputPathGOMSurf,
                                                  inputPathMABSurf = inputPathMABSurf,
                                                  outputPath)

