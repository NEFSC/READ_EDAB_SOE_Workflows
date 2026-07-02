#' Wrapper to Run ALL workflows
#'
#' If running locally:
#'  * paths will need to be changed
#'  * VPN connection
#'  * a connection to the file server required

pullRawData <- FALSE
rootPath <- "~/EDAB_Datasets/Workflows/"
# suite of paths to input and output files
output_path_indicators <- "~/EDAB_Indicators/"
#output_path_indicators <- "~/EDAB_Dev/beet/"
output_path_datasets <- rootPath
input_path_survey <- paste0(rootPath, "surveyNoLengthsData.rds")
input_path_mass_survey <- paste0(rootPath, "massInshoreData.rds")
input_path_species <- paste0(rootPath, "SOE_species_list_24.rds")
input_path_albatross <- paste0(rootPath, "albatrossData.rds")
input_path_bigelow <- paste0(rootPath, "bigelowData.rds")
input_path_rec <- paste0(rootPath, "hms_mrip_2025-10-03.rds")
input_path_condition <- paste0(rootPath, "conditionData.rds")
input_path_bennet <- paste0(rootPath, "commercial_bennetData.rds")
input_path_comdat <- paste0(rootPath, "commercial_comdatData.rds")
input_path_menhaden <- paste0(rootPath, "menhadenEOF.rds")
input_path_static_depth <- paste0(rootPath, "nes_bath_data.nc")
input_path_static_diagonal <- paste0(rootPath, "diag.csv")
input_path_static_coast_coord <- paste0(rootPath, "nes_coast_2.csv")
input_path_static_strat_areas <- paste0(rootPath, "stratareas.rds")
inputPathDecoder <- paste0(rootPath, "decoder.csv")
inputPathSST <- paste0(rootPath, "TS_SHP_adv_rep_MAB_GOM_GBK_NES_SCSPoly.csv")
input_path_rec_key <- paste0(rootPath, "hms_key.csv")
input_path_lw_coeffs <- paste0(rootPath, "LWparams.csv")
input_path_conditionSpecies <- paste0(rootPath, "species.codes.csv")
input_path_gb_surf <- paste0(rootPath, "GB_SST_1982_to_2024_detrended.csv")
input_path_gb_bot <- paste0(rootPath, "daily_bottomT_GB_1959_2024_detrended.csv")
input_path_gom_surf <- paste0(rootPath, "GOM_SST_1982_to_2024_detrended.csv")
input_path_gom_bot <- paste0(rootPath, "daily_bottomT_GOM_1959_2024_detrended.csv")
input_path_mab_surf <- paste0(rootPath, "MAB_SST_1982_to_2024_detrended.csv")
input_path_mab_bot <- paste0(rootPath, "daily_bottomT_MAB_1959_2024_detrended.csv")
input_survey_bio_epu <- paste0(rootPath, "surveyBiologicalByEPUData.rds")
input_survey_bio <- paste0(rootPath, "surveyBiologicalData.rds")
input_static_lw_table <- paste0(rootPath, "df_lw.rda")
input_path_species <- paste0(rootPath, "SOE_species_list_24.rds")
input_static_length_convert <- paste0(rootPath, "df_lconv.rda")

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
source(here::here("data-raw/workflow_productivity_anomaly.R"))

if (pullRawData) {
  ## Connects to the data base.
  # you'll need to add the server and your user id
  # This is only needed to pull the data
  channel <- dbutils::connect_to_database("server", "user")
  # workflows for pulling data
  # This is required to be run first. All indicators rely on these
  # pull and write survey data
  workflow_pull_survey_data(channel, output_path_indicators = output_path_datasets)
  # pull and write commercial data
  workflow_pull_commercial_data(channel, output_path_indicators = output_path_datasets)
  # pull and write recreational data
  workflow_pull_recreational_data(output_path_datasets)
}

# calculate the aggregate biomass index
message("Running aggregate_biomass ...")
indicator_aggegegate_biomass <- workflow_aggregate_biomass(
  output_path_indicators,
  input_path_survey,
  input_path_species
)
# calculate the bennet index
message("Running bennet ...")
indicator_bennet <- workflow_bennet(
  input_path_bennet,
  input_path_species,
  output_path_indicators
)

# calculate the comdat index
message("Running comdat ...")
indicator_comdat <- workflow_comdat(
  input_path_comdat = input_path_comdat,
  input_path_species = input_path_species,
  input_path_menhaden = input_path_menhaden,
  output_path_datasets = output_path_indicators
)


# calculate condition index
message("Running condition ...")
indicator_condition <- workflow_condition(
  inputPath = input_path_condition,
  input_path_lw_coeffs,
  input_path_species = input_path_conditionSpecies,
  output_path_indicators
)

# calculate the exp_n index
message("Running exp_n ...")
indicator_exp_n <- workflow_exp_n(
  input_path_bigelow,
  input_path_albatross,
  output_path_indicators
)

# calculate rec_hms index
message("Running rec_hms ...")
indicator_rec_hms <- workflow_rec_hms(
  output_path_indicators,
  inputPath = input_path_rec,
  input_path_rec_key
)

# calculate the mass_inshore_survey index
message("Running mass_inshore_survey ...")
indicator_mass_inshore_survey <- workflow_mass_inshore_survey(
  output_path_indicators = output_path_indicators,
  input_path_mass_survey = input_path_mass_survey,
  input_path_species = input_path_species
)

# calculate the species_dist index
message("Running species_dist ...")
indicator_species_dist <- workflow_species_dist(
  input_path_survey,
  input_path_species,
  input_path_static_depth,
  input_path_static_diagonal,
  input_path_static_coast_coord,
  input_path_static_strat_areas,
  output_path_indicators
)

# calculate the stock_status index
message("Running stock_status ...")
indicator_stock_status <- workflow_stock_status(
  inputPath = inputPathDecoder,
  output_path_indicators
)

# calculate the survey_shannon index
message("Running survey_shannon ...")
indicator_survey_shannon <- workflow_survey_shannon(
  output_path_indicators = output_path_indicators,
  input_path_bigelow = input_path_bigelow,
  input_path_albatross = input_path_albatross
)

# calculate the trans_dates index
message("Running trans_dates ...")
indicator_trans_dates <- workflow_trans_dates(
  inputPath = inputPathSST,
  output_path_indicators
)

# calculate the heatwave index
message("Running heatwave ...")
indicator_heatwave <- workflow_heatwave(
  input_path_gb_bot = input_path_gb_bot,
  input_path_gom_bot = input_path_gom_bot,
  input_path_mab_bot = input_path_mab_bot,
  input_path_gb_surf = input_path_gb_surf,
  input_path_gom_surf = input_path_gom_surf,
  input_path_mab_surf = input_path_mab_surf,
  output_path_indicators
)


# calculate the heatwave_year index
message("Running heatwave_year ...")
indicator_heatwave_year <- workflow_heatwave_year(
  input_path_gb_bot = input_path_gb_bot,
  input_path_gom_bot = input_path_gom_bot,
  input_path_mab_bot = input_path_mab_bot,
  input_path_gb_surf = input_path_gb_surf,
  input_path_gom_surf = input_path_gom_surf,
  input_path_mab_surf = input_path_mab_surf,
  output_path_indicators
)


message("Running productivity_anomaly . .")

indicator_productivity_anomaly <- workflow_productivity_anomaly(
  input_survey_bio_epu = input_survey_bio_epu,
  input_survey_bio = input_survey_bio,
  input_static_lw_table = input_static_lw_table,
  input_path_species = input_path_species,
  input_static_length_convert = input_static_length_convert,
  output_path_indicators = output_path_indicators
)
