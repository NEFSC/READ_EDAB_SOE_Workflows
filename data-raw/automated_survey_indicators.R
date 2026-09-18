### Pull_survey_data

# input = channel <- dbutils::connect_to_database("server","user")
# outputs = survey_no_lengths_data.rds, albatross_data.rds, bigelow_data.rds, condition_data.rds, survey_biological_data.rds, survey_biological_by_epu_data.rds, mass_inshore_data.rds
### How to format the channel input as a folder?
#Command Line Local
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "input path" "//nefscdata/EDAB_Datasets/Workflows/Survey_Data"

# Command Line Cloud
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "input path" "~/EDAB_Datasets/Workflows/Survey_Data"

############################################
### Aggregate biomass
# input = survey_no_lengths_data.rds
# outputs = aggregate_biomass.rds, aggregate_biomass_species.rds
# supplemental input = SOE_species_list_24.rds

#Command Line Local
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "//nefscdata/EDAB_Datasets/Workflows/Survey_Data" "//nefscdata/EDAB_Indicators/SOE_ecodata" "//nefscdata/EDAB_Resources/static_workflow_inputs"

# Command Line Cloud
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "~/EDAB_Datasets/Workflows/Survey_Data" "~/EDAB_Indicators/SOE_ecodata" "~/EDAB_Resources/static_workflow_inputs"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  supplemental_folder = args[3]
  print('Using command line arguments')
} else {
  # file path to use in local R
  input_folder = '//nefscdata/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '//nefscdata/EDAB_Indicators/SOE_ecodata'
  supplemental_folder = '//nefscdata/EDAB_Resources/static_workflow_inputs'

  # file path to use from container
  input_folder = '~/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '~/EDAB_Indicators/SOE_ecodata'
  supplemental_folder = '~/EDAB_Resources/static_workflow_inputs'

  message('Using default arguments')
}

message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))
message(paste0('supplemental_folder: ', supplemental_folder))

check.dir = function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

check.dir(output_folder)
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}

## (1) find survey data input file (survey_no_lengths_data.rds)
message("Looking for surveyNoLengths...")
files <- list.files(
  input_folder,
  pattern = "^survey_no_lengths_data\\.rds$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(paste0(
    "Survey data not found in input folder: ",
    input_folder
  ))
}

input_file <- sort(files)[length(files)] # Get the most recent file

## (2) find supplemental data file (SOE_species_list_24.rds)
message("Looking for SOE species list...")
files <- list.files(
  supplemental_folder,
  pattern = "^SOE_species_list_24\\.rds$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(paste0(
    "Species data not found in supplemental folder: ",
    supplemental_folder
  ))
}

supplemental_file <- sort(files)[length(files)] # Get the most recent file

## (3) run aggregated biomass
message("Calculating aggregated biomass...")
indicatorData <- SOEworkflows::create_aggregate_biomass(
  input_path_survey = input_file,
  input_path_species = supplemental_file
)

# (3) write data to file
message("Writing aggregate biomass to file...")
fname <- paste0(output_folder, "/aggregate_biomass.rds")
saveRDS(indicatorData, fname)

# how to write 2 output files (Agg bio has aggregate_biomass.rds and aggregate_biomass_species.rds)
message("Writing aggregate biomass species to file...")
fname2 <- paste0(output_folder, "/aggregate_biomass_species.rds")
saveRDS(indicatorData, fname2)

message("Data saved at: ", fname)
message("Data saved at: ", fname2)
message("Done: Aggregate biomass")
