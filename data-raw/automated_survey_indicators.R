### Pull_survey_data

# input = channel <- dbutils::connect_to_database("server","user")
# outputs = //nefscdata/EDAB_Datasets/Workflows/Survey_Data/survey_no_lengths_data.rds, albatross_data.rds, bigelow_data.rds, condition_data.rds, survey_biological_data.rds, survey_biological_by_epu_data.rds, mass_inshore_data.rds
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
input_file <- list.files(
  input_folder,
  pattern = "^survey_no_lengths_data\\.rds$",
  full.names = TRUE
)

if (length(input_file) == 0) {
  stop(paste0(
    "Survey data not found in input folder: ",
    input_folder
  ))
}

## (2) find supplemental data file (SOE_species_list_24.rds)
message("Looking for SOE species list...")
supplemental_file <- list.files(
  supplemental_folder,
  pattern = "^SOE_species_list_24\\.rds$",
  full.names = TRUE
)

if (length(supplemental_file) == 0) {
  stop(paste0(
    "Species data not found in supplemental folder: ",
    supplemental_folder
  ))
}

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

###################################
### Condition

# input = condition_data.rds
# outputs = condition.rds
# supplemental input = species.codes.rda, LWparams.rda

#Command Line Local
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "//nefscdata/EDAB_Datasets/Workflows/Survey_Data" "//nefscdata/EDAB_Indicators/SOE_ecodata" "//nefscdata/EDAB_Resources/workflow_resources/soe_workflows"

# Command Line Cloud
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "~/EDAB_Datasets/Workflows/Survey_Data" "~/EDAB_Indicators/SOE_ecodata" "~/EDAB_Resources/workflow_resources/soe_workflows"

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
  supplemental_folder = '//nefscdata/EDAB_Resources/workflow_resources/soe_workflows'

  # file path to use from container
  input_folder = '~/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '~/EDAB_Indicators/SOE_ecodata'
  supplemental_folder = '~/EDAB_Resources/workflow_resources/soe_workflows'

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

## (1) find survey data input file (condition_data.rds)
message("Looking for condition_data...")
input_file <- list.files(
  input_folder,
  pattern = "^condition_data\\.rds$",
  full.names = TRUE
)

if (length(input_file) == 0) {
  stop(paste0(
    "Condition data not found in input folder: ",
    input_folder
  ))
}

## (2) find supplemental data files (LWparams.rda, species.codes.rda)
###### CHECK THIS FOR MULTIPLE SUPPLEMENTAL FILES IN THE FOLDER
message("Looking for LWparams...")
supplemental_file_lw <- list.files(
  supplemental_folder,
  pattern = "^LWparams\\.rda$",
  full.names = TRUE
)

if (length(supplemental_file_lw) == 0) {
  stop(paste0(
    "LWparams not found in supplemental folder: ",
    supplemental_folder
  ))
}

message("Looking for species.codes...")
supplemental_file_species <- list.files(
  supplemental_folder,
  pattern = "^species.codes\\.rda$",
  full.names = TRUE
)

if (length(supplemental_file_species) == 0) {
  stop(paste0(
    "Species.codes not found in supplemental folder: ",
    supplemental_folder
  ))
}

## (3) run condition
message("Calculating condition...")
indicatorData <- SOEworkflows::create_condition(
  input_path_condition = input_file,
  input_path_lw_coeffs = supplemental_file_lw,
  input_path_species = supplemental_file_species
)

# (3) write data to file
message("Writing species condition to file...")
fname <- paste0(output_folder, "/condition.rds")
saveRDS(indicatorData, fname)

message("Data saved at: ", fname)
message("Done: Species condition")

###################################
### Expected number of species (exp_n)

# input = albatross_data.rds, bigelow_data.rds
# outputs = exp_n.rds

#Command Line Local
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "//nefscdata/EDAB_Datasets/Workflows/Survey_Data" "//nefscdata/EDAB_Indicators/SOE_ecodata"

# Command Line Cloud
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "~/EDAB_Datasets/Workflows/Survey_Data" "~/EDAB_Indicators/SOE_ecodata"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  print('Using command line arguments')
} else {
  # file path to use in local R
  input_folder = '//nefscdata/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '//nefscdata/EDAB_Indicators/SOE_ecodata'

  # file path to use from container
  input_folder = '~/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '~/EDAB_Indicators/SOE_ecodata'

  message('Using default arguments')
}

message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))

check.dir = function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

check.dir(output_folder)
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}

## (1) find survey data input file (albatross_data.rds)
message("Looking for albatross_data...")
albatross_input_file <- list.files(
  input_folder,
  pattern = "^albatross_data\\.rds$",
  full.names = TRUE
)

if (length(albatross_input_file) == 0) {
  stop(paste0(
    "Albatross data not found in input folder: ",
    input_folder
  ))
}

message("Looking for bigelow_data...")
bigelow_input_file <- list.files(
  input_folder,
  pattern = "^bigelow_data\\.rds$",
  full.names = TRUE
)

if (length(bigelow_input_file) == 0) {
  stop(paste0(
    "Bigelow data not found in input folder: ",
    input_folder
  ))
}

## (3) run exp_n
message("Calculating exp_n...")
indicatorData <- SOEworkflows::create_exp_n(
  input_path_albatross = albatross_input_file,
  input_path_bigelow = bigelow_input_file
)

# (3) write data to file
message("Writing exp n to file...")
fname <- paste0(output_folder, "/exp_n.rds")
saveRDS(indicatorData, fname)

message("Data saved at: ", fname)
message("Done: Exp n")

###################################
### Mass inshore survey

# input = mass_inshore_data.rds
# output = mass_inshore_survey.rds
# supplemental = SOE_species_list_24.rds

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

## (1) find survey data input file (mass_inshore_data.rds)
message("Looking for mass inshore data...")
input_file <- list.files(
  input_folder,
  pattern = "^mass_inshore_data\\.rds$",
  full.names = TRUE
)

if (length(input_file) == 0) {
  stop(paste0(
    "Mass inshore data not found in input folder: ",
    input_folder
  ))
}

## (2) find supplemental data file (SOE_species_list_24.rds)
message("Looking for SOE species list...")
supplemental_file <- list.files(
  supplemental_folder,
  pattern = "^SOE_species_list_24\\.rds$",
  full.names = TRUE
)

if (length(supplemental_file) == 0) {
  stop(paste0(
    "Species data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## (3) run mass inshore survey
message("Calculating mass inshore survey...")
indicatorData <- SOEworkflows::create_mass_inshore_survey(
  input_path_mass_survey = input_file,
  input_path_species = supplemental_file
)

# (3) write data to file
message("Writing mass inshore survey data to file...")
fname <- paste0(output_folder, "/mass_inshore_survey.rds")
saveRDS(indicatorData, fname)


message("Data saved at: ", fname)
message("Done: Mass inshore survey")

###################################
### Productivity Anomaly

# input = survey_biological_data.rds, survey_biological_by_epu_data.rds
# output = productivity_anomaly.rds
# supplemental = (EDAB_Resources/static_workflow_inputs/) df_lconv.rda, df_lw.rda, SOE_species_list_24.rds

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

## (1) find survey bio input file (survey_biological_data.rds)
message("Looking for survey bio data...")
survey_bio_file <- list.files(
  input_folder,
  pattern = "^survey_biological_data\\.rds$",
  full.names = TRUE
)

if (length(survey_bio_file) == 0) {
  stop(paste0(
    "Survey bio data not found in input folder: ",
    input_folder
  ))
}

## find survey bio EPU input file (survey_biological_by_epu_data.rds)
message("Looking for survey bio by EPU data...")
survey_bio_epu_file <- list.files(
  input_folder,
  pattern = "^survey_biological_by_epu_data\\.rds$",
  full.names = TRUE
)

if (length(survey_bio_epu_file) == 0) {
  stop(paste0(
    "Survey bio by EPU data not found in input folder: ",
    input_folder
  ))
}

## (2) find supplemental length conversion data file (df_lconv.rda)
message("Looking for df_lconv...")
lconv_file <- list.files(
  supplemental_folder,
  pattern = "^df_lconv\\.rda$",
  full.names = TRUE
)

if (length(lconv_file) == 0) {
  stop(paste0(
    "Length conversion data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## find supplemental length-weight data file (df_lw.rda)
message("Looking for df_lw...")
lw_file <- list.files(
  supplemental_folder,
  pattern = "^df_lw\\.rda$",
  full.names = TRUE
)

if (length(lw_file) == 0) {
  stop(paste0(
    "Length weight data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## find supplemental data file (SOE_species_list_24.rds)
message("Looking for SOE species list...")
species_file <- list.files(
  supplemental_folder,
  pattern = "^SOE_species_list_24\\.rds$",
  full.names = TRUE
)

if (length(species_file) == 0) {
  stop(paste0(
    "Species data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## (3) run productivity anomaly
message("Calculating productivity anomaly...")
indicatorData <- SOEworkflows::create_productivity_anomaly(
  input_survey_bio_epu = survey_bio_epu_file,
  input_survey_bio = survey_bio_file,
  input_static_lw_table = lw_file,
  input_path_species = species_file,
  input_static_length_convert = lconv_file
)

# (3) write data to file
message("Writing productivity anomaly data to file...")
fname <- paste0(output_folder, "/productivity_anomaly.rds")
saveRDS(indicatorData, fname)


message("Data saved at: ", fname)
message("Done: Productivity anomaly")

###################################
### Species dist

# input = survey_no_lengths_data.rds
# output = species_dist.rds
# supplemental = (EDAB/Resources/static_workflow_inputs) SOE_species_list_24.rds, nes_bath_data.nc, diag.csv, nes_coast_2.csv, stratareas.rds

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

## (1) find survey no lengths input file (survey_no_lengths_data.rds)
message("Looking for survey bio data...")
input_file <- list.files(
  input_folder,
  pattern = "^survey_no_lengths_data\\.rds$",
  full.names = TRUE
)

if (length(input_file) == 0) {
  stop(paste0(
    "Survey no lengths data not found in input folder: ",
    input_folder
  ))
}

## (2) find supplemental data file (SOE_species_list_24.rds)
message("Looking for SOE species list...")
species_file <- list.files(
  supplemental_folder,
  pattern = "^SOE_species_list_24\\.rds$",
  full.names = TRUE
)

if (length(species_file) == 0) {
  stop(paste0(
    "Species data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## find supplemental NES bath data file (nes_bath_data.nc)
message("Looking for NES bath data...")
bath_file <- list.files(
  supplemental_folder,
  pattern = "^nes_bath_data\\.nc$",
  full.names = TRUE
)

if (length(bath_file) == 0) {
  stop(paste0(
    "NES bath data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## find supplemental diag data file (diag.csv)
message("Looking for diag...")
diag_file <- list.files(
  supplemental_folder,
  pattern = "^diag\\.csv$",
  full.names = TRUE
)

if (length(diag_file) == 0) {
  stop(paste0(
    "Diag data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## find supplemental NES coast file (nes_coast_2.csv)
message("Looking for NES coast file...")
coast_file <- list.files(
  supplemental_folder,
  pattern = "^nes_coast_2\\.csv$",
  full.names = TRUE
)

if (length(coast_file) == 0) {
  stop(paste0(
    "NES coast data not found in supplemental folder: ",
    supplemental_folder
  ))
}

## find supplemental strat areas file (stratareas.rds)
message("Looking for strat areas file...")
strat_file <- list.files(
  supplemental_folder,
  pattern = "^stratareas\\.rds$",
  full.names = TRUE
)

if (length(strat_file) == 0) {
  stop(paste0(
    "Strat areas not found in supplemental folder: ",
    supplemental_folder
  ))
}


## (3) run species_dist
message("Calculating species dist...")
indicatorData <- SOEworkflows::create_species_dist(
  input_path_survey = input_file,
  input_path_species = species_file,
  input_path_static_depth = bath_file,
  input_path_static_diagonal = diag_file,
  input_path_static_coast_coord = coast_file,
  input_path_static_strat_areas = strat_file
)

# (3) write data to file
message("Writing species dist data to file...")
fname <- paste0(output_folder, "/species_dist.rds")
saveRDS(indicatorData, fname)


message("Data saved at: ", fname)
message("Done: Species dist")

###################################
### Survey shannon

# input = albatross_data.rds, bigelow_data.rds
# outputs = survey_shannon.rds

#Command Line Local
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "//nefscdata/EDAB_Datasets/Workflows/Survey_Data" "//nefscdata/EDAB_Indicators/SOE_ecodata"

# Command Line Cloud
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i94-format-survey-automation/data-raw/automated_survey_indicators.R "~/EDAB_Datasets/Workflows/Survey_Data" "~/EDAB_Indicators/SOE_ecodata"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  print('Using command line arguments')
} else {
  # file path to use in local R
  input_folder = '//nefscdata/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '//nefscdata/EDAB_Indicators/SOE_ecodata'

  # file path to use from container
  input_folder = '~/EDAB_Datasets/Workflows/Survey_Data'
  output_folder = '~/EDAB_Indicators/SOE_ecodata'

  message('Using default arguments')
}

message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))

check.dir = function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

check.dir(output_folder)
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}

## (1) find survey data input file (albatross_data.rds)
message("Looking for albatross_data...")
albatross_input_file <- list.files(
  input_folder,
  pattern = "^albatross_data\\.rds$",
  full.names = TRUE
)

if (length(albatross_input_file) == 0) {
  stop(paste0(
    "Albatross data not found in input folder: ",
    input_folder
  ))
}

message("Looking for bigelow_data...")
bigelow_input_file <- list.files(
  input_folder,
  pattern = "^bigelow_data\\.rds$",
  full.names = TRUE
)

if (length(bigelow_input_file) == 0) {
  stop(paste0(
    "Bigelow data not found in input folder: ",
    input_folder
  ))
}

## (3) run survey shannon
message("Calculating survey shannon...")
indicatorData <- SOEworkflows::create_survey_shannon(
  input_path_albatross = albatross_input_file,
  input_path_bigelow = bigelow_input_file
)

# (3) write data to file
message("Writing survey shannon to file...")
fname <- paste0(output_folder, "/survey_shannon.rds")
saveRDS(indicatorData, fname)

message("Data saved at: ", fname)
message("Done: Survey shannon")
