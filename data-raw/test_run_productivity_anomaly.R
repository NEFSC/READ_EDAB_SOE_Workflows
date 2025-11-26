# To review pull request for productivity_anomaly

# run workflow_productivity anomaly from the container --------
## set paths for data inputs generated in workflow ------------
input_survey_bio_epu <- "~/EDAB_Datasets/Workflows/surveyBiologicalByEPUData.rds"
input_survey_bio <- "~/EDAB_Datasets/Workflows/surveyBiologicalData.rds"
input_static_lw_table <- "~/EDAB_Datasets/Workflows/df_lw.rda"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
outputPath <- "~/EDAB_Indicators/"
input_static_length_convert <- "~/EDAB_Datasets/Workflows/df_lconv.rda"

## run workflow ------------------

source(here::here("data-raw/workflow_productivity_anomaly.R"))

test_productivity_anomaly <- workflow_productivity_anomaly(
  input_survey_bio_epu = input_survey_bio_epu,
  input_survey_bio = input_survey_bio,
  input_static_lw_table = input_lw_table,
  inputPathSpecies = inputPathSpecies,
  input_static_length_convert = input_static_length_convert,
  outputPath = outputPath
)

