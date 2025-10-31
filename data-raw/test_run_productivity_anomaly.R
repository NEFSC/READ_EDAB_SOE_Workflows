# To review pull request for productivity_anomaly

# run workflow_productivity anomaly from the container --------
## set paths for data inputs generated in workflow ------------
input_survey_bio_epu <- "~/EDAB_Datasets/Workflows/surveyBiologicalByEPUData.rds"
input_survey_bio <- "~/EDAB_Datasets/Workflows/surveyBiologicalData.rds"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
outputPath <- "~/EDAB_Indicators/productivity_anomaly.rds"


## run workflow ------------------

source(here::here("data-raw/workflow_productivity_anomaly.R"))

test_productivity_anomaly <- workflow_productivity_anomaly(
  input_survey_bio_epu = input_survey_bio_epu,
  input_survey_bio = input_survey_bio,
  inputPathSpecies = inputPathSpecies
)
