# To review pull request for productivity_anomaly

# run workflow_productivity anomaly from the container --------
## set paths for data inputs generated in workflow ------------
input_survey_bio_epu <- "~/EDAB_Datasets/Workflows/surveyBiologicalByEPUData.rds"
input_survey_bio <- "~/EDAB_Datasets/Workflows/surveyBiologicalData.rds"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"

# Add your desired output path here or keep it as Max's folder in EDAB_Dev
outputPath <- "~/EDAB_Dev/grezlik"


## run workflow ------------------
test_productivity_anomaly <- workflow_productivity_anomaly(
  input_survey_bio_epu = input_survey_bio_epu,
  inputPathSpecies = inputPathSpecies
)
