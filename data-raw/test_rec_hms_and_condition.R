devtools::load_all()

## step 1: pull rec HMS data
source(here::here("data-raw/workflow_pull_rec_hms.R"))
workflow_pull_rec_hms(outputDir = "data-raw")

## step 2: calculate HMS time series
source(here::here("data-raw/workflow_rec_hms.R"))
workflow_rec_hms(
  # the input file is produced in the previous step
  # change filename if needed
  inputPath = here::here("data-raw/hms_mrip_2025-09-26.csv"),
  input_path_rec_key = "~/EDAB_Resources/workflow_resources/soe_workflows/hms_key.csv",
  output_path_indicators = here::here("data-raw")
)

## step 3: calculate species condition
source(here::here("data-raw/workflow_species_condition.R"))
workflow_condition(
  inputPath = "~/EDAB_Dev/beet/condition.rds",
  input_path_lw_coeffs = "~/EDAB_Resources/workflow_resources/soe_workflows/LWparams.csv",
  input_path_species = "~/EDAB_Resources/workflow_resources/soe_workflows/species.codes.csv",
  output_path_indicators = here::here("data-raw")
)
