devtools::load_all()

source(here::here("data-raw/workflow_pull_rec_hms.R"))
workflow_pull_rec_hms(outputDir = "data-raw")

source(here::here("data-raw/workflow_rec_hms.R"))
workflow_rec_hms(
  inputPath = here::here("data-raw/hms_mrip.csv"),
  inputKey = here::here("data-raw/hms_key.csv"),
  outputPath = here::here("data-raw")
)

source(here::here("data-raw/workflow_species_condition.R"))
workflow_condition(
  inputPath = here::here("data-raw/survdat_subset.csv"),
  inputpathLW = here::here("data-raw/LWparams.csv"),
  inputpathSpecies = here::here("data-raw/species.codes.csv"),
  outputPath = here::here("data-raw")
)
