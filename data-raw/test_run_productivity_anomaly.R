# To review pull request for productivity_anomaly

# run workflow_productivity anomaly from the container --------
## set paths for data inputs generated in workflow ------------
# input_survey_bio_epu <- "~/EDAB_Datasets/Workflows/surveyBiologicalByEPUData.rds"
input_survey_bio_epu <- "~/EDAB_Dev/grezlik/trawlr_files/Survdat_bio_EPU.rds"
# input_survey_bio <- "~/EDAB_Datasets/Workflows/surveyBiologicalData.rds"
input_survey_bio <- "~/EDAB_Dev/grezlik/trawlr_files/Survdat.bio.rds"
input_lw_table <- "~/EDAB_Datasets/Workflows/df_lw.rda"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
outputPath <- "~/EDAB_Indicators/"
input_length_convert <- "~/EDAB_Datasets/Workflows/df_lconv.rda"


## run workflow ------------------

source(here::here("data-raw/workflow_productivity_anomaly.R"))

test_productivity_anomaly <- workflow_productivity_anomaly(
  input_survey_bio_epu = input_survey_bio_epu,
  input_survey_bio = input_survey_bio,
  input_lw_table = input_lw_table,
  inputPathSpecies = inputPathSpecies,
  outputPath = outputPath
)

## compare workflow outputs to ecodata -------------

# new <- readRDS("~/EDAB_Indicators/productivity_anomaly.rds") |> 
#             dplyr::mutate(source = 'workflow')

new <- productivity_anomaly |> dplyr::mutate(source = 'workflow')

new <- new |> dplyr::filter(EPU != "SS")

library(dplyr)

# was getting unexpected NAs. Looked into it with commented out chunk below and debugged

# new_survey <- new  |> 
#   filter(grepl("Survey", Var, ignore.case = TRUE))
# 
# range_summary <- new_survey  |> 
#   group_by(Var) |> 
#   summarise(
#     min_value = min(Value, na.rm = TRUE),
#     max_value = max(Value, na.rm = TRUE),
#     mean_value = mean(Value, na.rm = TRUE),
#     median_value = median(Value, na.rm = TRUE),
#     n = n()
#   ) |> 
#   arrange(Var)
# 
# print(range_summary)

# Get trawlr version of 


trawlr <- readRDS("~/EDAB_Dev/grezlik/trawlr_files/trawlr_productivity_anomaly.rds") |> 
              dplyr::mutate(source = 'trawlr')


old <- ecodata::productivity_anomaly |> 
            dplyr::mutate(source = 'ecodata')

compare <- dplyr::bind_rows(old, trawlr)

library(ggplot2)
library(dplyr)
library(purrr)

# loop through Vars and plot

# Create a directory to save the plots
out_dir <- "~/EDAB_Dev/grezlik/plots_productivity_ecodata_trawlr"
dir.create(out_dir, showWarnings = FALSE)

# Loop through each unique Var
unique(compare$Var) |> walk(function(v) {
  
  dat_v <- compare |> filter(Var == v)
  
  p <- ggplot(dat_v, aes(x = Time, y = Value, color = source)) +
    geom_line(alpha = 0.7) +
    labs(title = paste("Productivity Anomaly Comparison:", v),
         x = "Year", y = "Value",
         color = "Source") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)  # show in RStudio
  
  # Optionally save each plot as a PNG
  ggsave(filename = paste0(out_dir, "/", gsub("[^A-Za-z0-9_]", "_", v), ".png"),
         plot = p, width = 8, height = 5)
})




# direct difference plot

diff_df <- compare |>
  tidyr::pivot_wider(names_from = source, values_from = Value) |>
  mutate(diff = workflow - ecodata)

ggplot(diff_df, aes(x = Time, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_line(color = "firebrick") +
  facet_wrap(~ Var, scales = "free_y") +
  labs(title = "Difference (Workflow - Ecodata) in Productivity Anomaly",
       x = "Year", y = "Difference") +
  theme_minimal()


# plotting subset for clarity
compare |> 
  filter(grepl("COD|HADDOCK|FLOUNDER", Var)) |> 
  ggplot(aes(x = Time, y = Value, color = source)) +
  geom_line() +
  facet_wrap(~ Var, scales = "free_y")


# still not matching up exactly ----------------------

# testing intermediates

# dat_spec_rec_forSOE

new_intermediate <- dat_spec_rec_forSOE |> 
                      dplyr::mutate(source = 'workflow')

tmp_env <- new.env()
load("~/EDAB_Dev/grezlik/trawlr_files/dat_spec_rec_forSOE.Rdata", envir = tmp_env)

old_intermediate <- tmp_env$dat_spec_rec_forSOE |>
  mutate(source = "trawlr")

intermediate_compare <- dplyr::bind_rows(new_intermediate,old_intermediate)

# compare sources
columns_to_compare <- setdiff(names(new_intermediate), "source")

compare_wide <- intermediate_compare |>
  tidyr::pivot_wider(
    names_from = source,
    values_from = all_of(columns_to_compare),
    names_sep = "_"
  )

compare_diff <- compare_wide |>
  dplyr::mutate(across(
    tidyselect::ends_with("_workflow"),
    ~ . - get(sub("_workflow$", "_trawlr", cur_column())),
    .names = "{sub('_workflow$', '', .col)}_diff"
  ))

# range of values already differs so difference is further upstream

old_survdat_bio_epu <- readRDS("~/EDAB_Dev/grezlik/trawlr_files/Survdat_bio_EPU.rds") |> 
                          dplyr::mutate(source = 'trawlr')

new_survdat_bio_epu <- readRDS(input_survey_bio_epu) |> 
                          dplyr::mutate(source = 'workflow')

summary(old_survdat_bio_epu)
summary(new_survdat_bio_epu)

# inputs are different. going to run workflow with old inputs






# 11.21.2025 I think the difference comes with the handling of StockSMART data
# comparing dat_spec files to make sure the workflow reproduces trawlr at least

workflow_dsr <- readRDS("~/EDAB_Dev/grezlik/trawlr_files/workflow_dat_spec_rec_forSOE_old_survey_2023.rds") |> 
                    dplyr::mutate(source = 'workflow')
workflow_dsr_epu <- readRDS("~/EDAB_Dev/grezlik/trawlr_files/workflow_dat_spec_rec_epu_forSOE_old_survey_2023.rds") |> 
                    dplyr::mutate(source = 'workflow')
load("~/EDAB_Dev/grezlik/trawlr_files/dat_spec_rec_epu_forSOE.Rdata")
dat_spec_rec_epu_forSOE <- dat_spec_rec_epu_forSOE |> dplyr::mutate(source = 'trawlr')
load("~/EDAB_Dev/grezlik/trawlr_files/dat_spec_rec_forSOE.Rdata")
dat_spec_rec_forSOE <- dat_spec_rec_forSOE |> dplyr::mutate(source = 'trawlr')

compare <- bind_rows(workflow_dsr,dat_spec_rec_forSOE)

# Create a directory to save the plots
out_dir <- "~/EDAB_Dev/grezlik/plots_dsr_trawlr_workflow_old_inputs_endyear_2023"
dir.create(out_dir, showWarnings = FALSE)

# Loop through each unique Var
unique(compare$Var) |> walk(function(v) {
  
  dat_v <- compare |> filter(Var == v)
  
  p <- ggplot(dat_v, aes(x = Time, y = Value, color = source)) +
    geom_line(alpha = 0.7) +
    labs(title = paste("Productivity Anomaly Input Comparison:", v),
         x = "Year", y = "Value",
         color = "Source") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)  # show in RStudio
  
  # Optionally save each plot as a PNG
  ggsave(filename = paste0(out_dir, "/", gsub("[^A-Za-z0-9_]", "_", v), ".png"),
         plot = p, width = 8, height = 5)
})
