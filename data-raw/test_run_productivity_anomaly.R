# To review pull request for productivity_anomaly

# run workflow_productivity anomaly from the container --------
## set paths for data inputs generated in workflow ------------
input_survey_bio_epu <- "~/EDAB_Datasets/Workflows/surveyBiologicalByEPUData.rds"
input_survey_bio <- "~/EDAB_Datasets/Workflows/surveyBiologicalData.rds"
input_lw_table <- "~/EDAB_Datasets/Workflows/df_lw.rda"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
outputPath <- "~/EDAB_Indicators/"
input_length_convert <- "~/EDAB_Datasets/Workflows/df_lconv.rda"
input_ratio_estimators <- "~/EDAB_Datasets/Workflows/ratio_estimators.rda"


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


old <- ecodata::productivity_anomaly |> 
            dplyr::mutate(source = 'ecodata')

compare <- dplyr::bind_rows(new,old)

library(ggplot2)
library(dplyr)
library(purrr)

# loop through Vars and plot

# Create a directory to save the plots
out_dir <- "~/EDAB_Dev/grezlik/plots_productivity_comparison"
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




