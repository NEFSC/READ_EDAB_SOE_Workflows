# Example of how to test on the Rstudio container 

# suite of paths to input and output files
outputPath <- here::here()
inputPathGB <- here::here("daily_bottomT_GB_1959_2024_detrended.csv")
inputPathGOM <- here::here("daily_bottomT_GOM_1959_2024_detrended.csv")
inputPathMAB <- here::here("daily_bottomT_MAB_1959_2024_detrended.csv")

# source workflow functions from data-raw since they are not accessible from the package installation
source(here::here("data-raw/workflow_heatwave_bottom.R"))
source(here::here("data-raw/workflow_heatwave_year_bottom.R"))

# calculate the transition dates indicator
indicator_heatwave_bottom <- workflow_heatwave_bottom(inputPathGB = inputPathGB,
                                                      inputPathGOM = inputPathGOM,
                                                      inputPathMAB = inputPathMAB,
                                                      outputPath = outputPath)

indicator_heatwave_year_bottom <- workflow_heatwave_year_bottom(inputPathGB = inputPathGB,
                                                           inputPathGOM = inputPathGOM,
                                                           inputPathMAB = inputPathMAB,
                                                           outputPath = outputPath)