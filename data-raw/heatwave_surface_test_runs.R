# Example of how to test on the Rstudio container 

# suite of paths to input and output files
outputPath <- here::here()
inputPathGB <- here::here("GB_SST_1982_to_2024_detrended.csv")
inputPathGOM <- here::here("GOM_SST_1982_to_2024_detrended.csv")
inputPathMAB <- here::here("MAB_SST_1982_to_2024_detrended.csv")

# source workflow functions from data-raw since they are not accessible from the package installation
source(here::here("data-raw/workflow_heatwave_surface.R"))
source(here::here("data-raw/workflow_heatwave_year_surface.R"))

# calculate the transition dates indicator
indicator_heatwave_surface <- workflow_heatwave_surface(inputPathGB = inputPathGB,
                                                      inputPathGOM = inputPathGOM,
                                                      inputPathMAB = inputPathMAB,
                                                      outputPath = outputPath)

indicator_heatwave_year_surface <- workflow_heatwave_year_surface(inputPathGB = inputPathGB,
                                                           inputPathGOM = inputPathGOM,
                                                           inputPathMAB = inputPathMAB,
                                                           outputPath = outputPath)