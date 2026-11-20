# Example of how to test on the Rstudio container

# suite of paths to input and output files
output_path_indicators <- "/home/bbeltz/EDAB_Dev/beltz/"
inputPathGB <- "/home/bbeltz/EDAB_Dev/beltz/GB_SST_1982_to_2024_detrended.csv"
inputPathGOM <- "/home/bbeltz/EDAB_Dev/beltz/GOM_SST_1982_to_2024_detrended.csv"
inputPathMAB <- "/home/bbeltz/EDAB_Dev/beltz/MAB_SST_1982_to_2024_detrended.csv"

# source workflow functions from data-raw since they are not accessible from the package installation
source(here::here("data-raw/workflow_heatwave_surface.R"))
source(here::here("data-raw/workflow_heatwave_year_surface.R"))

# calculate the transition dates indicator
indicator_heatwave_surface <- workflow_heatwave_surface(
  inputPathGB = inputPathGB,
  inputPathGOM = inputPathGOM,
  inputPathMAB = inputPathMAB,
  output_path_indicators = output_path_indicators
)

indicator_heatwave_year_surface <- workflow_heatwave_year_surface(
  inputPathGB = inputPathGB,
  inputPathGOM = inputPathGOM,
  inputPathMAB = inputPathMAB,
  output_path_indicators = output_path_indicators
)
