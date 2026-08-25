#Command Line Local
#Rscript //nefscdata/EDAB_Workflows/GLORYS_automation_test.R "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST" "//nefscdata/EDAB_Dev/atyrell" "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

# Command Line Cloud
#Rscript ~/EDAB_Workflows/GLORYS_automation_test.R "~/EDAB_Datasets/OISST/V2/SOURCE/SST" "~/EDAB_Dev/atyrell" "~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  ltm_file = args[3]
  print('Using command line arguments')
} else {
  input_folder = '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST'
  output_folder = '//nefscdata/EDAB_Dev/atyrell'
  ltm_file = '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc'

  input_folder = '~/EDAB_Datasets/OISST/V2/SOURCE/SST'
  output_folder = '~/EDAB_Dev/atyrell'
  # ltm_file = '~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc'
  ltm_file = '~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/oisst_clim_test.nc'

  message('Using default arguments')
}

# file checks
message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))

check.dir = function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

check.dir(output_folder)
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}

if (!file.exists(ltm_file)) {
  stop(paste0('Long-term mean file does not exist: ', ltm_file))
}

# copied from workflow script
tryCatch(
  {
    if (
      !all(
        !is.null(output_path_indicators),
        file.exists(input_path_survey),
        file.exists(input_path_species)
      )
    ) {
      stop("Incorrect file path or file missing")
    }

    # calculate indicator
    indicatorData <- SOEworkflows::create_aggregate_biomass(
      input_path_survey = input_path_survey,
      input_path_species = input_path_species
    )

    # Write data to file
    saveRDS(
      indicatorData$aggregate_biomass,
      paste0(output_path_indicators, "/aggregate_biomass.rds")
    )
    saveRDS(
      indicatorData$aggregate_biomass_species,
      paste0(output_path_indicators, "/aggregate_biomass_species.rds")
    )
    return(indicatorData)
  },
  error = function(e) {
    message("An error occurred: ", conditionMessage(e))
    return(NULL)
  }
)
