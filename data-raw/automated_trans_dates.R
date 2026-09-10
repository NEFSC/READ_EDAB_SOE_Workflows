#Command Line Local
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i89-format-exp_n/data-raw/automated_trans_dates.R "//nefscdata/EDAB_Dev/atyrell" "//nefscdata/EDAB_Dev/atyrell"

# Command Line Cloud
#Rscript https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/feature/i89-format-exp_n/data-raw/automated_trans_dates.R "~/EDAB_Dev/atyrell" "~/EDAB_Dev/atyrell"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  print('Using command line arguments')
} else {
  input_folder = '//nefscdata/EDAB_Dev/atyrell'
  output_folder = '//nefscdata/EDAB_Dev/atyrell'

  input_folder = '~/EDAB_Dev/atyrell'
  output_folder = '~/EDAB_Dev/atyrell'

  message('Using default arguments')
}

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

## (1) find OISST file to use
message("Looking for processed OISST anomaly file...")
files <- list.files(
  input_folder,
  pattern = "oisst_anomaly_*\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(paste0(
    "No OISST anomaly files found in input folder: ",
    input_folder,
    "\n OISST anomaly file must be named 'oisst_anomaly_YYYY-MM-DD.csv' where YYYY-MM-DD is the date of the file."
  ))
}

input_file <- sort(files)[length(files)] # Get the most recent file

## (2) run transition dates
message("Calculating transition dates...")
indicatorData <- SOEworkflows::create_trans_dates(
  input_path_sst = input_file
)

# (3) write data to file
message("Writing transition dates to file...")
fname <- paste0(output_folder, "/trans_dates.rds")
saveRDS(indicatorData, fname)

message("Data saved at: ", fname)
message("Done: Transition Dates")
