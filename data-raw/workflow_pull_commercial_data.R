#!/usr/bin/env Rscript

# Headless Script: Pull Commercial Data
# Purpose: Automatically pull raw commercial data from Oracle DB and save output RDS files.

library(DBI)
library(ROracle)
library(here)
library(SOEworkflows)

message(paste0("[", Sys.time(), "] Starting automated commercial data pull..."))

# Define output directory (default to data-raw if not specified via ENV)
output_path <- Sys.getenv("SOE_OUTPUT_PATH", unset = here::here("data-raw"))

if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

tryCatch(
  {
    # Fetch secrets from environment variables
    db_server <- Sys.getenv("DB_SERVER")
    db_user   <- Sys.getenv("DB_USER")
    db_pass   <- Sys.getenv("DB_PASS")
    
    # Guard clause: Fail fast if required credentials are not populated
    if (nchar(db_server) == 0 || nchar(db_user) == 0 || nchar(db_pass) == 0) {
      stop("Missing required database credentials! Ensure DB_SERVER, DB_USER, and DB_PASS environment variables are set.")
    }
    
    message(paste0("Connecting non-interactively to database: ", db_server, " as user: ", db_user))
    
    # Connect directly via ROracle driver without interactive getPass popup
    driver  <- ROracle::Oracle()
    channel <- ROracle::dbConnect(
      driver,
      dbname   = db_server,
      username = db_user,
      password = db_pass
    )
    
    # Ensure database connection is closed on script exit
    on.exit(
      if (exists("channel") && isS4(channel)) DBI::dbDisconnect(channel),
      add = TRUE
    )
    
    message("Successfully connected to database.")
    
    # Pull commercial data using existing package function
    message("Fetching commercial data via SOEworkflows...")
    commercial_data <- SOEworkflows::get_commercial_data(channel)
    
    # Define output file paths
    comdat_file <- file.path(output_path, "commercial_comdat_data.rds")
    bennet_file <- file.path(output_path, "commercial_bennet_data.rds")
    
    # Save output RDS files
    saveRDS(commercial_data$comdat, comdat_file)
    message(paste0("Saved: ", comdat_file))
    
    saveRDS(commercial_data$bennet, bennet_file)
    message(paste0("Saved: ", bennet_file))
    
    message(paste0("[", Sys.time(), "] Commercial data pull completed successfully."))
  },
  error = function(e) {
    message(paste0("[", Sys.time(), "] ERROR in workflow_pull_commercial_data: "), conditionMessage(e))
    # Exit with code 1 so GitHub Actions / runner flags job failure
    quit(status = 1, save = "no")
  }
)