#!/usr/bin/env Rscript

# Headless Script: Pull Commercial Data, then build comdat and bennet
#
# Usage (run from the repo root so the project .Renviron is read):
#   Rscript data-raw/workflow_pull_commercial_data.R Input_Folder Output_Folder Supplement_Folder
#
#   Input_Folder      : where the raw commercial pull is saved (e.g. EDAB_Datasets)
#   Output_Folder     : where indicators are saved (e.g. EDAB_Indicators)
#   Supplement_Folder : holds SOE_species_list_24.rds and menhadenEOF.rds
#
# If no arguments are given, folders fall back to environment variables
# (SOE_INPUT_PATH, SOE_OUTPUT_PATH, SOE_SUPPLEMENT_PATH).
# Database credentials always come from DB_SERVER, DB_USER, DB_PASS.
#
# Order: (1) pull -> (2) comdat, (3) bennet.
# Steps 2-3 only run if the pull succeeds. Each indicator runs independently;
# the script exits with status 1 if the pull or either indicator fails.
#
# Outputs (date = YYYY-MM-DD):
#   Input_Folder : commercial_comdat_data_<date>.rds, commercial_bennet_data_<date>.rds
#   Output_Folder: comdat_<date>.rds, comdat_species_<date>.rds, bennet_<date>.rds

library(DBI)
library(ROracle)
library(SOEworkflows)

ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_msg <- function(...) message("[", ts(), "] ", ...)

# exit non-zero in Rscript; stop() when run interactively so RStudio isn't closed
fail <- function(msg) {
  message("[", ts(), "] ERROR: ", msg)
  if (interactive()) stop(msg, call. = FALSE) else quit(save = "no", status = 1)
}

# ---- arguments ---------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3) {
  input_folder <- args[1]
  output_folder <- args[2]
  supplemental_folder <- args[3]
  log_msg("Using command line arguments")
} else if (length(args) == 0) {
  input_folder <- Sys.getenv("SOE_INPUT_PATH")
  output_folder <- Sys.getenv("SOE_OUTPUT_PATH")
  supplemental_folder <- Sys.getenv("SOE_SUPPLEMENT_PATH")
  log_msg("Using environment variables for folders")
} else {
  fail("Expected 3 arguments: Input_Folder Output_Folder Supplement_Folder")
}

if (any(c(input_folder, output_folder, supplemental_folder) == "")) {
  fail("Input, output, or supplement folder not set (arguments or SOE_*_PATH env vars).")
}

log_msg("input_folder: ", input_folder)
log_msg("output_folder: ", output_folder)
log_msg("supplemental_folder: ", supplemental_folder)

for (d in c(input_folder, output_folder)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
if (!dir.exists(supplemental_folder)) {
  fail(paste0("Supplement directory does not exist: ", supplemental_folder))
}

run_date <- format(Sys.Date(), "%Y-%m-%d")

# check an indicator has the standard ecodata structure
check_indicator <- function(df, name, required = c("Time", "Var", "Value", "EPU", "Units")) {
  if (!is.data.frame(df) || nrow(df) == 0) stop(name, " is empty or not a data frame")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(name, " is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  if (all(is.na(df$Value))) stop(name, " has no non-missing values")
  log_msg(
    name, ": ", nrow(df), " rows, ", length(unique(df$Var)), " variables, years ",
    min(df$Time, na.rm = TRUE), "-", max(df$Time, na.rm = TRUE)
  )
  invisible(TRUE)
}

# ---- (0) supplemental files: check before the long pull --------------------
input_path_species <- file.path(supplemental_folder, "SOE_species_list_24.rds")
input_path_menhaden <- file.path(supplemental_folder, "menhadenEOF.rds")
for (f in c(input_path_species, input_path_menhaden)) {
  if (!file.exists(f)) fail(paste0("Supplemental file missing: ", f))
}

# ---- (1) pull commercial data ------------------------------------------------
comdat_file <- file.path(input_folder, paste0("commercial_comdat_data_", run_date, ".rds"))
bennet_file <- file.path(input_folder, paste0("commercial_bennet_data_", run_date, ".rds"))

channel <- NULL
pull_ok <- tryCatch(
  {
    db_server <- Sys.getenv("DB_SERVER")
    db_user <- Sys.getenv("DB_USER")
    db_pass <- Sys.getenv("DB_PASS")
    if (any(c(db_server, db_user, db_pass) == "")) {
      stop("Missing database credentials. Set DB_SERVER, DB_USER, and DB_PASS.")
    }
    
    log_msg("Connecting to database: ", db_server, " as user: ", db_user)
    channel <- ROracle::dbConnect(
      ROracle::Oracle(),
      dbname = db_server,
      username = db_user,
      password = db_pass
    )
    log_msg("Connected. Pulling commercial data...")
    
    commercial_data <- SOEworkflows::get_commercial_data(channel)
    
    # integrity checks on the pull
    for (nm in c("comdat", "bennet")) {
      cl <- commercial_data[[nm]]$comland
      if (is.null(cl) || nrow(cl) == 0) stop("Pull returned no rows for '", nm, "'")
    }
    yrs <- range(commercial_data$comdat$comland$YEAR, na.rm = TRUE)
    log_msg("Pull returned comland years ", yrs[1], "-", yrs[2])
    
    saveRDS(commercial_data$comdat, comdat_file)
    log_msg("Saved: ", comdat_file)
    saveRDS(commercial_data$bennet, bennet_file)
    log_msg("Saved: ", bennet_file)
    TRUE
  },
  error = function(e) {
    message("[", ts(), "] ERROR in commercial data pull: ", conditionMessage(e))
    FALSE
  },
  finally = {
    if (!is.null(channel)) try(DBI::dbDisconnect(channel), silent = TRUE)
  }
)

if (!pull_ok) fail("Commercial data pull failed; comdat and bennet were not run.")

# ---- (2) comdat --------------------------------------------------------------
comdat_ok <- tryCatch(
  {
    log_msg("Calculating comdat...")
    comdat <- SOEworkflows::create_comdat(
      input_path_comdat = comdat_file,
      input_path_species = input_path_species,
      input_path_menhaden = input_path_menhaden
    )
    check_indicator(comdat$comdat, "comdat")
    if (NROW(comdat$comdat_species) == 0) stop("comdat_species is empty")
    
    f1 <- file.path(output_folder, paste0("comdat_", run_date, ".rds"))
    f2 <- file.path(output_folder, paste0("comdat_species_", run_date, ".rds"))
    saveRDS(comdat$comdat, f1)
    saveRDS(comdat$comdat_species, f2)
    log_msg("Saved: ", f1)
    log_msg("Saved: ", f2)
    TRUE
  },
  error = function(e) {
    message("[", ts(), "] ERROR in comdat: ", conditionMessage(e))
    FALSE
  }
)

# ---- (3) bennet --------------------------------------------------------------
bennet_ok <- tryCatch(
  {
    log_msg("Calculating bennet...")
    bennet <- SOEworkflows::create_bennet(
      input_path_bennet = bennet_file,
      input_path_species = input_path_species
    )
    check_indicator(bennet, "bennet")
    
    f3 <- file.path(output_folder, paste0("bennet_", run_date, ".rds"))
    saveRDS(bennet, f3)
    log_msg("Saved: ", f3)
    TRUE
  },
  error = function(e) {
    message("[", ts(), "] ERROR in bennet: ", conditionMessage(e))
    FALSE
  }
)

# ---- summary -----------------------------------------------------------------
log_msg("Summary: pull = OK, comdat = ", if (comdat_ok) "OK" else "FAILED",
        ", bennet = ", if (bennet_ok) "OK" else "FAILED")

if (!(comdat_ok && bennet_ok)) fail("One or more indicators failed.")
log_msg("Done: commercial pull, comdat, bennet")