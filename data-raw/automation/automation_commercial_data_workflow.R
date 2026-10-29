#!/usr/bin/env Rscript

# Headless Script: Pull Commercial Data, then build comdat and bennet
#
# Usage (run from the repo root so the project .Renviron is read):
#   Rscript data-raw/automation/automation_commercial_data_workflow.R Input_Folder Output_Folder Supplement_Folder
#   Input_Folder      : where the raw commercial pull is saved (e.g. EDAB_Datasets)
#   Output_Folder     : where indicators are saved (e.g. EDAB_Indicators)
#   Supplement_Folder : holds SOE_species_list_24.rds
#
# If no arguments are given, folders fall back to environment variables
# (SOE_INPUT_PATH, SOE_OUTPUT_PATH, SOE_SUPPLEMENT_PATH).
# Database credentials always come from DB_SERVER, DB_USER, DB_PASS.
#
# Menhaden landings are read from SOE_MENHADEN_FILE
# (e.g. //nefscdata/EDAB_Datasets/menhaden/menhaden.rds). That file is long format
# (year, EPU, metric_tons, dollars, species). It is reshaped here to the wide layout
# create_comdat() expects (year, NEUScatch, MABcatch, GOMcatch, units), trimmed to the
# years in the commercial pull, and passed to create_comdat() as a temporary file.
#
# Menhaden revenue: create_comdat() sets menhaden value to 0, so after it runs this
# script adds the menhaden `dollars` (by year and EPU) to the revenue series menhaden
# belongs to ("Revenue", "<guild> Revenue", "<guild> <council> managed species - Revenue",
# with guild/council looked up for NESPP3 221 in the species list) and to the menhaden
# rows of comdat_species. Value is only added in year x EPU cells where the commercial
# pull has revenue (comland value data begin in 1982), so earlier revenue stays 0
# rather than becoming menhaden-only. Menhaden dollars are used as provided in the file; they are
# NOT adjusted to the commercial pull's reference-year dollars.
#
# Order: (0) input checks -> (1) pull -> (2) comdat, (3) bennet.
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

menhaden_file <- Sys.getenv("SOE_MENHADEN_FILE")
if (menhaden_file == "") fail("SOE_MENHADEN_FILE not set (path to menhaden.rds).")

log_msg("input_folder: ", input_folder)
log_msg("output_folder: ", output_folder)
log_msg("supplemental_folder: ", supplemental_folder)
log_msg("menhaden_file: ", menhaden_file)

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

# validate the long-format menhaden file
check_menhaden <- function(mh) {
  req <- c("year", "EPU", "metric_tons", "dollars")
  miss <- setdiff(req, names(mh))
  if (length(miss) > 0) stop("Menhaden file is missing columns: ", paste(miss, collapse = ", "))
  if (!is.numeric(mh$year) || !is.numeric(mh$metric_tons) || !is.numeric(mh$dollars)) {
    stop("Menhaden 'year', 'metric_tons' and 'dollars' must be numeric")
  }
  if (any(is.na(mh$dollars))) stop("Menhaden file has missing dollars values")
  if (any(mh$dollars < 0)) stop("Menhaden file has negative dollars values")
  if (!all(c("MAB", "GOM") %in% mh$EPU)) stop("Menhaden file must contain EPU values 'MAB' and 'GOM'")
  extra_epu <- setdiff(unique(mh$EPU), c("MAB", "GOM"))
  if (length(extra_epu) > 0) stop("Unexpected menhaden EPU values: ", paste(extra_epu, collapse = ", "))
  if ("species" %in% names(mh) && length(unique(mh$species)) > 1) {
    stop("Menhaden file contains more than one species: ", paste(unique(mh$species), collapse = ", "))
  }
  if (anyDuplicated(mh[, c("year", "EPU")]) > 0) stop("Menhaden file has duplicate year x EPU rows")
  if (any(is.na(mh$metric_tons))) stop("Menhaden file has missing metric_tons values")
  if (any(mh$metric_tons < 0)) stop("Menhaden file has negative metric_tons values")
  invisible(TRUE)
}

# reshape long menhaden data to the wide layout create_comdat() expects
tidy_menhaden <- function(mh, first_year) {
  mh |>
    dplyr::as_tibble() |>
    dplyr::filter(year >= first_year) |>
    dplyr::mutate(year = as.numeric(year), col = paste0(EPU, "catch")) |>
    dplyr::select(year, col, metric_tons) |>
    tidyr::pivot_wider(names_from = col, values_from = metric_tons, values_fill = 0) |>
    dplyr::mutate(NEUScatch = MABcatch + GOMcatch, units = "metric tons") |>
    dplyr::select(year, NEUScatch, MABcatch, GOMcatch, units) |>
    dplyr::arrange(year) |>
    as.data.frame()
}

# add menhaden dollars to the comdat revenue series and comdat_species menhaden rows
# (create_comdat() sets menhaden SPPVALUE to 0)
add_menhaden_value <- function(comdat_list, mh, species_path, years) {
  # guild and council for menhaden, looked up the same way create_comdat() does
  sp221 <- readRDS(species_path) |>
    dplyr::as_tibble() |>
    dplyr::distinct(NESPP3, .keep_all = TRUE) |>
    dplyr::mutate(NESPP3 = as.numeric(NESPP3)) |>
    dplyr::filter(NESPP3 == 221)
  guild <- if (nrow(sp221) == 0 || is.na(sp221$SOE.24[1])) "Other" else sp221$SOE.24[1]
  fed   <- if (nrow(sp221) == 0 || is.na(sp221$Fed.Managed[1])) "Other" else sp221$Fed.Managed[1]
  
  # years/EPUs where commercial revenue exists (comland value is 0 before value data begin)
  has_rev <- comdat_list$comdat |>
    dplyr::filter(Var == "Revenue", Value > 0) |>
    dplyr::transmute(Time = as.numeric(Time), EPU)
  
  val <- mh |>
    dplyr::as_tibble() |>
    dplyr::filter(year >= years[1], year <= years[2]) |>
    dplyr::group_by(Time = as.numeric(year), EPU) |>
    dplyr::summarise(add = sum(dollars, na.rm = TRUE), .groups = "drop") |>
    dplyr::semi_join(has_rev, by = c("Time", "EPU"))
  if (nrow(val) == 0) stop("No years with commercial revenue to add menhaden value to")
  
  rev_vars <- c("Revenue", paste(guild, "Revenue"), paste(guild, fed, "managed species - Revenue"))
  adds <- tidyr::crossing(val, Var = rev_vars) |>
    dplyr::mutate(Units = "US dollars")
  
  cd <- comdat_list$comdat
  if (is.integer(cd$Time)) adds$Time <- as.integer(adds$Time)
  
  n_match <- nrow(dplyr::semi_join(adds, cd, by = c("Time", "Var", "EPU", "Units")))
  out <- cd |>
    dplyr::full_join(adds, by = c("Time", "Var", "EPU", "Units")) |>
    dplyr::mutate(Value = dplyr::coalesce(Value, 0) + dplyr::coalesce(add, 0)) |>
    dplyr::select(Time, Var, Value, EPU, Units) |>
    dplyr::arrange(Var, Time)
  if (anyDuplicated(out[, c("Time", "Var", "EPU", "Units")]) > 0) {
    stop("Adding menhaden value created duplicate comdat rows")
  }
  
  # comdat_species: replace the zero SPPVALUE on menhaden rows
  spc <- comdat_list$comdat_species
  mv <- val |> dplyr::transmute(YEAR = Time, EPU, NESPP3 = 221, mh_value = add)
  if (is.integer(spc$YEAR)) mv$YEAR <- as.integer(mv$YEAR)
  spc <- spc |>
    dplyr::left_join(mv, by = c("YEAR", "EPU", "NESPP3")) |>
    dplyr::mutate(SPPVALUE = dplyr::if_else(is.na(mh_value), SPPVALUE, mh_value)) |>
    dplyr::select(-mh_value)
  
  log_msg("Menhaden value added: $", format(round(sum(val$add)), big.mark = ","),
          " across ", nrow(val), " year x EPU cells to: ", paste(rev_vars, collapse = "; "),
          " (", n_match, " of ", nrow(adds), " target rows already existed)")
  
  list(comdat = out, comdat_species = spc)
}

# ---- (0) input checks: before the long pull ----------------------------------
input_path_species <- file.path(supplemental_folder, "SOE_species_list_24.rds")
if (!file.exists(input_path_species)) fail(paste0("Supplemental file missing: ", input_path_species))
if (!file.exists(menhaden_file)) fail(paste0("Menhaden file missing: ", menhaden_file))

menhaden_raw <- tryCatch(
  {
    mh <- readRDS(menhaden_file)
    check_menhaden(mh)
    log_msg("Menhaden file OK: ", nrow(mh), " rows, years ",
            min(mh$year), "-", max(mh$year))
    mh
  },
  error = function(e) fail(paste0("Menhaden file failed checks: ", conditionMessage(e)))
)

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
    # tidy menhaden to the comland years and hand it to create_comdat() as a temp file
    menhaden_wide <- tidy_menhaden(menhaden_raw, first_year = yrs[1])
    if (max(menhaden_wide$year) < yrs[2]) {
      log_msg("WARNING: menhaden data end in ", max(menhaden_wide$year),
              " but the commercial pull ends in ", yrs[2],
              "; comdat will have no menhaden landings after ", max(menhaden_wide$year))
    }
    input_path_menhaden <- tempfile("menhaden_tidy_", fileext = ".rds")
    saveRDS(menhaden_wide, input_path_menhaden)
    on.exit(unlink(input_path_menhaden), add = TRUE)
    log_msg("Menhaden tidied: years ", min(menhaden_wide$year), "-", max(menhaden_wide$year))
    
    log_msg("Calculating comdat...")
    comdat <- SOEworkflows::create_comdat(
      input_path_comdat = comdat_file,
      input_path_species = input_path_species,
      input_path_menhaden = input_path_menhaden
    )
    unlink(input_path_menhaden)
    
    # add menhaden revenue (create_comdat() sets it to 0)
    comdat <- add_menhaden_value(comdat, menhaden_raw, input_path_species, years = yrs)
    
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