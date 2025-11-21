#' Calculate Productivity Anomalies from Survey Data
#'
#' @description
#' Processes survey and species data to calculate recruitment-spawner ratios (r/s) 
#' and their anomalies (z-scores) across species and EPUs. Outputs data formatted 
#' for State of the Ecosystem (SOE) reporting.
#'
#' @param input_survey_bio Character string. Full path to the survey data with bio rds file
#' @param input_survey_bio_epu File path to survey data with bio data and epu (.rds format)
#' @param input_lw_table File path to length weight table from Miller 2013 (.rda format)
#' @param inputPathSpecies File path to species lookup table (.rds format)
#' @param species2include Character vector of species to include (default: commonly surveyed species)
#'
#' @return a combined productivity anomaly data set
#'
#' @examples
#' \dontrun{
#' create_productivity_anomaly(
#'   input_survey_bio = "survey_bio.rds"
#'   input_survey_bio_epu = "survey_bio_epu.rds",
#'   input_lw_table = "lw_table.rda",
#'   inputPathSpecies = "species_lookup.rds"
#' )
#' }
#'
#' @export

create_productivity_anomaly <- function(
    input_survey_bio,
    input_survey_bio_epu,
    input_lw_table,
    inputPathSpecies,
    species2include = c(
      "ACADIAN REDFISH", "AMERICAN PLAICE", "ATLANTIC COD", "BLACK SEA BASS",
      "BUTTERFISH", "HADDOCK", "POLLOCK", "RED HAKE", "SCUP", "SILVER HAKE",
      "SUMMER FLOUNDER", "WHITE HAKE", "WINDOWPANE", "WINTER FLOUNDER",
      "WITCH FLOUNDER", "YELLOWTAIL FLOUNDER"
    )
) {
  
  end.year <- format(Sys.Date(),"%Y")
  end.year <- as.numeric(end.year)

# species lookup
message("Filtering for focal species")
species2out <- readRDS(inputPathSpecies) |>
  dplyr::select(COMNAME, SCINAME) |>
  dplyr::mutate(
    SCINAME = trimws(as.character(SCINAME)),
    COMNAME = trimws(COMNAME)
  ) |>
  dplyr::filter(COMNAME %in% species2include) |>
  dplyr::distinct(SCINAME) |>
  dplyr::arrange(SCINAME) |>
  dplyr::pull(SCINAME)


# load survey data -----------------------
message("Loading survey data")

survdat_bio_epu <- readRDS(input_survey_bio_epu)

# Strip survdat.bio.epu to just have epu information
station_epus <-
  survdat_bio_epu  |> 
  as.data.frame() |> 
  dplyr::select(STRATUM, STATION, YEAR, SEASON, EPU)  |> 
  dplyr::mutate(EPU =
                  factor(EPU,
                         levels = c("SS", "GOM", "GB", "MAB"))
  )  |> 
  dplyr::distinct()  |> 
  dplyr::mutate(STATION  = as.character(STATION),
                SEASON = as.character(SEASON))

# Load survdat bio without epu's
#load(file.path(raw.dir,"SurvdatBio.RData"))
bio.data <- readRDS(input_survey_bio)
survdat.bio <- bio.data$survdat

# Prep survdat.bio for merge with survdat_nefsc
survdat.bio <- survdat.bio |> 
                  as.data.frame()  |> 
                  dplyr::mutate(CRUISE6 = as.character(CRUISE6),
                      STATION  = as.character(STATION),
                      EST_TOWDATE = as.Date(EST_TOWDATE),
                      SEASON = as.character(SEASON),
                      SVVESSEL = as.character(SVVESSEL),
                      CATCHSEX = SEX) |> 
                  dplyr::filter(INDWT != 0)


bio.data <- readRDS(input_survey_bio)
survdat_nefsc <- bio.data$survdat


# Prep survdat_nefsc for join:
survdat_nefsc <- survdat_nefsc  |> 
  tibble::as_tibble() |> 
  dplyr::mutate(CRUISE6 = as.character(CRUISE6),
                STATION  = as.character(STATION),
                EST_TOWDATE = as.Date(EST_TOWDATE),
                SEASON = as.character(SEASON),
                SVVESSEL = as.character(SVVESSEL),
                CATCHSEX = as.factor(CATCHSEX))


# Combine surveys:
survdat_nefsc <- survdat_nefsc  |> 
  as.data.frame() |> 
  dplyr::left_join(station_epus)  |> 
  dplyr::left_join(survdat.bio, relationship = "many-to-many")  |> 
  # Add SEX = 0 to all nefsc observations where sex was
  # not recorded.
  dplyr::mutate(SEX = ifelse(is.na(SEX),
                             0,
                             SEX))


# species <- readRDS(inputPathSpecies) |> 
#               dplyr::filter(!is.na(SVSPP)) |> 
#               dplyr::mutate(COMNAME = as.factor(COMNAME))

load("~/EDAB_Dev/grezlik/trawlr_files/svspp_table.rda")
load("~/EDAB_Dev/grezlik/trawlr_files/tax_table.rda")

# Combine surveys:
survdat <-
  survdat_nefsc  |> 
  dplyr::mutate(month  = EST_TOWDATE  |> 
                  format("%m") |> 
                  as.integer(),
                day    = EST_TOWDATE |> 
                  format("%d") |> 
                  as.integer(),
                time   = EST_TOWDATE |> 
                  format("%T"),
                SEASON = factor(SEASON, # Reorder for plots
                                levels =
                                  c("WINTER", "SPRING",
                                    "SUMMER", "FALL"))) |> 
  dplyr::left_join(svspp_table, by = "SVSPP") |>
  dplyr::left_join(tax_table)

# Fix some SCINAMES
survdat <- survdat |> 
  dplyr::mutate(
    SCINAME = as.character(SCINAME),
    SCINAME = stringr::str_trim(SCINAME),  # removes spaces before and after
    SCINAME = ifelse(SCINAME == "MACROZOARCES AMERICANUS", "ZOARCES AMERICANUS", SCINAME)
  )


# Change survdat SVVESSEL to "AL" if it isn't "HB"
# (this is for length-conversion purposes)
survdat <- survdat  |> 
  dplyr::mutate(SVVESSEL = ifelse(SVVESSEL == "HB",
                                  SVVESSEL,
                                  "AL"))

# Perform length conversions where available
# Load length-conversions and combine into one df
# (conversions from Miller 2013)
load(input_length_convert)

# Add offshore hake to length conversion which is identical
# to silver hake (per Larry Alade 2016)
df_lconv <- df_lconv  |> 
  rbind({df_lconv |> 
      dplyr::filter(COMNAME == "SILVER HAKE")  |> 
      dplyr::mutate(COMNAME = "OFFSHORE HAKE")})  |> 
  dplyr::rename(sweptratio = ALoverHB_sweptratio)


survdat <- survdat |> 
  dplyr::left_join(df_lconv) |> 
  dplyr::mutate(NUMLEN = ifelse(is.na(rho),
                                NUMLEN,
                                NUMLEN*rho*sweptratio))


# Load length-weight table
load(input_lw_table)
df_lw <- df_lw |> 
  dplyr::mutate(SEX = as.character(SEX))
#data(df_lw)

survdat <- survdat  |> 
  dplyr::left_join(df_lw) |> 
  dplyr::mutate(INDWT_ALLPRED =
                  exp(SVLWCOEFF + SVLWEXP * log(LENGTH)),
                INDWT_PRED = ifelse(is.na(INDWT),
                                    INDWT_ALLPRED,
                                    INDWT))

# Remove observations that do not have NUMLEN (a few incomplete records)
survdat1 <-
  survdat  |> 
  #Keep only wanted species
  dplyr::filter(SCINAME %in% species2out)

# end of load_survdat because call in 2-load.R specified old_format=T
# https://github.com/NOAA-EDAB/trawlr/blob/392fdc6e841893bdfe2322abe6801cc695e04312/R/func-data.R#L313

# survdat1 passed to calc_rec

# Calculate the mean length at age-1 for species
# with age data
message("estimating length cut-off for age-1 fish")

df_len_at_age1_epu <- survdat1  |>
  dplyr::filter(YEAR >= (1980 - 1),
                YEAR <= end.year,
                !is.na(AGE),
                !is.na(LENGTH)) |> 
  dplyr::group_by(COMNAME, SCINAME)  |> 
  dplyr::summarise(length_at_age1 = {
    len_model <- glm(LENGTH ~ AGE, family = "poisson")
    as.numeric(predict(len_model, newdata = data.frame(AGE = 2), type = "response"))
  }, .groups = "drop")


# Calculate the number of tows in each survey season
message("calculating number of tows per cruise/year/season")

dat_tows_epu <- survdat1  |> 
  dplyr::distinct(CRUISE6, YEAR, SEASON, STATION, STRATUM)  |> 
  dplyr::group_by(CRUISE6, YEAR, SEASON)  |> 
  dplyr::summarise(n_tows = dplyr::n(), .groups = "drop")

# merge and calculate recruitment-related variables ---------------------

message("Calculating recruitment-related variables by EPU")

dat_spec_rec_epu <- survdat1 |> 
  dplyr::left_join(dat_tows_epu, by = c("CRUISE6", "YEAR", "SEASON")) |> 
  dplyr::left_join(df_len_at_age1_epu, by = c("COMNAME", "SCINAME")) |> 
  dplyr::group_by(COMNAME, SCINAME) |> 
  dplyr::filter(EPU %in% c("MAB", "GB", "GOM")) |> 
  dplyr::filter(YEAR >= (1980 - 1), YEAR <= end.year + 1) |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::group_by(EPU) |> 
  dplyr::mutate(rank_LENGTH = dplyr::cume_dist(LENGTH),
                NUMLEN      = as.numeric(NUMLEN),
                INDWT       = as.numeric(INDWT),
                WTperLENGTH = INDWT / LENGTH) |> 
  dplyr::filter(!is.na(rank_LENGTH)) |> 
  # If length at age1 exists use it, else use the length
  # cutoff
  dplyr::mutate(maturity = ifelse(is.na(length_at_age1),
                                  ifelse(rank_LENGTH > len_cutoff,
                                         "spawner",
                                         "recruit"),
                                  ifelse(LENGTH > length_at_age1,
                                         "spawner",
                                         "recruit"))
  )  |> 
  dplyr::mutate(ind_sp_abund = ifelse(maturity == "spawner",
                                      NUMLEN,
                                      0),
                ind_rec_abund = ifelse(maturity == "recruit",
                                       NUMLEN,
                                       0),
                ind_sp_wl    = ifelse(maturity == "spawner",
                                      WTperLENGTH,
                                      NaN),
                ind_rec_wl    = ifelse(maturity == "recruit",
                                       WTperLENGTH,
                                       NaN),
                ind_sp_wl_res = ifelse(maturity == "spawner",
                                       (INDWT - INDWT_ALLPRED),
                                       NaN),
                ind_rec_wl_res = ifelse(maturity == "recruit",
                                        (INDWT - INDWT_ALLPRED),
                                        NaN)) |> 
  dplyr::group_by(YEAR, SEASON, SCINAME, COMNAME, EPU) |> 
  dplyr::summarise(n_tows = unique(n_tows),
                   spawners_abund = sum(ind_sp_abund, na.rm = T)/n_tows,
                   spawners_biom = sum(ind_sp_abund *
                                         INDWT_PRED, na.rm = T)/n_tows,
                   recruits_abund = sum(ind_rec_abund, na.rm = T)/n_tows,
                   recruits_biom = sum(ind_rec_abund *
                                         INDWT_PRED, na.rm = T)/n_tows,
                   spawner_wl   = mean(ind_sp_wl, na.rm = T),
                   recruit_wl   = mean(ind_rec_wl, na.rm = T),
                   spawner_wl_res = mean(ind_sp_wl_res,
                                         na.rm = T),
                   recruit_wl_res = mean(ind_rec_wl_res,
                                         na.rm = T)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(SEASON,
                 COMNAME, SCINAME, EPU, YEAR)  |> 
  dplyr::group_by(SEASON,
                  COMNAME, SCINAME, EPU) |> 
  dplyr::mutate(recruits_biom_lead1 = dplyr::lead(recruits_biom,
                                                  n = 1),
                recruits_abund_lead1 = dplyr::lead(recruits_abund,
                                                   n = 1),
                rs         = recruits_abund_lead1/
                  spawners_biom,
                rs_abund   = recruits_abund_lead1/
                  spawners_abund,
                rs_biom    = recruits_biom_lead1/
                  spawners_biom,
                logr_abund = log(recruits_abund_lead1),
                logr_biom  = log(recruits_biom_lead1),
                logs_abund = log(spawners_abund),
                logs_biom  = log(spawners_biom),
                logrs      = log(rs),
                logrs_abund = log(rs_abund),
                logrs_biom  = log(rs_biom),
                spawners_abund_lag0_anom =
                  (spawners_abund - mean(spawners_abund, na.rm = TRUE)) / 
                  sd(spawners_abund, na.rm = TRUE)
                ,
                spawners_biom_lag0_anom =
                  (spawners_biom - mean(spawners_biom, na.rm = TRUE)) /
                  sd(spawners_biom, na.rm = TRUE),
                recruits_abund_lead1_anom =
                  (recruits_abund_lead1 - mean(recruits_abund_lead1, na.rm = TRUE)) /
                  sd(recruits_abund_lead1, na.rm = TRUE),
                recruits_biom_lead1_anom =
                  (recruits_biom_lead1 - mean(recruits_biom_lead1, na.rm = TRUE)) /
                  sd(recruits_biom_lead1, na.rm = TRUE),
                rs_anom       = 
                  (rs - mean(rs, na.rm = TRUE)) /
                  sd(rs, na.rm = TRUE),
                rs_abund_anom = 
                  (rs_abund - mean(rs_abund, na.rm = TRUE)) /
                  sd(rs_abund, na.rm = TRUE),
                rs_biom_anom = 
                  (rs_biom - mean(rs_biom, na.rm = TRUE)) /
                  sd(rs_biom, na.rm = TRUE),
                logr_abund_anom =
                  (logr_abund - mean(logr_abund, na.rm = TRUE)) /
                  sd(logr_abund, na.rm = TRUE),
                logr_biom_anom =
                  (logr_biom - mean(logr_biom, na.rm = TRUE)) /
                  sd(logr_biom, na.rm = TRUE),
                logs_abund_anom =
                  (logs_abund - mean(logs_abund, na.rm = TRUE)) /
                  sd(logs_abund, na.rm = TRUE),
                logs_biom_anom =
                  (logs_biom - mean(logs_biom, na.rm = TRUE)) /
                  sd(logs_biom, na.rm = TRUE),
                logrs_anom =
                  (logrs - mean(logrs, na.rm = TRUE)) /
                  sd(logrs, na.rm = TRUE),
                logrs_abund_anom =
                  (logrs_abund - mean(logrs_abund, na.rm = TRUE)) /
                  sd(logrs_abund, na.rm = TRUE),
                logrs_biom_anom =
                  (logrs_biom - mean(logrs_biom, na.rm = TRUE)) /
                  sd(logrs_biom, na.rm = TRUE),
                spawner_wl_anom  =
                  (spawner_wl - mean(spawner_wl, na.rm = TRUE)) /
                  sd(spawner_wl, na.rm = TRUE),
                recruit_wl_lead1 = dplyr::lead(recruit_wl, n = 1),
                recruit_wl_lead1_anom =
                  (recruit_wl_lead1 - mean(recruit_wl_lead1, na.rm = TRUE)) /
                  sd(recruit_wl_lead1, na.rm = TRUE),
                spawner_wl_res_anom =
                  (spawner_wl_res - mean(spawner_wl_res, na.rm = TRUE)) /
                  sd(spawner_wl_res, na.rm = TRUE),
                recruit_wl_res_lead1 = dplyr::lag(recruit_wl_res, n = 1),
                recruit_wl_res_lead1_anom =
                  (recruit_wl_res_lead1 - mean(recruit_wl_res_lead1, na.rm = TRUE)) /
                  sd(recruit_wl_res_lead1, na.rm = TRUE)) |> 
  # Remove year before minimum and year after maximum
  dplyr::filter(YEAR >= 1980,
                YEAR <= end.year) |> 
  dplyr::group_by(YEAR, SCINAME, COMNAME, EPU) |> 
  dplyr::summarise(
    spawners_abund_lag0 = mean(spawners_abund,
                               na.rm = T),
    spawners_abund_lag0_anom = mean(spawners_abund_lag0_anom,
                                    na.rm = T),
    spawners_biom_lag0 = mean(spawners_biom, na.rm = T),
    spawners_biom_lag0_anom = mean(spawners_biom_lag0_anom,
                                   na.rm = T),
    recruits_abund    = mean(recruits_abund, na.rm = T),
    recruits_abund_lead1      = mean(recruits_abund_lead1,
                                     na.rm = T),
    recruits_abund_lead1_anom = mean(recruits_abund_lead1_anom,
                                     na.rm = T),
    recruits_biom_lead1      = mean(recruits_biom_lead1,
                                    na.rm = T),
    recruits_biom_lead1_anom  = mean(recruits_biom_lead1_anom,
                                     na.rm = T),
    rs                 = mean(rs, na.rm = T),
    rs_abund           = mean(rs_abund, na.rm = T),
    rs_biom            = mean(rs_biom, na.rm = T),
    rs_anom            = mean(rs_anom, na.rm = T),
    rs_abund_anom      = mean(rs_abund_anom, na.rm = T),
    rs_biom_anom       = mean(rs_biom_anom, na.rm = T),
    logr_abund_anom    = mean(logr_abund_anom,
                              na.rm = T),
    logr_biom_anom     = mean(logr_biom_anom,
                              na.rm = T),
    logs_abund_anom    = mean(logs_abund_anom,
                              na.rm = T),
    logs_biom_anom     = mean(logs_biom_anom,
                              na.rm = T),
    logrs_anom         = mean(logrs_anom, na.rm = T),
    logrs_abund_anom    = mean(logrs_abund_anom,
                               na.rm = T),
    logrs_biom_anom     = mean(logrs_biom_anom,
                               na.rm = T),
    spawner_wl_anom     = mean(spawner_wl_anom,
                               na.rm = T),
    recruit_wl_lead1_anom = mean(recruit_wl_lead1_anom,
                                 na.rm = T),
    spawner_wl_res_anom = mean(spawner_wl_res_anom,
                               na.rm = T),
    recruit_wl_res_lead1_anom = mean(recruit_wl_res_lead1_anom,
                                     na.rm = T))

message("Calculating recruitment-related variables for whole shelf")

dat_spec_rec <- survdat1 |> 
  dplyr::left_join(dat_tows_epu, by = c("CRUISE6", "YEAR", "SEASON")) |> 
  dplyr::left_join(df_len_at_age1_epu, by = c("COMNAME", "SCINAME")) |> 
  dplyr::group_by(COMNAME, SCINAME) |> 
  dplyr::filter(EPU %in% c("MAB", "GB", "GOM")) |> 
  dplyr::filter(YEAR >= (1980 - 1), YEAR <= end.year + 1) |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::mutate(rank_LENGTH = dplyr::cume_dist(LENGTH),
                NUMLEN      = as.numeric(NUMLEN),
                INDWT       = as.numeric(INDWT),
                WTperLENGTH = INDWT / LENGTH) |> 
  dplyr::filter(!is.na(rank_LENGTH)) |> 
  # If length at age1 exists use it, else use the length
  # cutoff
  dplyr::mutate(maturity = ifelse(is.na(length_at_age1),
                                  ifelse(rank_LENGTH > len_cutoff,
                                         "spawner",
                                         "recruit"),
                                  ifelse(LENGTH > length_at_age1,
                                         "spawner",
                                         "recruit"))
  )  |> 
  dplyr::mutate(ind_sp_abund = ifelse(maturity == "spawner",
                                      NUMLEN,
                                      0),
                ind_rec_abund = ifelse(maturity == "recruit",
                                       NUMLEN,
                                       0),
                ind_sp_wl    = ifelse(maturity == "spawner",
                                      WTperLENGTH,
                                      NaN),
                ind_rec_wl    = ifelse(maturity == "recruit",
                                       WTperLENGTH,
                                       NaN),
                ind_sp_wl_res = ifelse(maturity == "spawner",
                                       (INDWT - INDWT_ALLPRED),
                                       NaN),
                ind_rec_wl_res = ifelse(maturity == "recruit",
                                        (INDWT - INDWT_ALLPRED),
                                        NaN)) |> 
  dplyr::group_by(YEAR, SEASON, SCINAME, COMNAME) |> 
  dplyr::summarise(n_tows = unique(n_tows),
                   spawners_abund = sum(ind_sp_abund, na.rm = T)/n_tows,
                   spawners_biom = sum(ind_sp_abund *
                                         INDWT_PRED, na.rm = T)/n_tows,
                   recruits_abund = sum(ind_rec_abund, na.rm = T)/n_tows,
                   recruits_biom = sum(ind_rec_abund *
                                         INDWT_PRED, na.rm = T)/n_tows,
                   spawner_wl   = mean(ind_sp_wl, na.rm = T),
                   recruit_wl   = mean(ind_rec_wl, na.rm = T),
                   spawner_wl_res = mean(ind_sp_wl_res,
                                         na.rm = T),
                   recruit_wl_res = mean(ind_rec_wl_res,
                                         na.rm = T)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(SEASON,
                 COMNAME, SCINAME, YEAR)  |> 
  dplyr::group_by(SEASON,
                  COMNAME, SCINAME) |> 
  dplyr::mutate(recruits_biom_lead1 = dplyr::lead(recruits_biom,
                                                  n = 1),
                recruits_abund_lead1 = dplyr::lead(recruits_abund,
                                                   n = 1),
                rs         = recruits_abund_lead1/
                  spawners_biom,
                rs_abund   = recruits_abund_lead1/
                  spawners_abund,
                rs_biom    = recruits_biom_lead1/
                  spawners_biom,
                logr_abund = log(recruits_abund_lead1),
                logr_biom  = log(recruits_biom_lead1),
                logs_abund = log(spawners_abund),
                logs_biom  = log(spawners_biom),
                logrs      = log(rs),
                logrs_abund = log(rs_abund),
                logrs_biom  = log(rs_biom),
                spawners_abund_lag0_anom =
                  (spawners_abund - mean(spawners_abund, na.rm = TRUE)) / 
                  sd(spawners_abund, na.rm = TRUE)
                ,
                spawners_biom_lag0_anom =
                  (spawners_biom - mean(spawners_biom, na.rm = TRUE)) /
                  sd(spawners_biom, na.rm = TRUE),
                recruits_abund_lead1_anom =
                  (recruits_abund_lead1 - mean(recruits_abund_lead1, na.rm = TRUE)) /
                  sd(recruits_abund_lead1, na.rm = TRUE),
                recruits_biom_lead1_anom =
                  (recruits_biom_lead1 - mean(recruits_biom_lead1, na.rm = TRUE)) /
                  sd(recruits_biom_lead1, na.rm = TRUE),
                rs_anom       = 
                  (rs - mean(rs, na.rm = TRUE)) /
                  sd(rs, na.rm = TRUE),
                rs_abund_anom = 
                  (rs_abund - mean(rs_abund, na.rm = TRUE)) /
                  sd(rs_abund, na.rm = TRUE),
                rs_biom_anom = 
                  (rs_biom - mean(rs_biom, na.rm = TRUE)) /
                  sd(rs_biom, na.rm = TRUE),
                logr_abund_anom =
                  (logr_abund - mean(logr_abund, na.rm = TRUE)) /
                  sd(logr_abund, na.rm = TRUE),
                logr_biom_anom =
                  (logr_biom - mean(logr_biom, na.rm = TRUE)) /
                  sd(logr_biom, na.rm = TRUE),
                logs_abund_anom =
                  (logs_abund - mean(logs_abund, na.rm = TRUE)) /
                  sd(logs_abund, na.rm = TRUE),
                logs_biom_anom =
                  (logs_biom - mean(logs_biom, na.rm = TRUE)) /
                  sd(logs_biom, na.rm = TRUE),
                logrs_anom =
                  (logrs - mean(logrs, na.rm = TRUE)) /
                  sd(logrs, na.rm = TRUE),
                logrs_abund_anom =
                  (logrs_abund - mean(logrs_abund, na.rm = TRUE)) /
                  sd(logrs_abund, na.rm = TRUE),
                logrs_biom_anom =
                  (logrs_biom - mean(logrs_biom, na.rm = TRUE)) /
                  sd(logrs_biom, na.rm = TRUE),
                spawner_wl_anom  =
                  (spawner_wl - mean(spawner_wl, na.rm = TRUE)) /
                  sd(spawner_wl, na.rm = TRUE),
                recruit_wl_lead1 = dplyr::lead(recruit_wl, n = 1),
                recruit_wl_lead1_anom =
                  (recruit_wl_lead1 - mean(recruit_wl_lead1, na.rm = TRUE)) /
                  sd(recruit_wl_lead1, na.rm = TRUE),
                spawner_wl_res_anom =
                  (spawner_wl_res - mean(spawner_wl_res, na.rm = TRUE)) /
                  sd(spawner_wl_res, na.rm = TRUE),
                recruit_wl_res_lead1 = dplyr::lag(recruit_wl_res, n = 1),
                recruit_wl_res_lead1_anom =
                  (recruit_wl_res_lead1 - mean(recruit_wl_res_lead1, na.rm = TRUE)) /
                  sd(recruit_wl_res_lead1, na.rm = TRUE)) |> 
  # Remove year before minimum and year after maximum
  dplyr::filter(YEAR >= 1980,
                YEAR <= end.year) |> 
  dplyr::group_by(YEAR, SCINAME, COMNAME) |> 
  dplyr::summarise(
    spawners_abund_lag0 = mean(spawners_abund,
                               na.rm = T),
    spawners_abund_lag0_anom = mean(spawners_abund_lag0_anom,
                                    na.rm = T),
    spawners_biom_lag0 = mean(spawners_biom, na.rm = T),
    spawners_biom_lag0_anom = mean(spawners_biom_lag0_anom,
                                   na.rm = T),
    recruits_abund    = mean(recruits_abund, na.rm = T),
    recruits_abund_lead1      = mean(recruits_abund_lead1,
                                     na.rm = T),
    recruits_abund_lead1_anom = mean(recruits_abund_lead1_anom,
                                     na.rm = T),
    recruits_biom_lead1      = mean(recruits_biom_lead1,
                                    na.rm = T),
    recruits_biom_lead1_anom  = mean(recruits_biom_lead1_anom,
                                     na.rm = T),
    rs                 = mean(rs, na.rm = T),
    rs_abund           = mean(rs_abund, na.rm = T),
    rs_biom            = mean(rs_biom, na.rm = T),
    rs_anom            = mean(rs_anom, na.rm = T),
    rs_abund_anom      = mean(rs_abund_anom, na.rm = T),
    rs_biom_anom       = mean(rs_biom_anom, na.rm = T),
    logr_abund_anom    = mean(logr_abund_anom,
                              na.rm = T),
    logr_biom_anom     = mean(logr_biom_anom,
                              na.rm = T),
    logs_abund_anom    = mean(logs_abund_anom,
                              na.rm = T),
    logs_biom_anom     = mean(logs_biom_anom,
                              na.rm = T),
    logrs_anom         = mean(logrs_anom, na.rm = T),
    logrs_abund_anom    = mean(logrs_abund_anom,
                               na.rm = T),
    logrs_biom_anom     = mean(logrs_biom_anom,
                               na.rm = T),
    spawner_wl_anom     = mean(spawner_wl_anom,
                               na.rm = T),
    recruit_wl_lead1_anom = mean(recruit_wl_lead1_anom,
                                 na.rm = T),
    spawner_wl_res_anom = mean(spawner_wl_res_anom,
                               na.rm = T),
    recruit_wl_res_lead1_anom = mean(recruit_wl_res_lead1_anom,
                                     na.rm = T))


# Prepare SOE output ----------------------
message("Formatting for SOE")

# Whole-shelf output
dat_spec_rec_forSOE <- dat_spec_rec |> 
  dplyr::ungroup() |> 
  dplyr::select(YEAR, COMNAME, rs_anom) |>
  dplyr::rename(Time = YEAR, Value = rs_anom) |>
  dplyr::mutate(Units = "anomaly (r/s)",
                Var  = COMNAME,
                Source = "SVDBS") |> 
  dplyr::select(-COMNAME)

# EPU output
dat_spec_rec_epu_forSOE <- dat_spec_rec_epu |> 
  dplyr::ungroup() |> 
  dplyr::select(YEAR, COMNAME, EPU, rs_anom) |>
  dplyr::rename(Time = YEAR, Value = rs_anom, Region = EPU) |>
  dplyr::mutate(Units = "anomaly (r/s)",
                Var  = COMNAME,
                Source = "SVDBS") |> 
  dplyr::select(-COMNAME)

# Reproduce changes in ecodata::get_productivity_anomaly.R ------------

#Select and rename
epu_rec_anom <- dat_spec_rec_epu_forSOE  |> 
  dplyr::select(Time, EPU = Region, Value, Units, -Source,Var)  |> 
  dplyr::filter(!Time == "2020")

#Select, rename, and bind
productivity_anomaly1 <- dat_spec_rec_forSOE  |> 
  dplyr::select(-Source) |> 
  dplyr::mutate(EPU = "All",
                Var = paste("NE LME",Var)) |> 
  dplyr::bind_rows(epu_rec_anom) |>  
  dplyr::select(Time, Var, Value, EPU, Units) |> 
  dplyr::mutate(Var = paste0(Var, "_Survey"))


# Add stockSMART assessment data -------------------

message("Pulling and tidying stockSMART data")

# following https://github.com/NOAA-EDAB/stockstatusindicator/blob/master/MultisppRec2024.Rmd
# old script adds prelim assessment data. I am leaving out for now

## get stocksmart info -----------------------

NErec <- stocksmart::stockAssessmentData  |> 
  dplyr::filter(RegionalEcosystem == "Northeast Shelf",
         Metric == "Recruitment") |> 
  dplyr::group_by(StockName)  |> 
  dplyr::filter(AssessmentYear == max(AssessmentYear))  |> 
  dplyr::ungroup() 

NErec_sppunits <- NErec  |> 
  dplyr::select(StockName, StockArea, Description, Units) |> 
  dplyr::distinct()

NEbio <- stocksmart::stockAssessmentData  |> 
  dplyr::filter(RegionalEcosystem == "Northeast Shelf",
         Metric == "Abundance")  |> 
  dplyr::group_by(StockName) |> 
  dplyr::filter(AssessmentYear == max(AssessmentYear)) |> 
  dplyr::ungroup() 

NEbio_sppunits <- NEbio |> 
  dplyr::select(StockName, StockArea, Description, Units) |> 
  dplyr::distinct()

SSByr <- NEbio  |> 
  dplyr::group_by(StockName) |> 
  dplyr::select(StockName, AssessmentYear) |> 
  dplyr::filter(AssessmentYear>2018) |> 
  dplyr::distinct()

recyr <- NErec  |> 
  dplyr::group_by(StockName) |> 
  dplyr::select(StockName, AssessmentYear) |> 
  dplyr::filter(AssessmentYear>2018) |> 
  dplyr::distinct()

NErecN <- NErec  |> 
  dplyr::filter(AssessmentYear>2018,
         Units %in% c("Thousand Recruits",
                      "Number x 1,000,000",
                      "Number x 1,000",
                      "Million Recruits")) 

NErecstocks <- NErecN |> 
  dplyr::select(StockName) |> 
  dplyr::distinct()

NEbiohasrec <- NEbio  |> 
  dplyr::filter(StockName %in% NErecstocks$StockName)

bothyr <- NEbiohasrec  |> 
  dplyr::group_by(StockName) |> 
  dplyr::select(StockName, AssessmentYear) |> 
  dplyr::distinct()


## standardize assessment outputs ------------

NErecNstd <- NErecN  |> 
  dplyr::mutate(NfishRec = dplyr::case_when(Units == "Thousand Recruits" ~ Value*1000,
                              Units == "Number x 1,000,000" ~ Value*1000000,
                              Units == "Number x 1,000" ~ Value*1000,
                              Units == "Million Recruits" ~ Value*1000000,
                              TRUE ~ as.numeric(NA)),
         AgeRec = as.numeric(stringr::str_extract(Description, "(\\d)+")),
         SSByr = Year-AgeRec
  )

## convert spiny dog -----------------------
# In 2023 the MT assessment for spiny dogfish now reports stock size as reproductive potential in million pups. 
# In order to keep spiny dogfish in the mix for this analysis, I am converting this to biomass units assuming that a pup weighs 0.5 kg. 
# We are calculating an anomaly of recruits per biomass so I think this is ok even though it is not a measure of adult biomass.

NEbioBstd <- NEbiohasrec  |> 
  dplyr::mutate(SSBkg = dplyr::case_when(Units == "Thousand Metric Tons" ~ Value*1000000,
                           Units == "Metric Tons" ~ Value*1000,
                           Units == "Million Pups" ~ Value*10000000/2,
                           TRUE ~ as.numeric(NA))
         #AgeRec = as.numeric(stringr::str_extract(Description, "(\\d)+")),
         #SSByr = Year-AgeRec
  )


## Perretti anomaly calculation ---------------------------------------

SSrecbio <- NErecNstd  |> 
  dplyr::select(StockName, SSByr, NfishRec)  |> 
  dplyr::left_join(NEbioBstd, by = c("StockName" = "StockName",
                              "SSByr"="Year"))  |> 
  dplyr::select(StockName, SSByr, NfishRec, SSBkg)


min_year <- 1980
max_year <- end.year


AssessFishProdAnomaly <- SSrecbio  |> 
  dplyr::group_by(StockName) |> 
  dplyr::mutate(
    recruits_abund_lead1 = NfishRec,
    spawners_biom = SSBkg,
    YEAR = SSByr,
    rs         = recruits_abund_lead1/
      spawners_biom,
    logr_abund = log(recruits_abund_lead1),
    logs_biom  = log(spawners_biom),
    logrs      = log(rs),
    spawners_biom_lag0_anom =
      (spawners_biom - mean(spawners_biom, na.rm = TRUE)) / sd(spawners_biom, na.rm = TRUE),
    recruits_abund_lead1_anom =
      (recruits_abund_lead1 - mean(recruits_abund_lead1, na.rm = TRUE)) / sd(recruits_abund_lead1, na.rm = TRUE),
    rs_anom       = (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE),
    logr_abund_anom =
      (logr_abund - mean(logr_abund, na.rm = TRUE)) / sd(logr_abund, na.rm = TRUE),
    logs_biom_anom =
      (logs_biom - mean(logs_biom, na.rm = TRUE)) / sd(logs_biom, na.rm = TRUE),
    logrs_anom =
      (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE),
  ) |> 
  # Remove year before minimum and year after maximum
  dplyr::filter(YEAR >= min_year,
                YEAR <= max_year)  |> 
  #Average seasonal anomalies to get yearly anomaly
  dplyr::group_by(YEAR, StockName)  |>
  dplyr::summarise(
    spawners_biom_lag0 = mean(spawners_biom, na.rm = T),
    spawners_biom_lag0_anom = mean(spawners_biom_lag0_anom,
                                   na.rm = T),
    recruits_abund_lead1      = mean(recruits_abund_lead1,
                                     na.rm = T),
    recruits_abund_lead1_anom = mean(recruits_abund_lead1_anom,
                                     na.rm = T),
    rs                 = mean(rs, na.rm = T),
    rs_anom            = mean(rs_anom, na.rm = T),
    logr_abund_anom    = mean(logr_abund_anom,
                              na.rm = T),
    logs_biom_anom     = mean(logs_biom_anom,
                              na.rm = T),
    logrs_anom         = mean(logrs_anom, na.rm = T),
  )


# Back to reproducing ecodata::get_productivity_anomaly.R -------------------

prod_assess1<- AssessFishProdAnomaly |> 
  tidyr::separate(StockName, into= c("Stock", "Region"), sep = "-") |> 
  dplyr::mutate(EPU = dplyr::recode(Region, " Gulf of Maine / Georges Bank" = "NE",
                                    " Gulf of Maine / Cape Hatteras" = "ALL",
                                    " Mid" = "MA",
                                    " Atlantic Coast" = "ALL",
                                    " Georges Bank" = "NE",
                                    " Northwestern Atlantic Coast" = "ALL",
                                    " Gulf of Maine" = "NE",
                                    " Southern New England / Mid" = "MA",
                                    " Cape Cod / Gulf of Maine" = "NE")) |> 
  tidyr::pivot_longer(cols = c("spawners_biom_lag0", "spawners_biom_lag0_anom",
                               "recruits_abund_lead1","recruits_abund_lead1_anom",
                               "rs","rs_anom","logr_abund_anom",
                               "logs_biom_anom","logrs_anom"),
                      names_to = "Var", values_to = "Value") |> 
  dplyr::mutate(Var = paste0(Stock, "-", Var, "-Assessment"),
                Time = YEAR,
                Units = c("NA")) |> 
  dplyr::ungroup() |> 
  dplyr::select(Time, Var, Value, EPU, Units)



# combining into final product ------------------------------
message("Combining final product")
productivity_anomaly<- rbind(productivity_anomaly1, prod_assess1)

# Return productivity_anomaly -----------------------

return(productivity_anomaly)

}
