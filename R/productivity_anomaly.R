#' Calculate Productivity Anomalies from Survey Data
#'
#' @description
#' Processes survey and species data to calculate recruitment-spawner ratios (r/s) 
#' and their anomalies (z-scores) across species and EPUs. Outputs data formatted 
#' for State of the Ecosystem (SOE) reporting.
#'
#' @param input_survey_bio_epu File path to survey data with bio data and epu (.rds format)
#' @param inputPathSpecies File path to species lookup table (.rds format)
#' @param species2include Character vector of species to include (default: commonly surveyed species)
#'
#' @return a combined productivity anomaly data set
#'
#' @examples
#' \dontrun{
#' create_productivity_anomaly(
#'   input_survey_bio_epu = "survey_bio_epu.rds",
#'   inputPathSpecies = "species_lookup.rds",
#' )
#' }
#'
#' @export

create_productivity_anomaly <- function(
    input_survey_bio_epu,
    inputPathSpecies,
    species2include = c(
      "ACADIAN REDFISH", "AMERICAN PLAICE", "ATLANTIC COD", "BLACK SEA BASS",
      "BUTTERFISH", "HADDOCK", "POLLOCK", "RED HAKE", "SCUP", "SILVER HAKE",
      "SUMMER FLOUNDER", "WHITE HAKE", "WINDOWPANE", "WINTER FLOUNDER",
      "WITCH FLOUNDER", "YELLOWTAIL FLOUNDER"
    )
) {
  
  end.year <- format(Sys.Date(),"%Y")

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
survey_data <- readRDS(input_survey_bio_epu)
species <- readRDS(inputPathSpecies)

survdata <- survey_data |> 
  dplyr::left_join(species, by = 'SVSPP', relationship = "many-to-many") |> 
  dplyr::filter(SCINAME %in% species2out)

# Estimate length at age-1 for species with age data --------------------
message("Estimating length at age-1")
df_len_at_age1 <- survdata  |> 
  dplyr::filter(YEAR >= (1980 - 1),
                YEAR <= end.year,
                !is.na(AGE),
                !is.na(LENGTH)) |> 
  dplyr::group_by(COMNAME, SCINAME)  |> 
  dplyr::summarise(length_at_age1 = {
    len_model <- glm(LENGTH ~ AGE, family = "poisson")
    as.numeric(predict(len_model, newdata = data.frame(AGE = 2), type = "response"))
  }, .groups = "drop")

# count number of tows per cruise/year/season ----------------------
dat_tows <- survdata  |> 
  dplyr::distinct(CRUISE6, YEAR, SEASON, STATION, STRATUM)  |> 
  dplyr::group_by(CRUISE6, YEAR, SEASON)  |> 
  dplyr::summarise(n_tows = dplyr::n(), .groups = "drop")

# merge and calculate recruitment-related variables ---------------------
message("Calculating recruitment-related variables")
dat_spec_rec <- survdata |> 
  dplyr::left_join(dat_tows, by = c("CRUISE6", "YEAR", "SEASON")) |> 
  dplyr::left_join(df_len_at_age1, by = c("COMNAME", "SCINAME")) |> 
  dplyr::filter(EPU %in% c("MAB", "GB", "GOM")) |> 
  dplyr::filter(YEAR >= (1980 - 1), YEAR <= end.year) |> 
  dplyr::mutate(
    NUMLEN = as.numeric(NUMLEN),
    INDWT  = as.numeric(INDWT),
    WTperLENGTH = INDWT / LENGTH,
    rank_LENGTH = dplyr::cume_dist(LENGTH),
    maturity = ifelse(is.na(length_at_age1),
                      ifelse(rank_LENGTH > 0.2, "spawner", "recruit"),
                      ifelse(LENGTH > length_at_age1, "spawner", "recruit")),
    ind_sp_abund = ifelse(maturity == "spawner", NUMLEN, 0),
    ind_rec_abund = ifelse(maturity == "recruit", NUMLEN, 0),
    ind_sp_wl = ifelse(maturity == "spawner", WTperLENGTH, NaN),
    ind_rec_wl = ifelse(maturity == "recruit", WTperLENGTH, NaN)
  )

# summarize by species (whole shelf) ---------------------
message("Summarizing for SOE")
dat_spec_rec_summary <- dat_spec_rec |> 
  dplyr::group_by(YEAR, SCINAME, COMNAME) |> 
  dplyr::reframe(
    spawners_abund = sum(ind_sp_abund, na.rm = TRUE) / unique(n_tows),
    recruits_abund = sum(ind_rec_abund, na.rm = TRUE) / unique(n_tows),
    spawner_wl     = mean(ind_sp_wl, na.rm = TRUE),
    recruit_wl     = mean(ind_rec_wl, na.rm = TRUE)
  ) |> 
  dplyr::arrange(YEAR, COMNAME, SCINAME)

## calc rs ---------------
dat_spec_rec_summary <- dat_spec_rec_summary |> 
  dplyr::mutate(
    recruits_abund_lead1 = dplyr::lead(recruits_abund, n = 1),
    rs = recruits_abund_lead1 / spawners_abund,
    rs_anom = (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE)
  )


# summarize by EPU --------------------------
dat_spec_rec_epu <- dat_spec_rec |> 
  dplyr::group_by(YEAR, SCINAME, COMNAME, EPU)  |> 
  dplyr::reframe(
    spawners_abund = sum(ind_sp_abund, na.rm = TRUE) / unique(n_tows),
    recruits_abund = sum(ind_rec_abund, na.rm = TRUE) / unique(n_tows),
    spawner_wl = mean(ind_sp_wl, na.rm = TRUE),
    recruit_wl = mean(ind_rec_wl, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::arrange(YEAR, EPU, COMNAME, SCINAME)

## calc rs ------------------------
dat_spec_rec_epu <- dat_spec_rec_epu |> 
  dplyr::mutate(
    recruits_abund_lead1 = dplyr::lead(recruits_abund, n = 1),
    rs = recruits_abund_lead1 / spawners_abund,
    rs_anom = (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE)
  )



# prepare data frames for SOE -----------
message("Formatting for SOE")
# whole-shelf SOE output
dat_spec_rec_forSOE <- dat_spec_rec_summary |>
  dplyr::ungroup() |>
  dplyr::select(YEAR, COMNAME, rs_anom) |>
  dplyr::rename(Time = YEAR,
                Value = rs_anom) |>
  dplyr::mutate(Units = "anomaly (r/s)",
                Var  = COMNAME,
                Source = "SVDBS") |>
  dplyr::select(-COMNAME)

# EPU SOE output
dat_spec_rec_epu_forSOE <- dat_spec_rec_epu |>
  dplyr::ungroup() |>
  dplyr::select(YEAR, COMNAME, EPU, rs_anom) |>
  dplyr::rename(Time = YEAR,
                Value = rs_anom,
                Region = EPU) |>
  dplyr::mutate(Units = "anomaly (r/s)",
                Var  = COMNAME,
                Source = "SVDBS") |>
  dplyr::select(-COMNAME)



#Select and rename
epu_rec_anom <- dat_spec_rec_epu_forSOE  |> 
  dplyr::select(Time, EPU = Region, Value, Units, -Source,Var)  |> 
  dplyr::filter(!Time == "2020")

#Select, rename, and bind
productivity_anomaly1 <- dat_spec_rec_forSOE |>
  dplyr::select(-Source) |>
  dplyr::mutate(EPU = "All",
                Var = paste("NE LME", Var)) |>
  (\(x) rbind(x, epu_rec_anom))() |>
  as.data.frame() |>
  tibble::as_tibble() |>
  dplyr::select(Time, Var, Value, EPU, Units) |>
  dplyr::mutate(Var = paste0(Var, "_Survey"))

# prod_assess<- readRDS(file.path(prod_anom_sarah))

# recreating part of MultisppRec2024 -------------
# https://github.com/NOAA-EDAB/stockstatusindicator/blob/master/MultisppRec2024.Rmd

message("Pulling and tidying stockSMART data")
AssessFishProdAnomaly <- stocksmart::stockAssessmentData |>
  # Keep only Northeast Shelf
  dplyr::filter(RegionalEcosystem == "Northeast Shelf") |>
  # For each StockName, Metric, and Year, collapse duplicate values
  dplyr::group_by(StockName, Metric, Year) |>
  dplyr::summarise(
    Value = mean(Value, na.rm = TRUE),
    Units = dplyr::first(Units),
    Description = dplyr::first(Description),
    AssessmentYear = max(AssessmentYear, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Optional: arrange for readability
  dplyr::arrange(StockName, Metric, Year)

AssessFishProdAnomaly_wide <- AssessFishProdAnomaly |>
  tidyr::pivot_wider(
    names_from = Metric,
    values_from = Value
  )

AssessFishProdAnomaly_wide <- AssessFishProdAnomaly_wide |>
  # rename for clarity
  dplyr::rename(
    spawners_biom_lag0 = Abundance,
    recruits_abund = Recruitment
  ) |>
  dplyr::group_by(StockName) |>
  dplyr::mutate(
    # Lead/lag relationships
    recruits_abund_lead1 = dplyr::lead(recruits_abund, 1),
    
    # Recruitment/spawner ratio
    rs = recruits_abund_lead1 / spawners_biom_lag0,
    
    # anomalies (z-scores)
    spawners_biom_lag0_anom = (spawners_biom_lag0 - mean(spawners_biom_lag0, na.rm = TRUE)) / sd(spawners_biom_lag0, na.rm = TRUE),
    recruits_abund_lead1_anom = (recruits_abund_lead1 - mean(recruits_abund_lead1, na.rm = TRUE)) / sd(recruits_abund_lead1, na.rm = TRUE),
    rs_anom = (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE),
    
    # log-transformed anomalies
    logr_abund_anom = (log(recruits_abund) - mean(log(recruits_abund), na.rm = TRUE)) / sd(log(recruits_abund), na.rm = TRUE),
    logs_biom_anom = (log(spawners_biom_lag0) - mean(log(spawners_biom_lag0), na.rm = TRUE)) / sd(log(spawners_biom_lag0), na.rm = TRUE),
    logrs_anom = (log(rs) - mean(log(rs), na.rm = TRUE)) / sd(log(rs), na.rm = TRUE)
  ) |>
  dplyr::ungroup()


prod_assess1<- AssessFishProdAnomaly_wide |>
  tidyr::separate(StockName, into= c("Stock", "Region"), sep = "-") |>
  # line above creates a warning as tidyr::separate() expects only 2 values
  # separated by a "-". Mid-Atlantic causes 3 sections separated by "-".
  # Sarah accounted for this in the next line starting dplyr::mutate()
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
                Time = Year,
                Units = c("NA")) |>
  dplyr::ungroup() |>
  dplyr::select(Time, Var, Value, EPU, Units)


productivity_anomaly <- rbind(productivity_anomaly1, prod_assess1)

return(productivity_anomaly)

}