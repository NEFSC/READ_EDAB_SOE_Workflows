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
    input_survey_bio,
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
message("Loading survey data")
survey_data_epu <- readRDS(input_survey_bio_epu) |> 
  as.data.frame() |>
  # convert columns to match trawlr function
  dplyr::mutate(
    CRUISE6 = as.character(CRUISE6),
    STATION = as.character(STATION),
    EST_TOWDATE = as.Date(EST_TOWDATE)
  )

species <- readRDS(inputPathSpecies)

survdata_epu <- survey_data_epu |> 
  dplyr::left_join(species, by = 'SVSPP', relationship = "many-to-many") |> 
  dplyr::filter(SCINAME %in% species2out)

# Estimate the length cut-off for age-1 fish
# https://github.com/NOAA-EDAB/trawlr/blob/392fdc6e841893bdfe2322abe6801cc695e04312/R/func-data.R#L771

message("estimating length cut-off for age-1 fish")

df_len_at_age1_epu <- survdata_epu  |> 
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

dat_tows_epu <- survdata_epu  |> 
  dplyr::distinct(CRUISE6, YEAR, SEASON, STATION, STRATUM)  |> 
  dplyr::group_by(CRUISE6, YEAR, SEASON)  |> 
  dplyr::summarise(n_tows = dplyr::n(), .groups = "drop")

# merge and calculate recruitment-related variables ---------------------

message("Calculating recruitment-related variables")

dat_spec_rec_epu <- survdata_epu |> 
  dplyr::left_join(dat_tows_epu, by = c("CRUISE6", "YEAR", "SEASON")) |> 
  dplyr::left_join(df_len_at_age1_epu, by = c("COMNAME", "SCINAME")) |> 
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
message("Summarizing whole-shelf for SOE")
dat_spec_rec_summary <- dat_spec_rec_epu |> 
  dplyr::group_by(YEAR, SCINAME, COMNAME) |> 
  dplyr::summarise(
    spawners_abund = sum(ind_sp_abund, na.rm = TRUE) / mean(n_tows, na.rm = TRUE),
    recruits_abund = sum(ind_rec_abund, na.rm = TRUE) / mean(n_tows, na.rm = TRUE),
    spawner_wl     = mean(ind_sp_wl, na.rm = TRUE),
    recruit_wl     = mean(ind_rec_wl, na.rm = TRUE),
    .groups = "drop") |> 
  dplyr::mutate(
    recruits_abund_lead1 = dplyr::lead(recruits_abund, n = 1),
    rs = recruits_abund_lead1 / spawners_abund,
    rs_anom = (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

# summarize by EPU ------------------
message("Summarizing by EPU for SOE")
dat_spec_rec_epu_summary <- dat_spec_rec_epu |> 
  dplyr::group_by(YEAR, SCINAME, COMNAME, EPU) |> 
  dplyr::reframe(
    spawners_abund = sum(ind_sp_abund, na.rm = TRUE) / mean(n_tows),
    recruits_abund = sum(ind_rec_abund, na.rm = TRUE) / mean(n_tows),
    spawner_wl = mean(ind_sp_wl, na.rm = TRUE),
    recruit_wl = mean(ind_rec_wl, na.rm = TRUE)
  ) |> 
  dplyr::mutate(
    recruits_abund_lead1 = dplyr::lead(recruits_abund, n = 1),
    rs = recruits_abund_lead1 / spawners_abund,
    rs_anom = (rs - mean(rs, na.rm = TRUE)) / sd(rs, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()


# Prepare SOE output ----------------------
message("Formatting for SOE")

# Whole-shelf output
dat_spec_rec_forSOE <- dat_spec_rec_summary |> 
  dplyr::select(YEAR, COMNAME, rs_anom) |>
  dplyr::rename(Time = YEAR, Value = rs_anom) |>
  dplyr::mutate(Units = "anomaly (r/s)",
                Var  = factor(COMNAME),
                Source = "SVDBS") |> 
  dplyr::select(-COMNAME)

# EPU output
dat_spec_rec_epu_forSOE <- dat_spec_rec_epu_summary |> 
  dplyr::select(YEAR, COMNAME, EPU, rs_anom) |>
  dplyr::rename(Time = YEAR, Value = rs_anom, Region = EPU) |>
  dplyr::mutate(Units = "anomaly (r/s)",
                Var  = factor(COMNAME),
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
