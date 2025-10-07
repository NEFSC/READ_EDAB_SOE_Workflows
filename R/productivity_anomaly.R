# working backwards to understand old workflow
# I'll clean this up once its working


# inputs for get_productvity_anomaly.R created in 
# https://github.com/NOAA-EDAB/trawlr/blob/main/run_for_soe/2-load.R

# Choose species to include -----------------------------
# We can only include species with HB to AL length conversions.
species2include <- c(
  "ACADIAN REDFISH", "AMERICAN PLAICE", "ATLANTIC COD", "BLACK SEA BASS",
  "BUTTERFISH", "HADDOCK", "POLLOCK", "RED HAKE", "SCUP", "SILVER HAKE",
  "SUMMER FLOUNDER", "WHITE HAKE", "WINDOWPANE", "WINTER FLOUNDER",
  "WITCH FLOUNDER", "YELLOWTAIL FLOUNDER"
)


# Convert COMNAMES of species to scientific names
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
df_len_at_age1 <- survdata  |> 
  dplyr::filter(YEAR >= (1980 - 1),
                YEAR <= (2022 + 1),
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
dat_spec_rec <- survdata |> 
  dplyr::left_join(dat_tows, by = c("CRUISE6", "YEAR", "SEASON")) |> 
  dplyr::left_join(df_len_at_age1, by = c("COMNAME", "SCINAME")) |> 
  dplyr::filter(EPU %in% c("MAB", "GB", "GOM")) |> 
  dplyr::filter(YEAR >= (1980 - 1), YEAR <= (2022 + 1)) |> 
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

# # Save as .rds files
# saveRDS(dat_spec_rec_forSOE, file = "dat_spec_rec_forSOE.rds")
# saveRDS(dat_spec_rec_epu_forSOE, file = "dat_spec_rec_epu_forSOE.rds")
# 
# # Load them back
# dat_spec_rec_forSOE_loaded <- readRDS("dat_spec_rec_forSOE.rds")
# dat_spec_rec_epu_forSOE_loaded <- readRDS("dat_spec_rec_epu_forSOE.rds")



# ecodata::productivity_anomaly created by get_productivity_anomaly.R
# https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/get_productivity_anomaly.R

productivity_anomaly_rdata <- "dat_spec_rec_forSOE.Rdata"
productivity_anomaly__epu_rdata <- "dat_spec_rec_epu_forSOE.Rdata"
prod_anom_assessment_rds<- "AssessFishProdAnomaly - Sarah Gaichas - NOAA Federal.rds"

# holding static files in my dev folder for now
# will replace these. Hopefully with workflow path
dev_folder_path <- "/home/mgrezlik/EDAB_Dev/grezlik"

load(file.path(dev_folder_path,productivity_anomaly_rdata))
load(file.path(dev_folder_path,productivity_anomaly__epu_rdata))

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

prod_assess<- readRDS(file.path(dev_folder_path,prod_anom_assessment_rds))
prod_assess1<- prod_assess |>
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

# call above creates a warning as tidyr::separate() expects only 2 values
# separated by a "-". Mid-Atlantic causes 3 sections separated by "-".
# Sarah accounted for this in the next line starting dplyr::mutate()

productivity_anomaly<- rbind(productivity_anomaly1, prod_assess1)

