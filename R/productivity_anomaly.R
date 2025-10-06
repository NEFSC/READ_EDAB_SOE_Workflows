# working backwards to understand old workflow
# I'll clean this up once its working


# inputs for get_productvity_anomaly.R created in 
# https://github.com/NOAA-EDAB/trawlr/blob/main/run_for_soe/2-load.R

### Choose species to include -----------------------------
# We can only include species with HB to AL length conversions.
species2include <-
  c("ACADIAN REDFISH",
    "AMERICAN PLAICE",
    "ATLANTIC COD",
    "BLACK SEA BASS",
    "BUTTERFISH",
    "HADDOCK",
    "POLLOCK",
    "RED HAKE",
    "SCUP",
    "SILVER HAKE",
    "SUMMER FLOUNDER",
    "WHITE HAKE",
    "WINDOWPANE",
    "WINTER FLOUNDER",
    "WITCH FLOUNDER",
    "YELLOWTAIL FLOUNDER")


# Convert COMNAMES of species to scientific names
dev_folder_path <- "/home/mgrezlik/EDAB_Dev/grezlik"
species2out <-
  read.csv(file.path(dev_folder_path,"popular_species.csv")) |> 
  dplyr::filter(COMNAME %in% species2include) |> 
  dplyr::select(SCINAME) |> 
  dplyr::distinct() |> 
  dplyr::pull(SCINAME)


# TRY ##################

# 2-load.R uses trawlr::load_survdat() here
# I will try to use the survey pull from this repo instead

survey_data <- readRDS(inputPathSurvey)
species <- readRDS(inputPathSpecies)

survdata <- survey_data$survdat |> 
  dplyr::left_join(species, by = 'SVSPP', relationship = "many-to-many") |> 
  dplyr::filter(SCINAME %in% species2out)



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

