# Indicator Dependencies

All relevant package dependencies are listed in the DESCRIPTIONS file under `Imports`.
When all indicators have been added to the repo we can provide a list of package versions

## Survey Based Indicators

The survey data will pulled from the Oracle database via a cron job (quarterly?) using R function

```
workflow_pull_survey_data(channel,output_path_datasets)
```

* `channel` is a connection object created using `ROracle::dbConnect()`
* `output_path_datasets` is the path to the folder where "raw" data is stored (`EDAB_Datasets`)
* Currently the data sets created are:
  - `survey_no_lengths_data.rds` - used in `aggregate_biomass`
  - `bigelow_data.rds` - used in `survey_shannon`
  - `albatros_data.rds` - used in `survey_shannon`
  - `condition_data.rds` - used in `condition`
  - `mass_inshore_data` - used in `mass_inshore_survey`
  - `survey_biological_data` - used in `productivity_anomaly`
  - `survey_biological__by_epu_data` - used in `productivity_anomaly`
  
### aggregate_biomass

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_aggregate_biomass(output_path_indicators,input_path_survey,input_path_species)
```

* `input_path_species` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `input_path_survey` is the path to the dynamically created survey data `EDAB_Datasets/survey_no_lengths_data.rds`. 
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `aggregate_biomass.rds` to match the `ecodata` package dataset

### survey_shannon

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_survey_shannon(output_path_indicators,input_path_bigelow,input_path_albatross)
```

* `input_path_bigelow` is the path to the data set `EDAB_Datasets/bigelow_data.rds`.
* `input_path_albatross` is the path to the created survey data `EDAB_Datasets/albatross_data.rds`. 
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `survey_shannon.rds` to match the `ecodata` package dataset

### mass_inshore_survey

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_mass_inshore_survey(output_path_indicators,input_path_mass_survey,input_path_species)
```

* `input_path_mass_survey` is the path to data set `EDAB_Datasets/mass_inshore_data.rds`.
* `input_path_species` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `mass_inshore_survey.rds` to match the `ecodata` package dataset


### exp_n

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_exp_n(output_path_indicators,input_path_bigelow,input_path_albatross)
```

* `input_path_bigelow` is the path to static data set `EDAB_Datasets/bigelow_data.rds`.
* `input_path_albatross` is the path to static data set `EDAB_Datasets/albatross_data.rds`. 
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `exp_n.rds` to match the `ecodata` package dataset

### species_dist

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_species_dist(input_path_survey,input_path_species, input_path_static_depth,
                      input_path_static_diagonal, input_path_static_coast_coord, input_path_static_strat_areas)
```

* `input_path_species` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `input_path_survey` is the path to the dynamically created survey data `EDAB_Datasets/survey_no_lengths_data.rds`. 
* `input_path_static_depth` is the path to the file `nes_bath_data.nc`
* `input_path_static_diagonal` is the path to the file `diag.csv`
* `input_path_static_coast_coord` is the path to the file `nes_coast_2.csv`
* `input_path_static_strat_areas` is the path to the file `stratareas.rds`

The rds file name is hardcoded as `species_dist.rds` to match the `ecodata` package dataset

### productivity_anomaly
To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_productivity_anomaly(
                              input_survey_bio_epu,
                              input_survey_bio,
                              input_static_lw_table,
                              input_path_species,
                              input_static_length_convert,
                              output_path_indicators
                              )
```

* `input_survey_bio_epu` is the path to the dynamically created survey data `EDAB_Datasets/Workflows/survey_biological_by_epu_data.rds`.
* `input_survey_bio` is the path to the dynamically created survey data `EDAB_Datasets/Workflows/survey_biological_data.rds`.
* `input_static_lw_table` is the path to the static length weight table from Miller 2013 `EDAB_Datasets/Workflows/df_lw.rda`.
* `input_path_species` is the path to static data set `EDAB_Datasets/Workflows/SOE_species_list_24.rds`.
* `input_static_length_convert` is the path to the static length conversion table `EDAB_Datasets/Workflows/df_lconv.rda`.
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.

The rds file name is hardcoded as `productivity_anomaly.rds` to match the `ecodata` package dataset

### species_condition
```
workflow_condition(input_path_condition, input_path_lw_coeffs, input_path_species, output_path_indicators)
```
* `input_path_condition` is the path to the static data set `EDAB_Dev/beet/condition_data.rds`
* `input_path_species` is the path to static data set `EDAB_Resources/workflow_resources/soe_workflows/species.codes.csv`
* `input_path_lw_coeffs` is the path to the static data set `EDAB_Resources/workflow_resources/soe_workflows/LWparams.csv`
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `condition.rds` to match the `ecodata` package dataset


## Commercial Landings Based Indicators

The commercial data will pulled from the Oracle database via a cron job (quarterly?) using R function

```
workflows_pull_commercial_data(channel,output_path_datasets)
```

* `channel` is a connection object created using `ROracle::dbConnect()`
* `output_path_datasets` is the path to the folder where "raw" data is stored (`EDAB_Datasets`)
* Currently the data sets created are:
  - `commercial_comdat_data.rds` - used in `comdat`
  - `commercial_bennet_data.rds` - used in `bennet`


### Bennet

To run the "workflow" below, it is assumed that the commercial data has been pulled using the 
`get_commercial_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_bennet(input_path_bennet, input_path_species, output_path_indicators)
```

* `input_path_species` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `input_path_bennet` is the path to the dynamically created commercial data `EDAB_Datasets/commercial_benent_data.rds`. 
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `bennet.rds` to match the `ecodata` package dataset


### comdat

To run the "workflow" below, it is assumed that the commercial data has been pulled using the 
`get_commercial_data` function above and that menhaden data have been pulled using the 
`create_menhaden_input.R` script in the folder `data-raw`.
*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```R
workflow_comdat(input_path_comdat, input_path_species, input_path_menhaden, output_path_datasets)
```

* `input_path_comdat` is the path to the raw, comprehensive commercial landings data file, e.g., `EDAB_Datasets/commercial_comdat_data.rds`.
* `input_path_species` is the path to the species list used for grouping, e.g., `EDAB_Datasets/SOE_species_list_24.rds`.
* `input_path_menhaden` is the path to the Menhaden landings data output by create_`create_menhaden_input.R`.
* `output_path_datasets` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `comdat.rds` to match the `ecodata` package dataset

## `stocksmart` Based Indicators

To run the "workflow" below, it is assumed that the `stocksmart` R package has been updated to include recent assessment data.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_stock_status(input_path_decoder, output_path_indicators)
```

* `input_path_decoder` is the path to static data set `EDAB_Datasets/decoder.csv`.
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `stock_status.rds` to match the `ecodata` package dataset


## Oceanographic Indicators

### Transition Dates (trans_dates)

To run the "workflow" below, it is assumed that the static input file from Kevin Friedland is present.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*


```
workflow_trans_dates(input_path_sst, output_path_indicators)
```

* `input_path_sst` is the path to static data set `EDAB_Datasets/TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv`.
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `trans_dates.rds` to match the `ecodata` package dataset

### Heatwave (heatwave & heatwave_year)

To run the "workflow" below, it is assumed that the static, detrended input files from Joe Caracappa and Vince Saba are present.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*


```
workflow_heatwave(input_path_gb_bot,input_path_gom_bot,input_path_mab_bot,input_path_gb_surf,input_path_gom_surf,input_path_mab_surf)
workflow_heatwave_year(input_path_gb_bot,input_path_gom_bot,input_path_mab_bot,input_path_gb_surf,input_path_gom_surf,input_path_mab_surf)
```

* `input_path_gb_xxx` is the path to static, detrended data sets `EDAB_Datasets/GB_SST_1982_to_2024_detrended.csv` or `EDAB_Datasets/daily_bottomT_GB_1959_2024_detrended.csv`for Georges Bank surface and bottom heatwaves, respectively.
* `input_path_gom_xxx` is the path to static, detrended data sets `EDAB_Datasets/GOM_SST_1982_to_2024_detrended.csv` or `EDAB_Datasets/daily_bottomT_GOM_1959_2024_detrended.csv`for Gulf of Maine surface and bottom heatwaves, respectively.
* `input_path_mab_xxx` is the path to static, detrended data sets `EDAB_Datasets/MAB_SST_1982_to_2024_detrended.csv` or `EDAB_Datasets/daily_bottomT_MAB_1959_2024_detrended.csv`for Mid-Atlantic Bight surface and bottom heatwaves, respectively.
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file names are hardcoded as `heatwave.rds`, `heatwave_year.rds` to match the `ecodata` package dataset



## Recreational Landings Based Indicators

### rec_hms

Recreational data will pulled using the function below.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_pull_recreational_data(output_path_datasets)
```

* `output_path_datasets` is the path to the folder where "raw" data is stored, currently `EDAB_Dev/atyrell`
* The data set created is `hms_mrip_(Sys.Date).csv`

To run the "workflow" below, it is assumed that the recreational data has been pulled using the 
`workflow_pull_recreational_data` function above. 

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_rec_hms(input_path_rec,input_path_rec_key, output_path_indicators)
```

* `input_path_rec` is the path to the data set created by `workflow_pull_recreational_data`, currently residing in `EDAB_Dev/atyrell/hms_mrip_(Sys.Date).csv`.
* `input_path_rec_key` is the path to the static data set `EDAB_Resources/workflow_resources/soe_workflows/hms_key.csv`
* `output_path_indicators` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `rec_hms.rds` to match the `ecodata` package dataset
