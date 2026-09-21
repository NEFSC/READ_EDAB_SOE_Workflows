# Species distribution on the NES.

Data include time series of depth, distance from shelf and distance
along shelf.

## Usage

``` r
create_species_dist(
  input_path_survey,
  input_path_species,
  input_path_static_depth,
  input_path_static_diagonal,
  input_path_static_coast_coord,
  input_path_static_strat_areas
)
```

## Arguments

- input_path_survey:

  Character string. Full path to the survdat data pull rds file

- input_path_species:

  Character string. Full path to the species list data pull rds file

- input_path_static_depth:

  Character string. Path to file with depth data for NE shelf

- input_path_static_diagonal:

  Character string. Path to file with along shelf diagonal data

- input_path_static_coast_coord:

  Character string. Path to file with lat lon coordinates defining the
  coastline

- input_path_static_strat_areas:

  Character string. Path to file defining NEFSC trawl survey strata

## Value

ecodata::species_dist data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::species_dist indicator
create_species_dist(input_path_survey = here::here("surveyNoLengths.rds"),
 input_path_species = "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds",
 input_path_static_depth =  "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_bath_data.nc",
 input_path_static_diagonal = "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/diag.csv",
 input_path_static_coast_coord = "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_coast_2.csv",
 input_path_static_strat_areas = "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/stratareas.rds")

} # }

```
