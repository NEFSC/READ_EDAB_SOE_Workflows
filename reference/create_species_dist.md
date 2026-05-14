# Species distribution on the NES.

Data include time series of depth, distance from shelf and distance
along shelf.

## Usage

``` r
create_species_dist(
  inputPathSurvey,
  inputPathSpecies,
  static_depth,
  static_diagonal,
  static_coast_coord,
  static_strat_areas
)
```

## Arguments

- inputPathSurvey:

  Character string. Full path to the survdat data pull rds file

- inputPathSpecies:

  Character string. Full path to the species list data pull rds file

- static_depth:

  Character string. Path to file with depth data for NE shelf

- static_diagonal:

  Character string. Path to file with along shelf diagonal data

- static_coast_coord:

  Character string. Path to file with lat lon coordinates defining the
  coastline

- static_strat_areas:

  Character string. Path to file defining NEFSC trawl survey strata

## Value

ecodata::species_dist data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::species_dist indicator
create_species_dist(inputPathSurvey = here::here("surveyNoLengths.rds"),
 inputPathSpecies = "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds",
 static_depth =  "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_bath_data.nc",
 static_diagonal = "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/diag.csv",
 static_coast_coord = "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/nes_coast_2.csv",
 static_strat_areas = "/home/<user>/EDAB_Resources/workflow_resources/soe_workflows/stratareas.rds")

} # }

```
