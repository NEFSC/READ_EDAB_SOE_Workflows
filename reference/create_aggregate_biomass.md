# Calculates aggregate_biomass data set for automated workflow

This uses the survdat data pull from the survey package and creates EPU
and shelfwide indicators. It is formatted exactly like the ecodata data
object

## Usage

``` r
create_aggregate_biomass(input_path_survey, input_path_species)
```

## Arguments

- input_path_survey:

  Character string. Full path to the survdat data pull rds file

- input_path_species:

  Character string. Full path to the species list data pull rds file

## Value

list

- aggregate_biomass:

  The `ecodata::aggregate_biomass` data frame

- aggregate_biomass_species:

  Stratified mean for each species/Season at EPU level that make up the
  aggregate

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::aggregate_biomass indicator
create_aggregate_biomass(input_path_survey = "path/to/survdatData.rds",
                         input_path_species = "path/to/species.rds")

} # }

```
