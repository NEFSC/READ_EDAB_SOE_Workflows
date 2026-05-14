# Calculates aggregate_biomass data set for automated workflow

This uses the survdat data pull from the survey package and creates EPU
and shelfwide indicators. It is formatted exactly like the ecodata data
object

## Usage

``` r
create_aggregate_biomass(inputPathSurvey, inputPathSpecies)
```

## Arguments

- inputPathSurvey:

  Character string. Full path to the survdat data pull rds file

- inputPathSpecies:

  Character string. Full path to the species list data pull rds file

## Value

`ecodata::aggregate_biomass` data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::aggregate_biomass indicator
create_aggregate_biomass(inputPathSurvey = "path/to/survdatData.rds",
                         inputPathSpecies = "path/to/species.rds")

} # }

```
