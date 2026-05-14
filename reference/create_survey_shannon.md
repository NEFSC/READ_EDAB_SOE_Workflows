# Calculates survey_shannon diversity index for automated workflow

This uses the survdat data pull from the survey package. It is formatted
exactly like the ecodata data object This calculates the shannon
diversity at the station level (by year, cruise etc) Then takes the mean
of the shannon indices for each year

## Usage

``` r
create_survey_shannon(inputPathAlbatross, inputPathBigelow)
```

## Arguments

- inputPathAlbatross:

  Character string. Full path to the Albatross data pull rds file

- inputPathBigelow:

  Character string. Full path to the Bigelow data pull rds file

## Value

ecodata::survey_shannon data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::survey_shannon indicator
create_aggregate_biomass(inputPathAlbatros = "path/to/albatross.rds",
                         inputPathBigelow = "path/to/bigelow.rds")

} # }

```
