# Calculates expected number of species for automated workflow

This uses the survdat data pull from the survey package. It is formatted
exactly like the ecodata data object This calculates the expected number
of species per tow in the NEFSC Bottom Trawl Survey for Fall and Spring

## Usage

``` r
create_exp_n(inputPathAlbatross, inputPathBigelow)
```

## Arguments

- inputPathAlbatross:

  Character string. Full path to the Albatross data pull rds file

- inputPathBigelow:

  Character string. Full path to the Bigelow data pull rds file

## Value

ESn.epu, ecodata::exp_n data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::exp_n indicator
create_exp_n(inputPathAlbatros = "path/to/albatross.rds",
                         inputPathBigelow = "path/to/bigelow.rds")

} # }
```
