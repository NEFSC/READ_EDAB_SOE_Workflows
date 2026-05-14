# Calculates trans_dates data set for automated workflow

This uses a static input file from Kevin Friedland It is formatted
exactly like the ecodata data object

## Usage

``` r
create_trans_dates(inputPath)
```

## Arguments

- inputPath:

  Character string. Full path to the input data file

## Value

ecodata::trans_dates data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::trans_dates indicator for 2025
create_trans_dates(inputPath = "path/to/inputData.csv")

} # }

```
