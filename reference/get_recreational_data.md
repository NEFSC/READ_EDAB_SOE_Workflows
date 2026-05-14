# Pulls recreational HMS data from MRIP

This pulls landings data for HMS species in the North and Mid-Atlantic.
The function creates an rds and csv file for each species, to be stored
in the designated output directory. The function then combines all
individual output files and saves as 'hms_mrip\_' followed by the date
in the output directory.

## Usage

``` r
get_recreational_data(outputPath)
```

## Arguments

- outputPath:

  Character string. Full path the directory in which to store files.

## Value

new.hms, a data frame with landings data from MRIP

## Examples

``` r
if (FALSE) { # \dontrun{
get_recreational_data(outputPath = "path/to/output/directory/hms_mrip_date.csv")

} # }
```
