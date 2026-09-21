# Creates recreational HMS indicator

This uses the output file from 'pull_rec_hms' as an input parameter The
function formats the data to be used in the SOE It is formatted exactly
like the ecodata data object

## Usage

``` r
create_rec_hms(input_path_rec, input_path_rec_key)
```

## Arguments

- input_path_rec:

  Character string. Full path to the data from the 'pull_rec_hms' csv
  file in EDAB_Dev.

- input_path_rec_key:

  Character string. Full path to the hms_key file in EDAB_Resources
  ("EDAB_Resources/workflow_resources/soe_workflows/hms_key.csv")

## Value

rec_hms, ecodata::rec_hms data frame

## Examples

``` r
if (FALSE) { # \dontrun{
#create the ecodata::rec_hms indicator
create_rec_hms(
 input_path_rec_key = "path/to/hms_key.csv")

} # }
```
