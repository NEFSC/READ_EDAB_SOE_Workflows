# Create species condition indicator

This function calculates mean relative condition calculated from the
NEFSC bottom trawl survey Methods derived from Laurel Smith
(https://github.com/Laurels1/Condition/blob/master/R/RelConditionEPU.R)

## Usage

``` r
create_condition(inputPath, inputPathLW, inputPathSpecies)
```

## Arguments

- inputPath:

  Character string. Full path to the condition data pull rds file.

- inputPathLW:

  Character string. Full path to the LWparams csv file in
  'EDAB_Resources/workflow_resources/soe_workflows'.

- inputPathSpecies:

  Character string. Full path to the species.codes csv file in
  'EDAB_Resources/workflow_resources/soe_workflows'.

## Value

condition, ecodata::condition data frame

## Examples

``` r
if (FALSE) { # \dontrun{
#create the ecodata::condition indicator
create_condition(
 inputPathLW = "path/to/LWparams.csv",
 inputPathSpecies = "path/to/species.codes.csv")

} # }
```
