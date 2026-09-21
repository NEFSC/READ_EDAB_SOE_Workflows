# Function to process Massachusetts inshore bottom trawl survey for automated workflow

Data include aggregated time series of inshore fishery-independent trawl
survey data from Massachusetts waters. MA inshore surveys have been
performed biannually in the spring and fall since 1978.

## Usage

``` r
create_mass_inshore_survey(input_path_mass_survey, input_path_species)
```

## Arguments

- input_path_mass_survey:

  Character string. Full path to the mass inshore data pull rds file
  created by workflow_pull_survey_data

- input_path_species:

  Character string. Full path to the species list data pull rds file

## Value

ecodata::mass_inshore_survey data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::mass_inshore_survey indicator
create_mass_inshore_survey(input_path_mass_survey <- here::here("mass_inshore.rds"),
                         input_path_species <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds")

} # }

```
