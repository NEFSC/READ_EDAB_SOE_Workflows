# Calculates species_groupings data set for automated workflow

This data object is used in many of the other workflows in table joins
It is formatted exactly like the ecodata data object

## Usage

``` r
create_species_groupings(input_path_species_list, input_path_functional_group)
```

## Arguments

- input_path_species_list:

  Character string. Full path to the original rds file from 2024

- input_path_functional_group:

  Character string. Full path to the species functional group csv file.
  SVSPP codes are mapped to functional group

## Value

ecodata::species_groupings data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::species_groupings table
create_species_groupings(input_path_species_list = "path/to/SOE_species_list_old.rds",
                         input_path_functional_group = "path/to/functional_groups_list.csv")

} # }
```
