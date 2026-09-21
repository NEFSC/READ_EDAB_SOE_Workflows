# Create data for ecodata::comdat

Processes and combines commercial landings and Menhaden data to produce
a summary of landings and revenue metrics for the State of the Ecosystem
report.

## Usage

``` r
create_comdat(input_path_comdat, input_path_species, input_path_menhaden)
```

## Arguments

- input_path_comdat:

  Character string. Path to commercial_comdat.rds

- input_path_species:

  Character string. Path to the 'SOE_species_list_24.RData' file.

- input_path_menhaden:

  Character string. Path to the menhaden data output by
  data-raw/create_menhaden_input.R

## Value

list

- comdat:

  `ecodata::comdat` data frame

- comdat_species:

  species data used to create the `comdat` indicator
