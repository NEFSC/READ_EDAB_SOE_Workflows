# Create data for ecodata::comdat

Processes and combines commercial landings and Menhaden data to produce
a summary of landings and revenue metrics for the State of the Ecosystem
report.

## Usage

``` r
create_comdat(
  comdat_path,
  input_path_species,
  menhaden_path,
  outputPathDataSets
)
```

## Arguments

- comdat_path:

  Character string. Path to commercial_comdat.rds

- input_path_species:

  Character string. Path to the 'SOE_species_list_24.RData' file.

- menhaden_path:

  Character string. Path to the menhaden data output by
  data-raw/create_menhaden_input.R

- outputPathDataSets:

  Character string. Path to folder where data pull should be saved

## Value

A single tibble containing all summarized commercial data.
