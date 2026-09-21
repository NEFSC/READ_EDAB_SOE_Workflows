# Calculates the heatwave_year indicator

Combines the surface SST, and the bottom temperature components into a
single indicator

## Usage

``` r
create_heatwave_year(
  input_path_gb_bot,
  input_path_gom_bot,
  input_path_mab_bot,
  input_path_gb_surf,
  input_path_gom_surf,
  input_path_mab_surf
)
```

## Arguments

- input_path_gb_bot:

  Character string. Full path to the GB GLORYS input file from Joe
  Caracappa

- input_path_gom_bot:

  Character string. Full path to the GOM GLORYS input file from Joe
  Caracappa

- input_path_mab_bot:

  Character string. Full path to the MAB GLORYS input file from Joe
  Caracappa

- input_path_gb_surf:

  Character string. Full path to the GB OISST input file from Kim Hyde

- input_path_gom_surf:

  Character string. Full path to the GOM OISST input file from Kim Hyde

- input_path_mab_surf:

  Character string. Full path to the MAB OISST input file from Kim Hyde

## Value

ecodata::heatwave data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::heatwave_year indicator
create_heatwave_year(input_path_gb_bot = "path/to/input/GBdata.csv",
                        input_path_gom_bot = "path/to/input/GOMdata.csv",
                        input_path_mab_bot = "path/to/input/MABdata.csv",
                        input_path_gb_surf = "path/to/input/GBdata.csv",
                        input_path_gom_surf = "path/to/input/GOMdata.csv",
                        input_path_mab_surf = "path/to/input/MABdata.csv")

} # }
```
