# Calculates the heatwave indicator

Combines the surface SST, and the bottom temperature components into a
single indicator

## Usage

``` r
create_heatwave(
  inputPathGBBot,
  inputPathGOMBot,
  inputPathMABBot,
  inputPathGBSurf,
  inputPathGOMSurf,
  inputPathMABSurf
)
```

## Arguments

- inputPathGBBot:

  Character string. Full path to the GB GLORYS input file from Joe
  Caracappa

- inputPathGOMBot:

  Character string. Full path to the GOM GLORYS input file from Joe
  Caracappa

- inputPathMABBot:

  Character string. Full path to the MAB GLORYS input file from Joe
  Caracappa

- inputPathGBSurf:

  Character string. Full path to the GB OISST input file from Kim Hyde

- inputPathGOMSurf:

  Character string. Full path to the GOM OISST input file from Kim Hyde

- inputPathMABSurf:

  Character string. Full path to the MAB OISST input file from Kim Hyde

## Value

ecodata::heatwave data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# create the ecodata::heatwave indicator
create_heatwave(inputPathGBBot = "path/to/input/GBdata.csv",
                        inputPathGOMBot = "path/to/input/GOMdata.csv",
                        inputPathMABBot = "path/to/input/MABdata.csv",
                        inputPathGBSurf = "path/to/input/GBdata.csv",
                        inputPathGOMSurf = "path/to/input/GOMdata.csv",
                        inputPathMABSurf = "path/to/input/MABdata.csv")

} # }
```
