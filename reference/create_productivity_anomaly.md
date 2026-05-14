# Calculate Productivity Anomalies from Survey Data

Processes survey and species data to calculate recruitment-spawner
ratios (r/s) and their anomalies (z-scores) across species and EPUs.
Outputs data formatted for State of the Ecosystem (SOE) reporting.

## Usage

``` r
create_productivity_anomaly(
  input_survey_bio,
  input_survey_bio_epu,
  input_static_lw_table,
  inputPathSpecies,
  input_static_length_convert,
  species2include = c("SPINY DOGFISH", "BARNDOOR SKATE", "WINTER SKATE",
    "CLEARNOSE SKATE", "ROSETTE SKATE", "LITTLE SKATE", "SMOOTH SKATE", "THORNY SKATE",
    "OFFSHORE HAKE", "SILVER HAKE", "ATLANTIC COD", "HADDOCK", "POLLOCK", "WHITE HAKE",
    "RED HAKE", "ATLANTIC HALIBUT", "AMERICAN PLAICE", "SUMMER FLOUNDER",
    "YELLOWTAIL FLOUNDER", "WINTER FLOUNDER", "WITCH FLOUNDER", "WINDOWPANE",
    "BUTTERFISH", "BLUEFISH", "BLACK SEA BASS", "SCUP", "TILEFISH", "ACADIAN REDFISH",
    "ATLANTIC WOLFFISH", "OCEAN POUT", "GOOSEFISH", "BLUELINE TILEFISH", 
    
    "ATLANTIC SALMON")
)
```

## Arguments

- input_survey_bio:

  Character string. Full path to the survey data with bio rds file

- input_survey_bio_epu:

  File path to survey data with bio data and epu (.rds format)

- input_static_lw_table:

  File path to length weight table from Miller 2013 (.rda format)

- inputPathSpecies:

  File path to species lookup table (.rds format)

- input_static_length_convert:

  File path to length conversion table (.rda format)

- species2include:

  Character vector of species to include (default: commonly surveyed
  species)

## Value

a combined productivity anomaly data set

## Examples

``` r
if (FALSE) { # \dontrun{
create_productivity_anomaly(
  input_survey_bio = "survey_bio.rds",
  input_survey_bio_epu = "survey_bio_epu.rds",
  input_static_lw_table = "lw_table.rda",
  input_static_length_convert = "df_lconv.rda",
  inputPathSpecies = "species_lookup.rds"
)
} # }
```
