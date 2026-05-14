# create the bennet indicator

create the bennet indicator

## Usage

``` r
create_bennet(inputPathBennet, inputPathSpecies)
```

## Arguments

- inputPathBennet:

  Character string. Full path to the comland data pull rds file

- inputPathSpecies:

  Character string. Full path to the species list data pull rds file

## Value

ecodata::bennet data frame

History, R code to construct Bennet Indicator for Ecosystem Project
Author: John Walden Date: October 4, 2017

Revised January 18, 2018 to calculate the indicator relative to average
conditions during each time period.

Modified by Sean Lucey 12/16/2019

For 2019 report on standardized to 2015 as base year rather than average

Revised October 24, 2023. The year 1985, which is the first year in the
series is the base year. This may need to be modified in the future if
the length of the time series changes

Revised by John Walden 01/11/2024 to document changes and post final
version to Github

Revised by ABeet 10/2024 for 2025 report
