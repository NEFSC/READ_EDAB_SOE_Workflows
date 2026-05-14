# Get survey data using survdat

Pulls survey data in format required for indicator generation

## Usage

``` r
get_survey_data(channel)
```

## Arguments

- channel:

  an Object inherited from DBIConnection-class. . This object is used to
  connect to communicate with the database engine.

## Value

A list of survey data pulls

## Examples

``` r
if (FALSE) { # \dontrun{
channel <- dbutils::connect_to_database("server",user)
rawData <- get_survey_data(channel)
} # }
```
