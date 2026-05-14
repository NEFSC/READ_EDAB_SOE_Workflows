# Get commercial data set to store somewhere

Pulls commercial data in multiple formats required for indicator
generation Menhaden data are removed from the commercial data pull since
they are incomplete

## Usage

``` r
get_commercial_data(channel)
```

## Arguments

- channel:

  an Object inherited from DBIConnection-class.

## Value

A list of commercial data pulls

## Examples

``` r
if (FALSE) { # \dontrun{
channel <- dbutils::connect_to_database("server","user")
rawData <- get_commercial_data(channel)
} # }
```
