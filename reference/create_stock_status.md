# Creates the stock status data frame for the SOE

This function creates a stock status data frame that is formatted for
inclusion in the `ecodata` R package.

## Usage

``` r
create_stock_status(data, decode)
```

## Arguments

- data:

  the stock status data frame, typically from
  [`stocksmart::stockAssessmentSummary`](https://noaa-edab.github.io/stocksmart/reference/stockAssessmentSummary.html)

- decode:

  a data frame that matches the stock names with a code abbreviation to
  use in plotting, typically read from a CSV file. If set to `FALSE`,
  the function will return a data frame in a less processed form, like
  Sarah used to provide in the `assess.csv` file.

## Value

a tibble
