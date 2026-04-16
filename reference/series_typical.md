# Typical price: (High + Low + Close) / 3

Returns a function that computes the typical price `(H + L + C) / 3`.

## Usage

``` r
series_typical(high = "high", low = "low", close = "close")
```

## Arguments

- high:

  String. High column name. Default: `"high"`.

- low:

  String. Low column name. Default: `"low"`.

- close:

  String. Close column name. Default: `"close"`.

## Value

A function `function(dt) -> numeric` of length `nrow(dt)`.

## Examples

``` r
dt <- data.table::data.table(high = c(10, 20), low = c(8, 18), close = c(9, 19))
fn <- series_typical()
fn(dt)
#> [1]  9 19
```
