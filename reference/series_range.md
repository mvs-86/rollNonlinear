# Price range: High - Low

Returns a function that computes `High - Low`.

## Usage

``` r
series_range(high = "high", low = "low")
```

## Arguments

- high:

  String. High column name. Default: `"high"`.

- low:

  String. Low column name. Default: `"low"`.

## Value

A function `function(dt) -> numeric` of length `nrow(dt)`.

## Examples

``` r
dt <- data.table::data.table(high = c(10, 20), low = c(8, 18))
fn <- series_range()
fn(dt)
#> [1] 2 2
```
