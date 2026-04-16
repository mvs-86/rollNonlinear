# Extract a single column as the series

Returns a function that extracts column `col` from a `data.table` or
data.frame as a numeric vector.

## Usage

``` r
series_close(col = "close")
```

## Arguments

- col:

  String. Column name. Default: `"close"`.

## Value

A function `function(dt) -> numeric`.

## Examples

``` r
dt <- data.table::data.table(close = c(1.0, 2.0, 3.0))
fn <- series_close()
fn(dt)
#> [1] 1 2 3
```
