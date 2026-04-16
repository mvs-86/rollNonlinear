# Log-returns of a price column

Returns a function that computes `diff(log(x))` on column `col`. The
first element of the returned vector is `NA_real_`.

## Usage

``` r
series_log_returns(col = "close")
```

## Arguments

- col:

  String. Column name. Default: `"close"`.

## Value

A function `function(dt) -> numeric` of length `nrow(dt)`.

## Examples

``` r
dt <- data.table::data.table(close = c(100, 101, 102, 103))
fn <- series_log_returns()
fn(dt)
#> [1]          NA 0.009950331 0.009852296 0.009756175
```
