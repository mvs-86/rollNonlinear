# List available metrics

Returns the names of all metrics registered in the metric registry,
including built-in and any user-registered metrics.

## Usage

``` r
available_metrics()
```

## Value

A sorted character vector of metric names.

## Examples

``` r
available_metrics()
#> [1] "approx_entropy" "corr_dim"       "dfa"            "hurst"         
#> [5] "lyapunov"       "sample_entropy"
```
