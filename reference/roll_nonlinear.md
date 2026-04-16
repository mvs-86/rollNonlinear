# Rolling nonlinear metrics for multiple symbols

Computes rolling window nonlinear time-series metrics for one or more
symbols. Splits the input `data` by `symbol_col`, resolves the series
via `resolve_series()`, and calls
[`roll_nonlinear_one()`](https://rollnonlinear.github.io/rollNonlinear/reference/roll_nonlinear_one.md)
on each chunk sequentially.

## Usage

``` r
roll_nonlinear(
  data,
  metrics = c("hurst", "dfa"),
  window_size = 100L,
  stride = 1L,
  series = "close",
  symbol_col = "symbol",
  time_col = "time",
  min_obs = window_size,
  metric_args = list(),
  on_error = c("warn_na", "stop", "silent_na"),
  parallel = FALSE,
  n_workers = NULL
)
```

## Arguments

- data:

  A `data.table` (or `data.frame`, which will be coerced). Must contain
  at least `symbol_col`, `time_col`, and the column(s) implied by
  `series`.

- metrics:

  Character vector of metric names. Must be a subset of
  [`available_metrics()`](https://rollnonlinear.github.io/rollNonlinear/reference/available_metrics.md).
  Default: `c("hurst", "dfa")`.

- window_size:

  Positive integer. Number of observations per window. Default: `100L`.

- stride:

  Positive integer. Step between window anchors. Default: `1L`.

- series:

  Either a string (column name) or a function `function(dt) -> numeric`
  returning the series to analyse. Built-in constructors:
  [`series_close()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_close.md),
  [`series_log_returns()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_log_returns.md),
  [`series_typical()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_typical.md),
  [`series_range()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_range.md).
  Default: `"close"`.

- symbol_col:

  String. Name of the symbol grouping column. Default: `"symbol"`.

- time_col:

  String. Name of the time column. Must be strictly increasing *within
  each symbol*. Default: `"time"`.

- min_obs:

  Positive integer (`<= window_size`). Minimum observations required per
  window. Default: `window_size`.

- metric_args:

  Named list of per-metric argument overrides. Names must be a subset of
  `metrics`. Default: [`list()`](https://rdrr.io/r/base/list.html).

- on_error:

  Error-handling policy passed to
  [`roll_nonlinear_one()`](https://rollnonlinear.github.io/rollNonlinear/reference/roll_nonlinear_one.md).
  One of `"warn_na"` (default), `"stop"`, or `"silent_na"`.

- parallel:

  Logical. When `TRUE` and more than one symbol is present, execution is
  dispatched to local `mirai` daemons. If `mirai` is not available,
  falls back to sequential with a warning. Default: `FALSE`.

- n_workers:

  `NULL` (default) or a positive integer. Number of local `mirai`
  daemons to start when none are already running. `NULL` uses
  `parallelly::availableCores(omit = 1L)`. Ignored when
  `parallel = FALSE` or when daemons are already running.

## Value

A `data.table` keyed by `(symbol_col, time_col)` with one row per valid
rolling window per symbol and columns:

- `<symbol_col>`:

  Symbol identifier.

- `time`:

  Anchor time (`times[end]`).

- `window_start_idx`:

  Integer. Window start index.

- `window_end_idx`:

  Integer. Window end index.

- `<metric columns>`:

  One or more numeric columns per metric.

## Examples

``` r
# \donttest{
dt <- data.table::data.table(
  symbol = rep(c("A", "B"), each = 200L),
  time   = c(
    seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = 200L),
    seq(as.POSIXct("2024-06-01 09:30:00", tz = "UTC"), by = 60, length.out = 200L)
  ),
  close  = c(cumsum(rnorm(200L)), cumsum(rnorm(200L)))
)
out <- roll_nonlinear(dt, metrics = "hurst", window_size = 100L, stride = 20L)
#> ℹ Rolling 1 metric across 2 symbols (sequential mode).
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo 
print(out)
#> Key: <symbol, time>
#>     symbol                time window_start_idx window_end_idx     hurst
#>     <char>              <POSc>            <int>          <int>     <num>
#>  1:      A 2024-01-02 11:09:00                1            100 0.2999774
#>  2:      A 2024-01-02 11:29:00               21            120 0.3969799
#>  3:      A 2024-01-02 11:49:00               41            140 0.5668257
#>  4:      A 2024-01-02 12:09:00               61            160 0.5401414
#>  5:      A 2024-01-02 12:29:00               81            180 0.5602830
#>  6:      A 2024-01-02 12:49:00              101            200 0.1075465
#>  7:      B 2024-06-01 11:09:00                1            100 0.3890272
#>  8:      B 2024-06-01 11:29:00               21            120 0.6289509
#>  9:      B 2024-06-01 11:49:00               41            140 0.5613819
#> 10:      B 2024-06-01 12:09:00               61            160 0.1141574
#> 11:      B 2024-06-01 12:29:00               81            180 0.2102351
#> 12:      B 2024-06-01 12:49:00              101            200 0.4184707
# }
```
