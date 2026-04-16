# Rolling nonlinear metrics for a single symbol

Computes rolling window nonlinear time-series metrics for a single price
(or any numeric) series using sequential execution.

## Usage

``` r
roll_nonlinear_one(
  x,
  times,
  metrics = c("hurst", "dfa"),
  window_size = 100L,
  stride = 1L,
  min_obs = window_size,
  on_error = c("warn_na", "stop", "silent_na"),
  metric_args = list()
)
```

## Arguments

- x:

  Numeric vector. The time series values (NAs allowed in the series;
  handling is per-window based on `on_error`).

- times:

  A strictly-increasing vector of timestamps corresponding to each
  element of `x`. Must be `POSIXct`, `Date`, or integer.

- metrics:

  Character vector of metric names to compute. Must be a subset of
  [`available_metrics()`](https://rollnonlinear.github.io/rollNonlinear/reference/available_metrics.md).
  Defaults to all available metrics.

- window_size:

  Positive integer. Number of observations per rolling window. Default:
  `100L`.

- stride:

  Positive integer. Step between consecutive window anchors. Default:
  `1L`.

- min_obs:

  Positive integer (\<= `window_size`). Minimum observations required
  for a window to be computed. Default: `window_size` (fully formed
  windows only).

- on_error:

  Error-handling policy for metric computation failures (including
  windows containing NAs):

  `"warn_na"`

  :   Emit a deduplicated
      [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
      and fill with `NA_real_` (default).

  `"stop"`

  :   Re-throw via
      [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
      including window indices.

  `"silent_na"`

  :   Fill with `NA_real_` silently.

- metric_args:

  Named list of per-metric argument overrides, keyed by metric name.
  Each element is passed as `.args` to the metric wrapper (merged with
  the metric's registered defaults via
  [`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html)).
  Names must be a subset of `metrics`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A keyed `data.table` (keyed by `time`) with one row per valid rolling
window and the following columns:

- `time`:

  Anchor time (`times[end]`).

- `window_start_idx`:

  Integer. 1-based start index of the window.

- `window_end_idx`:

  Integer. 1-based end index (anchor).

- `hurst`:

  Numeric. Hurst exponent (if `"hurst"` in `metrics`).

- `dfa_alpha`:

  Numeric. DFA scaling exponent (if `"dfa"` in `metrics`).

The schema is stable even when all metric values are `NA`. Returns a
0-row `data.table` with the same schema when there are no valid windows.

## Examples

``` r
# \donttest{
set.seed(1L)
n   <- 300L
x   <- cumsum(rnorm(n))
tms <- seq(
  as.POSIXct("2024-01-02 09:30:00", tz = "UTC"),
  by = 60,
  length.out = n
)
out <- roll_nonlinear_one(x, tms, window_size = 128L, stride = 32L)
print(out)
#> Key: <time>
#>                   time window_start_idx window_end_idx     hurst dfa_alpha
#>                 <POSc>            <int>          <int>     <num>     <num>
#> 1: 2024-01-02 11:37:00                1            128 0.2707119  1.270712
#> 2: 2024-01-02 12:09:00               33            160 0.5185714  1.518571
#> 3: 2024-01-02 12:41:00               65            192 0.6034170  1.603417
#> 4: 2024-01-02 13:13:00               97            224 0.8362714  1.836271
#> 5: 2024-01-02 13:45:00              129            256 0.8115970  1.811597
#> 6: 2024-01-02 14:17:00              161            288 0.7208838  1.720884
# }
```
