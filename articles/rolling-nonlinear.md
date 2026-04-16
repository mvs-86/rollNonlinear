# Rolling Nonlinear Time-Series Metrics

``` r
library(rollNonlinear)
library(data.table)
```

## 1. Introduction

`rollNonlinear` computes rolling-window nonlinear time-series metrics on
intraday OHLC data. It is designed for quantitative research workflows
where you need to characterise the *dynamical regime* of a price series
— e.g., trend persistence (Hurst), scaling behaviour (DFA), complexity
(sample/ approximate entropy), or chaotic divergence (Lyapunov,
correlation dimension) — over time rather than at a single point.

Key features:

- **Six built-in metrics**: `hurst`, `dfa`, `sample_entropy`,
  `approx_entropy`, `lyapunov`, `corr_dim`.
- **Extensible registry**: add custom metrics via
  [`register_metric()`](https://rollnonlinear.github.io/rollNonlinear/reference/register_metric.md).
- **Built-in series transforms**:
  [`series_close()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_close.md),
  [`series_log_returns()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_log_returns.md),
  [`series_typical()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_typical.md),
  [`series_range()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_range.md),
  or any custom `function(dt)`.
- **Multi-symbol dispatch**: accepts a data.table with a `symbol` column
  and processes each symbol independently.
- **Optional parallel execution** via `mirai` for multi-symbol
  workloads.

## 2. Input format

[`roll_nonlinear()`](https://rollnonlinear.github.io/rollNonlinear/reference/roll_nonlinear.md)
expects a `data.table` (or `data.frame`, which is coerced) with at
minimum:

| Column          | Type                     | Notes                                    |
|-----------------|--------------------------|------------------------------------------|
| `symbol`        | character                | Symbol identifier                        |
| `time`          | POSIXct / Date / integer | Strictly increasing *within* each symbol |
| price column(s) | numeric                  | Determined by the `series` argument      |

``` r
set.seed(1L)
n <- 300L

dt <- data.table(
  symbol = rep(c("AAPL", "MSFT"), each = n),
  time   = c(
    seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = n),
    seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = n)
  ),
  open   = c(rep(150, n), rep(370, n)),
  high   = c(cumsum(rnorm(n, 0, 0.3)) + 151, cumsum(rnorm(n, 0, 0.3)) + 371),
  low    = c(cumsum(rnorm(n, 0, 0.3)) + 149, cumsum(rnorm(n, 0, 0.3)) + 369),
  close  = c(cumsum(rnorm(n)), cumsum(rnorm(n))) + c(rep(150, n), rep(370, n))
)

head(dt)
#>    symbol                time  open     high      low    close
#>    <char>              <POSc> <num>    <num>    <num>    <num>
#> 1:   AAPL 2024-01-02 09:30:00   150 150.8121 148.8977 148.4430
#> 2:   AAPL 2024-01-02 09:31:00   150 150.8672 149.3484 150.3661
#> 3:   AAPL 2024-01-02 09:32:00   150 150.6165 149.5069 148.5093
#> 4:   AAPL 2024-01-02 09:33:00   150 151.0951 149.6696 146.4032
#> 5:   AAPL 2024-01-02 09:34:00   150 151.1939 149.6286 147.1008
#> 6:   AAPL 2024-01-02 09:35:00   150 150.9478 149.2875 148.0083
```

## 3. Basic usage — single symbol, Hurst + DFA

``` r
single <- dt[symbol == "AAPL"]

out <- roll_nonlinear(
  single,
  metrics     = c("hurst", "dfa"),
  window_size = 128L,
  stride      = 16L
)
#> ℹ Rolling 2 metrics across 1 symbol (sequential mode).
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo

head(out)
#> Key: <symbol, time>
#>    symbol                time window_start_idx window_end_idx     hurst
#>    <char>              <POSc>            <int>          <int>     <num>
#> 1:   AAPL 2024-01-02 11:37:00                1            128 0.7679974
#> 2:   AAPL 2024-01-02 11:53:00               17            144 0.7272546
#> 3:   AAPL 2024-01-02 12:09:00               33            160 0.7534161
#> 4:   AAPL 2024-01-02 12:25:00               49            176 0.7979166
#> 5:   AAPL 2024-01-02 12:41:00               65            192 0.6861397
#> 6:   AAPL 2024-01-02 12:57:00               81            208 0.8831254
#>    dfa_alpha
#>        <num>
#> 1:  1.767997
#> 2:  1.727255
#> 3:  1.753416
#> 4:  1.797917
#> 5:  1.686140
#> 6:  1.883125
```

Each row corresponds to one rolling window. `window_start_idx` and
`window_end_idx` are 1-based row indices into the input symbol’s data.

## 4. Multi-symbol dispatch

Pass the full multi-symbol table:

``` r
out_multi <- roll_nonlinear(
  dt,
  metrics     = c("hurst", "dfa"),
  window_size = 128L,
  stride      = 32L
)
#> ℹ Rolling 2 metrics across 2 symbols (sequential mode).

out_multi[, .(symbol, time, hurst, dfa_alpha)][1:6]
#> Key: <symbol, time>
#>    symbol                time     hurst dfa_alpha
#>    <char>              <POSc>     <num>     <num>
#> 1:   AAPL 2024-01-02 11:37:00 0.7679974  1.767997
#> 2:   AAPL 2024-01-02 12:09:00 0.7534161  1.753416
#> 3:   AAPL 2024-01-02 12:41:00 0.6861397  1.686140
#> 4:   AAPL 2024-01-02 13:13:00 0.4350000  1.435000
#> 5:   AAPL 2024-01-02 13:45:00 0.3696960  1.369696
#> 6:   AAPL 2024-01-02 14:17:00 0.4732562  1.473256
```

The result is keyed by `(symbol, time)`.

## 5. Custom series transforms

### Built-in: log-returns

``` r
lr_out <- roll_nonlinear(
  dt[symbol == "AAPL"],
  series      = series_log_returns(),
  metrics     = "hurst",
  window_size = 128L,
  stride      = 32L
)
#> ℹ Rolling 1 metric across 1 symbol (sequential mode).
#> Warning: Metric "hurst" failed (window [1, 128]); filling `NA`.
#> ✖ Window contains NA values.
head(lr_out[, .(time, hurst)])
#>                   time     hurst
#>                 <POSc>     <num>
#> 1: 2024-01-02 11:37:00        NA
#> 2: 2024-01-02 12:09:00 0.5111897
#> 3: 2024-01-02 12:41:00 0.4844938
#> 4: 2024-01-02 13:13:00 0.6716737
#> 5: 2024-01-02 13:45:00 0.7281123
#> 6: 2024-01-02 14:17:00 0.7754734
```

### Typical price

``` r
tp_out <- roll_nonlinear(
  dt[symbol == "AAPL"],
  series      = series_typical(),
  metrics     = "hurst",
  window_size = 128L,
  stride      = 32L
)
#> ℹ Rolling 1 metric across 1 symbol (sequential mode).
head(tp_out[, .(time, hurst)])
#>                   time     hurst
#>                 <POSc>     <num>
#> 1: 2024-01-02 11:37:00 0.7339985
#> 2: 2024-01-02 12:09:00 0.7044117
#> 3: 2024-01-02 12:41:00 0.5857700
#> 4: 2024-01-02 13:13:00 0.6184259
#> 5: 2024-01-02 13:45:00 0.4555913
#> 6: 2024-01-02 14:17:00 0.5099675
```

### User-defined transform

Any function `function(dt) -> numeric` is accepted:

``` r
log_close <- function(dt_chunk) log(dt_chunk[["close"]])

out_logclose <- roll_nonlinear(
  dt[symbol == "AAPL"],
  series      = log_close,
  metrics     = "hurst",
  window_size = 128L,
  stride      = 32L
)
#> ℹ Rolling 1 metric across 1 symbol (sequential mode).
head(out_logclose[, .(time, hurst)])
#>                   time     hurst
#>                 <POSc>     <num>
#> 1: 2024-01-02 11:37:00 0.7794158
#> 2: 2024-01-02 12:09:00 0.7579486
#> 3: 2024-01-02 12:41:00 0.6929557
#> 4: 2024-01-02 13:13:00 0.4284412
#> 5: 2024-01-02 13:45:00 0.3694222
#> 6: 2024-01-02 14:17:00 0.4664336
```

## 6. Metric arguments

Use `metric_args` to override per-metric defaults:

``` r
ent_out <- roll_nonlinear(
  dt[symbol == "AAPL"],
  metrics     = c("sample_entropy", "approx_entropy"),
  window_size = 100L,
  stride      = 20L,
  metric_args = list(
    sample_entropy = list(dimension = 2L, r_frac = 0.15),
    approx_entropy = list(dimension = 2L, r_frac = 0.20)
  )
)
#> ℹ Rolling 2 metrics across 1 symbol (sequential mode).
head(ent_out[, .(time, sample_entropy, approx_entropy)])
#>                   time sample_entropy approx_entropy
#>                 <POSc>          <num>          <num>
#> 1: 2024-01-02 11:09:00      0.6733446      0.4702413
#> 2: 2024-01-02 11:29:00      0.5453863      0.3573229
#> 3: 2024-01-02 11:49:00      0.4759168      0.3488843
#> 4: 2024-01-02 12:09:00      0.5796090      0.4255343
#> 5: 2024-01-02 12:29:00      0.6505876      0.4544956
#> 6: 2024-01-02 12:49:00      0.4568871      0.3062640
```

> **Note:** `r_frac` scales the tolerance `r` as `r_frac * sd(x)` per
> window (volatility-normalised). For volatility-sensitive analysis,
> pass `r` explicitly instead.

## 7. Parallel execution

For workloads with many symbols, `parallel = TRUE` dispatches each
symbol to a local `mirai` daemon:

``` r
# One-time setup (optional — rollNonlinear manages daemons automatically)
rn_daemons(4L)

out_par <- roll_nonlinear(
  dt,
  metrics     = c("hurst", "dfa"),
  window_size = 128L,
  stride      = 32L,
  parallel    = TRUE
)

rn_stop_daemons()
```

### Daemon management

| Scenario                               | Behaviour                                                                                                     |
|----------------------------------------|---------------------------------------------------------------------------------------------------------------|
| No daemons running                     | Package starts daemons (using `parallelly::availableCores(omit=1)` by default), then tears them down on exit. |
| Daemons already running (user-managed) | Package uses existing daemons and does **not** stop them.                                                     |

Use `n_workers` to override the automatic core count:

``` r
roll_nonlinear(dt, parallel = TRUE, n_workers = 2L)
```

### When parallel pays off

Parallel execution has setup overhead (~100–500 ms). Rule of thumb:

- **Worth it**: ≥ 10 symbols *and* ≥ 1 000 bars each.
- **Not worth it**: small datasets (\< ~10 symbols or \< ~1 000 bars
  each) — sequential is usually faster.

## 8. Extending the package — `register_metric()`

Register a custom metric that returns the mean absolute deviation:

``` r
register_metric(
  name    = "mad_metric",
  fn      = function(x, .args = list()) {
    c(mad_metric = mean(abs(x - mean(x, na.rm = TRUE)), na.rm = TRUE))
  },
  outputs = "mad_metric"
)

available_metrics()
#> [1] "approx_entropy" "corr_dim"       "dfa"            "hurst"         
#> [5] "lyapunov"       "mad_metric"     "sample_entropy"

mad_out <- roll_nonlinear(
  dt[symbol == "AAPL"],
  metrics     = "mad_metric",
  window_size = 64L,
  stride      = 16L
)
#> ℹ Rolling 1 metric across 1 symbol (sequential mode).
head(mad_out[, .(time, mad_metric)])
#>                   time mad_metric
#>                 <POSc>      <num>
#> 1: 2024-01-02 10:33:00   3.006953
#> 2: 2024-01-02 10:49:00   1.809379
#> 3: 2024-01-02 11:05:00   3.966879
#> 4: 2024-01-02 11:21:00   7.093376
#> 5: 2024-01-02 11:37:00   6.786955
#> 6: 2024-01-02 11:53:00   3.537289
```

Registered metrics work identically under both sequential and parallel
execution.

## Caveats and known limitations

- **Lyapunov & correlation dimension** require substantial data (≥ 500–1
  000 bars per window) to produce meaningful estimates. Short windows
  yield unreliable values; use them with `\donttest{}` guards in your
  scripts.
- **Sample/approximate entropy** with `r_frac` is volatility-normalised
  by default. Pass `r` explicitly if you need volatility-sensitive
  comparisons.
- **Parallel overhead**: for small datasets, sequential mode is faster.
  Prefer `parallel = TRUE` for wide, large-bar symbol sets.
