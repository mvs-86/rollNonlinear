# rollNonlinear: Rolling Nonlinear Time-Series Metrics Engine

Computes rolling window nonlinear time-series metrics (Hurst exponent,
Detrended Fluctuation Analysis, sample and approximate entropy, Lyapunov
exponent, correlation dimension) for one or more symbols in OHLC-style
data.tables. Provides an extensible metric registry via
[`register_metric()`](https://rollnonlinear.github.io/rollNonlinear/reference/register_metric.md),
built-in series transforms, and optional parallel execution via
[mirai](https://mirai.r-lib.org/reference/mirai.html) for multi-symbol
workloads.

## Author

**Maintainer**: rollNonlinear Authors <rollnonlinear@example.com>
