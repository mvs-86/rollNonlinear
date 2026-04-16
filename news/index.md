# Changelog

## rollNonlinear 0.1.0

- Initial release.
- Rolling nonlinear metrics (Hurst, DFA, sample/approximate entropy,
  Lyapunov, correlation dimension) on intraday OHLC data.
- Multi-symbol input with optional parallel execution via mirai.
- Extensible metric registry via
  [`register_metric()`](https://rollnonlinear.github.io/rollNonlinear/reference/register_metric.md).
- Built-in series transforms:
  [`series_close()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_close.md),
  [`series_log_returns()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_log_returns.md),
  [`series_typical()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_typical.md),
  [`series_range()`](https://rollnonlinear.github.io/rollNonlinear/reference/series_range.md).
- Daemon lifecycle helpers:
  [`rn_daemons()`](https://rollnonlinear.github.io/rollNonlinear/reference/rn_daemons.md),
  [`rn_stop_daemons()`](https://rollnonlinear.github.io/rollNonlinear/reference/rn_stop_daemons.md).
