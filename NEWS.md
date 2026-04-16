# rollNonlinear 0.1.0

* Initial release.
* Rolling nonlinear metrics (Hurst, DFA, sample/approximate entropy, Lyapunov,
  correlation dimension) on intraday OHLC data.
* Multi-symbol input with optional parallel execution via mirai.
* Extensible metric registry via `register_metric()`.
* Built-in series transforms: `series_close()`, `series_log_returns()`,
  `series_typical()`, `series_range()`.
* Daemon lifecycle helpers: `rn_daemons()`, `rn_stop_daemons()`.
