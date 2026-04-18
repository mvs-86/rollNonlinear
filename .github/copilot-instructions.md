# Copilot instructions for `rollNonlinear`

## Build and test commands

### Package build and check

```bash
R CMD build .
R CMD check --no-manual --as-cran rollNonlinear_*.tar.gz
```

### Full local test suite

```bash
Rscript -e "testthat::test_local()"
```

### Run a single test file

```bash
Rscript -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-roll.R")'
Rscript -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-parallel.R")'
```

Prefer `test_file()` for targeted runs instead of a broad `test_local(filter = ...)` substring when you mean one file; `roll` also matches `test-parallel.R`.

### Build the pkgdown site

```bash
Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"
```

## High-level architecture

- `R/metrics-registry.R` defines an environment-backed metric registry. Registry entries are the contract for the engine: `fn`, `outputs`, `defaults`, and `min_len`.
- `R/zzz.R` populates the built-in metrics on package load. `register_metric()` is the supported extension point, so rolling code should discover metrics through the registry rather than hard-coding metric logic.
- `R/roll.R` is the single-series core. It validates inputs, builds rolling windows with `make_windows()`, evaluates each requested metric per valid window, applies `on_error`, and returns a schema-stable keyed `data.table`.
- `R/roll-by-symbol.R` is the multi-symbol dispatcher. It coerces input to `data.table`, splits by symbol, resolves the series specification, chooses sequential vs `mirai` execution, then row-binds and keys the per-symbol outputs.
- `R/parallel.R` and `R/worker.R` implement the parallel path. The main process freezes the series and metric specs, dispatches one job per symbol, and only tears down `mirai` daemons that it started itself.
- `R/transforms.R` keeps series extraction separate from rolling logic. Built-in `series_*()` helpers return `function(dt) -> numeric` closures for close, log returns, typical price, and range.
- `R/plots.R` is downstream of the engine: it expects the keyed result tables from `roll_nonlinear()` / `roll_nonlinear_one()` and optionally combines them with original price data for panel plots.

## Key conventions

- Use `checkmate` for validation and `cli::cli_abort()`, `cli::cli_warn()`, and `cli::cli_inform()` for user-facing conditions. Avoid introducing bare `stop()`, `warning()`, or `message()` in public paths.
- Preserve `data.table` outputs and their keys. Single-symbol results are keyed by `time`; multi-symbol results are keyed by `(symbol_col, time_col)`. Empty/all-`NA` paths still keep the requested metric columns.
- Metric wrappers must have the form `function(x, .args = list())` and return a named numeric vector whose names exactly match the registry `outputs`.
- Metric arguments are resolved by merging registry defaults with per-call overrides through `utils::modifyList()`. Keep new metrics compatible with that contract.
- Metric implementations should never plot. Existing nonlinearTseries wrappers force `do.plot = FALSE`, and numerical failures inside wrappers usually degrade to `NA_real_`; `roll_nonlinear_one()` owns the `warn_na` / `stop` / `silent_na` policy.
- `hurst` is intentionally derived from DFA `alpha` in `R/metrics.R`, not from a separate nonlinearTseries Hurst API.
- `sample_entropy` and `approx_entropy` are intentionally pure-R implementations in `R/metrics-entropy.R`. `lyapunov` and `corr_dim` pass `regression.range` to `estimate()`, not to the primary nonlinearTseries call.
- Time ordering is a hard invariant: `times` must be strictly increasing, and the multi-symbol path enforces that within each symbol chunk before rolling starts.
- Series helpers are designed to survive transport to workers. If you add a new built-in series transform, keep it as a lightweight `function(dt) -> numeric` closure without unnecessary captured state.
