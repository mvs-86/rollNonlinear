# ---------------------------------------------------------------------------
# Tests for rn_plot()
# ---------------------------------------------------------------------------

# These tests require ggplot2 (and patchwork for type="panel").
# They are skipped if those packages are not installed.

skip_if_not_installed("ggplot2")

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

n <- 200L
dt_multi <- data.table::data.table(
  symbol = rep(c("A", "B"), each = n),
  time   = c(make_times(n), make_times(n)),
  close  = c(cumsum(make_ar1(n)), cumsum(make_ar1(n, phi = 0.5, seed = 2L))) + 100
)

result_multi <- roll_nonlinear(
  dt_multi,
  metrics     = c("hurst", "dfa"),
  window_size = 64L,
  stride      = 16L
)

result_single <- result_multi[symbol == "A"]

dt_single <- dt_multi[symbol == "A"]

# ---------------------------------------------------------------------------
# type = "time_series"
# ---------------------------------------------------------------------------

test_that("time_series returns a ggplot for multi-symbol, multi-metric", {
  p <- rn_plot(result_multi, type = "time_series")
  expect_s3_class(p, "ggplot")
})

test_that("time_series returns a ggplot for single metric", {
  p <- rn_plot(result_multi, metrics = "hurst", type = "time_series")
  expect_s3_class(p, "ggplot")
})

test_that("time_series applies regime_thresholds = 'auto' without error", {
  p <- rn_plot(result_multi, metrics = "hurst", type = "time_series",
               regime_thresholds = "auto")
  expect_s3_class(p, "ggplot")
})

test_that("time_series applies custom regime_thresholds without error", {
  p <- rn_plot(result_multi, metrics = "hurst", type = "time_series",
               regime_thresholds = list(hurst = list(lo = 0.4, hi = 0.6)))
  expect_s3_class(p, "ggplot")
})

test_that("time_series works for single-symbol result (no symbol column)", {
  res_one <- roll_nonlinear(
    dt_single,
    metrics     = "hurst",
    window_size = 64L,
    stride      = 16L
  )
  p <- rn_plot(res_one, type = "time_series")
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# type = "symbols"
# ---------------------------------------------------------------------------

test_that("symbols returns a ggplot", {
  p <- rn_plot(result_multi, metrics = "hurst", type = "symbols")
  expect_s3_class(p, "ggplot")
})

test_that("symbols aborts when more than one metric supplied", {
  expect_error(
    rn_plot(result_multi, metrics = c("hurst", "dfa_alpha"), type = "symbols"),
    "exactly one metric"
  )
})

test_that("symbols applies regime_thresholds without error", {
  p <- rn_plot(result_multi, metrics = "dfa_alpha", type = "symbols",
               regime_thresholds = "auto")
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# type = "distribution"
# ---------------------------------------------------------------------------

test_that("distribution returns a ggplot for multiple metrics", {
  p <- rn_plot(result_multi, type = "distribution")
  expect_s3_class(p, "ggplot")
})

test_that("distribution returns a ggplot for single metric", {
  p <- rn_plot(result_multi, metrics = "hurst", type = "distribution")
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# type = "panel"
# ---------------------------------------------------------------------------

test_that("panel returns a patchwork object", {
  skip_if_not_installed("patchwork")
  p <- rn_plot(result_single, type = "panel",
               original_data = dt_single, title = "Symbol A")
  expect_s3_class(p, "patchwork")
})

test_that("panel aborts when original_data is missing", {
  expect_error(
    rn_plot(result_single, type = "panel"),
    "original_data"
  )
})

test_that("panel aborts for multi-symbol result", {
  expect_error(
    rn_plot(result_multi, type = "panel", original_data = dt_multi),
    "single-symbol"
  )
})

test_that("panel aborts when price_col not in original_data", {
  expect_error(
    rn_plot(result_single, type = "panel",
            original_data = dt_single, price_col = "open"),
    "not found"
  )
})

# ---------------------------------------------------------------------------
# Argument validation
# ---------------------------------------------------------------------------

test_that("invalid type aborts with match.arg error", {
  expect_error(rn_plot(result_multi, type = "pie"))
})

test_that("unknown metrics abort with informative error", {
  expect_error(
    rn_plot(result_multi, metrics = "nonexistent_col"),
    "not found"
  )
})

test_that("regime_thresholds with wrong type aborts", {
  expect_error(
    rn_plot(result_multi, metrics = "hurst", regime_thresholds = 42),
    "regime_thresholds"
  )
})

test_that("NULL metrics auto-detects columns", {
  p <- rn_plot(result_multi, metrics = NULL, type = "time_series")
  expect_s3_class(p, "ggplot")
})
