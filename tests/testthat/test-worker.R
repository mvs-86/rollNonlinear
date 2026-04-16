test_that(".freeze_series works for string input", {
  spec <- rollNonlinear:::.freeze_series("close")
  expect_equal(spec, list(kind = "col", value = "close"))
})

test_that(".freeze_series works for function input", {
  fn   <- series_log_returns()
  spec <- rollNonlinear:::.freeze_series(fn)
  expect_equal(spec$kind, "fn")
  expect_true(is.function(spec$value))
  # Function is stored as-is (not stripped) so closure variables are preserved
  expect_identical(spec$value, fn)
})

test_that(".freeze_series aborts on invalid input", {
  expect_error(rollNonlinear:::.freeze_series(42), class = "rlang_error")
  expect_error(rollNonlinear:::.freeze_series(NULL), class = "rlang_error")
})

test_that(".strip_env replaces enclosing environment with baseenv()", {
  f <- function(x) x + 1L
  environment(f) <- globalenv()
  g <- rollNonlinear:::.strip_env(f)
  expect_identical(environment(g), baseenv())
})

test_that(".thaw_series reconstructs column extractor", {
  spec <- list(kind = "col", value = "close")
  fn   <- rollNonlinear:::.thaw_series(spec)
  dt   <- data.table::data.table(close = 1:5)
  expect_equal(fn(dt), 1:5)
})

test_that(".thaw_series reconstructs function as-is", {
  f    <- function(dt) as.numeric(dt[["close"]]) * 2
  spec <- list(kind = "fn", value = f)
  fn   <- rollNonlinear:::.thaw_series(spec)
  dt   <- data.table::data.table(close = 1:3)
  expect_equal(fn(dt), c(2, 4, 6))
})

test_that("round-trip .freeze/.thaw works for each built-in transform", {
  dt <- data.table::data.table(
    close  = c(100, 102, 101, 103, 105),
    high   = c(103, 104, 103, 105, 107),
    low    = c( 99, 101, 100, 102, 104),
    open   = c(100, 101, 102, 100, 103)
  )

  transforms <- list(
    series_close(),
    series_typical(),
    series_range(),
    series_log_returns()
  )

  for (f in transforms) {
    expected <- f(dt)
    spec     <- rollNonlinear:::.freeze_series(f)
    fn       <- rollNonlinear:::.thaw_series(spec)
    expect_equal(fn(dt), expected)
  }
})

test_that(".worker_roll_one returns correct output in-process", {
  set.seed(42L)
  n  <- 120L
  dt <- data.table::data.table(
    symbol = "X",
    time   = make_times(n),
    close  = cumsum(rnorm(n))
  )

  spec <- rollNonlinear:::.freeze_series("close")

  result <- rollNonlinear:::.worker_roll_one(
    chunk       = dt,
    metrics     = c("hurst", "dfa"),
    window_size = 64L,
    stride      = 10L,
    series_spec = spec,
    time_col    = "time",
    symbol_col  = "symbol",
    min_obs     = 64L,
    metric_args = list(),
    on_error    = "warn_na"
  )

  expect_s3_class(result, "data.table")
  expect_true("symbol" %in% names(result))
  expect_true("hurst"  %in% names(result))
  expect_true("dfa_alpha" %in% names(result))
  expect_equal(unique(result[["symbol"]]), "X")
})

test_that(".worker_roll_one symbol column is prepended", {
  set.seed(7L)
  n  <- 100L
  dt <- data.table::data.table(
    symbol = "ABC",
    time   = make_times(n),
    close  = cumsum(rnorm(n))
  )
  spec   <- rollNonlinear:::.freeze_series("close")
  result <- rollNonlinear:::.worker_roll_one(
    chunk       = dt,
    metrics     = "hurst",
    window_size = 64L,
    stride      = 5L,
    series_spec = spec,
    time_col    = "time",
    symbol_col  = "symbol",
    min_obs     = 64L,
    metric_args = list(),
    on_error    = "warn_na"
  )
  expect_equal(names(result)[[1L]], "symbol")
})
