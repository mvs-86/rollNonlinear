test_that("multi-symbol input produces correctly keyed output", {
  dt <- data.table::data.table(
    symbol = rep(c("A", "B"), each = 200L),
    time   = c(make_times(200L), make_times(200L, start = "2024-06-01")),
    close  = c(make_ar1(200L), make_ar1(200L, seed = 2L))
  )

  out <- roll_nonlinear(dt, metrics = "hurst", window_size = 100L,
                        stride = 20L)

  expect_s3_class(out, "data.table")
  expect_true(all(c("symbol", "time") %in% names(out)))
  expect_true(all(out[["symbol"]] %in% c("A", "B")))
  expect_true(data.table::haskey(out))
})

test_that("per-symbol results match individual processing", {
  dt <- data.table::data.table(
    symbol = rep(c("A", "B"), each = 200L),
    time   = c(make_times(200L), make_times(200L, start = "2024-06-01")),
    close  = c(make_ar1(200L), make_ar1(200L, seed = 2L))
  )

  combined <- roll_nonlinear(dt, metrics = "hurst", window_size = 100L,
                             stride = 20L)

  dt_a  <- dt[symbol == "A"]
  out_a <- roll_nonlinear(dt_a, metrics = "hurst", window_size = 100L,
                          stride = 20L)

  combined_a <- combined[symbol == "A"]
  data.table::setkey(combined_a, time)
  data.table::setkey(out_a, time)

  expect_equal(combined_a[["hurst"]], out_a[["hurst"]])
})

test_that("data.frame input is coerced with informational message", {
  df <- data.frame(
    symbol = rep("A", 200L),
    time   = make_times(200L),
    close  = make_ar1(200L),
    stringsAsFactors = FALSE
  )

  expect_message(
    roll_nonlinear(df, metrics = "hurst", window_size = 100L, stride = 20L),
    regexp = "coercing|data.table"
  )
})

test_that("parallel = TRUE emits a notice and runs sequentially", {
  dt <- data.table::data.table(
    symbol = rep("A", 200L),
    time   = make_times(200L),
    close  = make_ar1(200L)
  )

  expect_message(
    out <- roll_nonlinear(dt, metrics = "hurst", window_size = 100L,
                          stride = 20L, parallel = TRUE),
    regexp = "parallel|sequential"
  )
  expect_s3_class(out, "data.table")
})

test_that("non-monotonic time within a symbol aborts with informative error", {
  dt <- data.table::data.table(
    symbol = rep("A", 200L),
    time   = make_times(200L),
    close  = rnorm(200L)
  )
  # Break time ordering for symbol A at index 3
  dt[["time"]][[3L]] <- dt[["time"]][[1L]]

  expect_error(
    roll_nonlinear(dt, metrics = "hurst", window_size = 100L),
    class = "rlang_error"
  )
})

test_that("metric_args with r_frac overrides flows through correctly", {
  set.seed(1L)
  dt <- data.table::data.table(
    symbol = rep("A", 300L),
    time   = make_times(300L),
    close  = cumsum(rnorm(300L))
  )

  out1 <- roll_nonlinear(
    dt, metrics = "sample_entropy", window_size = 100L, stride = 50L,
    metric_args = list(sample_entropy = list(r_frac = 0.1))
  )
  out2 <- roll_nonlinear(
    dt, metrics = "sample_entropy", window_size = 100L, stride = 50L,
    metric_args = list(sample_entropy = list(r_frac = 0.5))
  )

  vals1 <- out1[["sample_entropy"]]
  vals2 <- out2[["sample_entropy"]]
  finite1 <- vals1[is.finite(vals1)]
  finite2 <- vals2[is.finite(vals2)]

  # At least some finite values must differ between the two r_frac settings
  if (length(finite1) > 0L && length(finite2) > 0L) {
    expect_false(isTRUE(all.equal(finite1, finite2)))
  }
})

test_that("min-length advisory warning fires for lyapunov with small window_size", {
  dt <- data.table::data.table(
    symbol = rep("A", 200L),
    time   = make_times(200L),
    close  = cumsum(rnorm(200L))
  )

  expect_warning(
    roll_nonlinear(dt, metrics = "lyapunov", window_size = 50L, stride = 25L),
    regexp = "lyapunov|window_size|recommend|200"
  )
})

test_that("missing required columns aborts with informative error", {
  dt <- data.table::data.table(
    symbol = rep("A", 200L),
    time   = make_times(200L),
    x      = rnorm(200L)
  )
  expect_error(
    roll_nonlinear(dt, metrics = "hurst", window_size = 100L, series = "close"),
    class = "rlang_error"
  )
})

test_that("unknown metric aborts", {
  dt <- data.table::data.table(
    symbol = rep("A", 5L),
    time   = make_times(5L),
    close  = rnorm(5L)
  )
  expect_error(
    roll_nonlinear(dt, metrics = "nonexistent_metric", window_size = 3L),
    class = "rlang_error"
  )
})
