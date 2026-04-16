test_that("end-to-end: row count matches make_windows expectations", {
  n    <- 2000L
  ws   <- 256L
  st   <- 32L
  x    <- make_ar1(n)
  tms  <- make_times(n)
  out  <- roll_nonlinear_one(x, tms, window_size = ws, stride = st,
                             on_error = "silent_na")
  wins <- rollNonlinear:::make_windows(n, ws, st, ws)
  expect_equal(nrow(out), sum(wins$valid))
})

test_that("end-to-end: correct columns and types", {
  n   <- 500L
  ws  <- 100L
  x   <- make_ar1(n)
  tms <- make_times(n)
  out <- roll_nonlinear_one(x, tms, window_size = ws, stride = 20L,
                            on_error = "silent_na")
  expect_true(data.table::is.data.table(out))
  expect_true("time"             %in% names(out))
  expect_true("window_start_idx" %in% names(out))
  expect_true("window_end_idx"   %in% names(out))
  expect_true("hurst"            %in% names(out))
  expect_true("dfa_alpha"        %in% names(out))
  expect_type(out$window_start_idx, "integer")
  expect_type(out$window_end_idx,   "integer")
  expect_type(out$hurst,            "double")
  expect_type(out$dfa_alpha,        "double")
})

test_that("end-to-end: time column strictly increasing", {
  n   <- 500L
  x   <- make_ar1(n)
  tms <- make_times(n)
  out <- roll_nonlinear_one(x, tms, window_size = 100L, stride = 10L,
                            on_error = "silent_na")
  if (nrow(out) >= 2L) {
    expect_true(all(diff(as.numeric(out$time)) > 0))
  }
})

test_that("end-to-end: keyed by time", {
  n   <- 300L
  x   <- make_ar1(n)
  tms <- make_times(n)
  out <- roll_nonlinear_one(x, tms, window_size = 50L, stride = 5L,
                            on_error = "silent_na")
  expect_equal(data.table::key(out), "time")
})

test_that("on_error='stop' re-throws on NA window", {
  n   <- 200L
  x   <- make_ar1(n)
  x[[100L]] <- NA_real_  # inject NA in some window
  tms <- make_times(n)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 50L, stride = 1L,
                       on_error = "stop"),
    regexp = "NA"
  )
})

test_that("on_error='warn_na' produces warnings and NA rows on NA window", {
  n   <- 200L
  x   <- make_ar1(n)
  x[[100L]] <- NA_real_
  tms <- make_times(n)
  out <- NULL
  expect_warning(
    {out <- roll_nonlinear_one(x, tms, window_size = 50L, stride = 1L,
                               on_error = "warn_na")},
    regexp = "NA"
  )
  # Rows covering the NA should have NA metric values
  mask <- out[["window_start_idx"]] <= 100L & out[["window_end_idx"]] >= 100L
  bad_rows <- out[mask]
  expect_true(nrow(bad_rows) >= 1L)
  expect_true(all(is.na(bad_rows$hurst)))
})

test_that("on_error='silent_na' produces NA rows without warnings", {
  n   <- 200L
  x   <- make_ar1(n)
  x[[100L]] <- NA_real_
  tms <- make_times(n)
  expect_no_warning(
    out <- roll_nonlinear_one(x, tms, window_size = 50L, stride = 1L,
                              on_error = "silent_na")
  )
  mask <- out[["window_start_idx"]] <= 100L & out[["window_end_idx"]] >= 100L
  bad_rows <- out[mask]
  expect_true(all(is.na(bad_rows$hurst)))
})

test_that("empty-result path: length(x) < window_size returns 0-row dt", {
  x   <- make_ar1(50L)
  tms <- make_times(50L)
  out <- roll_nonlinear_one(x, tms, window_size = 100L, stride = 1L)
  expect_equal(nrow(out), 0L)
  expect_true(data.table::is.data.table(out))
  expect_true("time"             %in% names(out))
  expect_true("window_start_idx" %in% names(out))
  expect_true("window_end_idx"   %in% names(out))
  expect_true("hurst"            %in% names(out))
  expect_true("dfa_alpha"        %in% names(out))
  expect_equal(data.table::key(out), "time")
})

test_that("single-metric request returns stable schema", {
  n   <- 300L
  x   <- make_ar1(n)
  tms <- make_times(n)
  out <- roll_nonlinear_one(x, tms, metrics = "hurst",
                            window_size = 100L, stride = 20L,
                            on_error = "silent_na")
  expect_true("hurst" %in% names(out))
  expect_false("dfa_alpha" %in% names(out))
})
