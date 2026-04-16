test_that("invalid x emits error mentioning 'x'", {
  tms <- make_times(5L)
  expect_error(
    roll_nonlinear_one("not numeric", tms, window_size = 2L,
                       stride = 1L, min_obs = 2L),
    regexp = "x"
  )
})

test_that("x too short emits error mentioning 'x'", {
  tms <- make_times(1L)
  expect_error(
    roll_nonlinear_one(1.0, tms, window_size = 2L, stride = 1L, min_obs = 2L),
    regexp = "x"
  )
})

test_that("times length mismatch emits error mentioning 'times'", {
  x   <- make_ar1(10L)
  tms <- make_times(9L)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 1L, min_obs = 5L),
    regexp = "times"
  )
})

test_that("times wrong class emits error mentioning 'times'", {
  x   <- make_ar1(10L)
  tms <- letters[seq_len(10L)]
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 1L, min_obs = 5L),
    regexp = "times"
  )
})

test_that("non-increasing times emits error mentioning 'times'", {
  x   <- make_ar1(10L)
  tms <- rev(make_times(10L))
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 1L, min_obs = 5L),
    regexp = "times"
  )
})

test_that("unknown metric emits error mentioning 'metrics'", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, metrics = "bogus", window_size = 5L,
                       stride = 1L, min_obs = 5L),
    regexp = "metrics"
  )
})

test_that("empty metrics character emits error mentioning 'metrics'", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, metrics = character(0L), window_size = 5L,
                       stride = 1L, min_obs = 5L),
    regexp = "metrics"
  )
})

test_that("invalid window_size emits error mentioning 'window_size'", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 0L, stride = 1L, min_obs = 1L),
    regexp = "window_size"
  )
})

test_that("invalid stride emits error mentioning 'stride'", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 0L, min_obs = 3L),
    regexp = "stride"
  )
})

test_that("invalid min_obs emits error mentioning 'min_obs'", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 1L, min_obs = 0L),
    regexp = "min_obs"
  )
})

test_that("min_obs > window_size emits error mentioning 'min_obs'", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 1L, min_obs = 6L),
    regexp = "min_obs"
  )
})

test_that("invalid on_error emits an error", {
  x   <- make_ar1(10L)
  tms <- make_times(10L)
  expect_error(
    roll_nonlinear_one(x, tms, window_size = 5L, stride = 1L, min_obs = 5L,
                       on_error = "unknown"),
    regexp = "on_error|arg"
  )
})
