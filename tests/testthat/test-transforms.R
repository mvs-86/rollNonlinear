test_that("series_close produces numeric vector of correct length", {
  dt <- data.table::data.table(close = c(1.0, 2.0, 3.0))
  fn <- series_close("close")

  result <- fn(dt)

  expect_true(is.numeric(result))
  expect_length(result, 3L)
  expect_equal(result, c(1.0, 2.0, 3.0))
})

test_that("series_close with non-default column name works", {
  dt <- data.table::data.table(price = c(10.0, 20.0))
  fn <- series_close("price")
  expect_equal(fn(dt), c(10.0, 20.0))
})

test_that("series_log_returns first element is NA_real_", {
  dt <- data.table::data.table(close = c(100, 101, 102, 103))
  fn <- series_log_returns()

  result <- fn(dt)

  expect_true(is.na(result[[1L]]))
  expect_true(is.numeric(result))
  expect_length(result, 4L)
})

test_that("series_log_returns subsequent values match diff(log(x))", {
  prices <- c(100, 101, 102, 103)
  dt     <- data.table::data.table(close = prices)
  fn     <- series_log_returns()

  result   <- fn(dt)
  expected <- diff(log(prices))

  expect_equal(result[-1L], expected)
})

test_that("series_typical produces (H+L+C)/3 of correct length", {
  dt <- data.table::data.table(
    high  = c(10.0, 20.0),
    low   = c(8.0,  18.0),
    close = c(9.0,  19.0)
  )
  fn       <- series_typical()
  result   <- fn(dt)
  expected <- (c(10, 20) + c(8, 18) + c(9, 19)) / 3

  expect_true(is.numeric(result))
  expect_length(result, 2L)
  expect_equal(result, expected)
})

test_that("series_range produces H - L of correct length", {
  dt <- data.table::data.table(
    high = c(10.0, 20.0),
    low  = c(8.0,  18.0)
  )
  fn     <- series_range()
  result <- fn(dt)

  expect_true(is.numeric(result))
  expect_length(result, 2L)
  expect_equal(result, c(2.0, 2.0))
})

test_that("Missing column produces informative error for series_log_returns", {
  dt <- data.table::data.table(close = 1:3)
  fn <- series_log_returns("open")

  expect_error(fn(dt), class = "rlang_error")
})

test_that("Missing columns produce informative errors for series_typical", {
  dt <- data.table::data.table(close = 1:3)
  fn <- series_typical()

  expect_error(fn(dt), class = "rlang_error")
})

test_that("Missing columns produce informative error for series_range", {
  dt <- data.table::data.table(close = 1:3)
  fn <- series_range()

  expect_error(fn(dt), class = "rlang_error")
})

test_that("resolve_series handles string input correctly", {
  dt     <- data.table::data.table(close = c(1.0, 2.0, 3.0))
  result <- rollNonlinear:::resolve_series(dt, "close")

  expect_equal(result, c(1.0, 2.0, 3.0))
})

test_that("resolve_series handles function input correctly", {
  dt     <- data.table::data.table(close = c(1.0, 2.0, 3.0))
  fn     <- function(dt) as.numeric(dt[["close"]]) * 2
  result <- rollNonlinear:::resolve_series(dt, fn)

  expect_equal(result, c(2.0, 4.0, 6.0))
})

test_that("resolve_series aborts on missing column (string)", {
  dt <- data.table::data.table(close = c(1.0, 2.0, 3.0))
  expect_error(rollNonlinear:::resolve_series(dt, "open"), class = "rlang_error")
})

test_that("resolve_series aborts on invalid series type", {
  dt <- data.table::data.table(close = 1:3)
  expect_error(rollNonlinear:::resolve_series(dt, 42L), class = "rlang_error")
})

test_that("resolve_series aborts when function returns wrong length", {
  dt <- data.table::data.table(close = 1:5)
  fn <- function(dt) c(1.0, 2.0)  # wrong length

  expect_error(rollNonlinear:::resolve_series(dt, fn), class = "rlang_error")
})

test_that("resolve_series aborts when function returns non-numeric", {
  dt <- data.table::data.table(close = 1:3)
  fn <- function(dt) as.character(dt[["close"]])

  expect_error(rollNonlinear:::resolve_series(dt, fn), class = "rlang_error")
})
