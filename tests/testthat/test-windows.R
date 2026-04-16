test_that("make_windows returns correct number of windows (stride=1)", {
  wins <- rollNonlinear:::make_windows(100L, 20L, 1L, 20L)
  expect_equal(nrow(wins), 81L)  # anchors: 20, 21, ..., 100
})

test_that("make_windows returns correct number of windows (stride=5)", {
  wins <- rollNonlinear:::make_windows(100L, 20L, 5L, 20L)
  expect_equal(nrow(wins), 17L)  # anchors: 20, 25, ..., 100
})

test_that("first anchor equals window_size", {
  wins <- rollNonlinear:::make_windows(50L, 10L, 1L, 10L)
  expect_equal(wins$end[[1L]], 10L)
  expect_equal(wins$start[[1L]], 1L)
})

test_that("last anchor is <= n", {
  wins <- rollNonlinear:::make_windows(55L, 10L, 7L, 10L)
  expect_true(wins$end[[nrow(wins)]] <= 55L)
})

test_that("all valid when min_obs == window_size", {
  wins <- rollNonlinear:::make_windows(50L, 10L, 1L, 10L)
  expect_true(all(wins$valid))
})

test_that("valid column reflects min_obs edge cases", {
  # With window_size=20 and min_obs=15, all windows have end-start+1==20 >= 15
  wins <- rollNonlinear:::make_windows(50L, 20L, 1L, 15L)
  expect_true(all(wins$valid))
})

test_that("returns empty data.table when n < window_size", {
  wins <- rollNonlinear:::make_windows(5L, 10L, 1L, 10L)
  expect_true(is.data.frame(wins))
  expect_equal(nrow(wins), 0L)
  expect_named(wins, c("start", "end", "valid"))
})

test_that("returns empty data.table when n == window_size - 1", {
  wins <- rollNonlinear:::make_windows(9L, 10L, 1L, 10L)
  expect_equal(nrow(wins), 0L)
})

test_that("make_windows: exactly n == window_size gives one row", {
  wins <- rollNonlinear:::make_windows(10L, 10L, 1L, 10L)
  expect_equal(nrow(wins), 1L)
  expect_equal(wins$start[[1L]], 1L)
  expect_equal(wins$end[[1L]], 10L)
})

test_that("make_windows errors on invalid window_size", {
  expect_error(
    rollNonlinear:::make_windows(100L, 1L, 1L, 1L),
    regexp = "window_size"
  )
  expect_error(
    rollNonlinear:::make_windows(100L, -1L, 1L, 2L),
    regexp = "window_size"
  )
})

test_that("make_windows errors on invalid stride", {
  expect_error(
    rollNonlinear:::make_windows(100L, 10L, 0L, 5L),
    regexp = "stride"
  )
})

test_that("make_windows errors on invalid min_obs", {
  expect_error(
    rollNonlinear:::make_windows(100L, 10L, 1L, 1L),
    regexp = "min_obs"
  )
})

test_that("make_windows errors when min_obs > window_size", {
  expect_error(
    rollNonlinear:::make_windows(100L, 10L, 1L, 11L),
    regexp = "min_obs"
  )
})

test_that("output columns are correctly typed", {
  wins <- rollNonlinear:::make_windows(50L, 10L, 5L, 8L)
  expect_type(wins$start, "integer")
  expect_type(wins$end,   "integer")
  expect_type(wins$valid, "logical")
})
