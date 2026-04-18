test_that(".metric_hurst returns named numeric of length 1", {
  x <- make_ar1(200L)
  result <- rollNonlinear:::.metric_hurst(x)
  expect_named(result, "hurst")
  expect_length(result, 1L)
  expect_type(result, "double")
})

test_that(".metric_hurst returns NA on constant input (no throw)", {
  x <- rep(1.0, 100L)
  result <- expect_no_error(rollNonlinear:::.metric_hurst(x))
  expect_named(result, "hurst")
  expect_true(is.na(result[["hurst"]]))
})

test_that(".metric_dfa returns named numeric of length 1", {
  x <- make_fbm(300L, hurst = 0.7, seed = 1L)
  result <- rollNonlinear:::.metric_dfa(x)
  expect_named(result, "dfa_alpha")
  expect_length(result, 1L)
  expect_type(result, "double")
})

test_that(".metric_dfa returns NA on constant input (no throw)", {
  x <- rep(2.0, 200L)
  result <- expect_no_error(rollNonlinear:::.metric_dfa(x))
  expect_named(result, "dfa_alpha")
  expect_true(is.na(result[["dfa_alpha"]]))
})

test_that("available_metrics returns sorted character vector", {
  m <- available_metrics()
  expect_type(m, "character")
  expect_true(length(m) >= 2L)
  expect_identical(m, sort(m))
  expect_true("hurst" %in% m)
  expect_true("dfa"   %in% m)
})
