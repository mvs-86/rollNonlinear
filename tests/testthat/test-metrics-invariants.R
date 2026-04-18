test_that("lyapunov returns correctly named scalar or NA", {
  skip_on_cran()
  x      <- make_ar1(1000L)
  result <- rollNonlinear:::.metric_lyapunov(x)

  expect_named(result, "lyapunov")
  expect_length(result, 1L)
  expect_true(is.numeric(result))
})

test_that("corr_dim returns correctly named scalar or NA", {
  skip_on_cran()
  x      <- make_ar1(2000L)
  result <- rollNonlinear:::.metric_corr_dim(x)

  expect_named(result, "corr_dim")
  expect_length(result, 1L)
  expect_true(is.numeric(result))
})

test_that("invariants gracefully return NA on constant input (no throw)", {
  x <- rep(1.0, 500L)

  expect_no_error({
    ly <- rollNonlinear:::.metric_lyapunov(x)
    cd <- rollNonlinear:::.metric_corr_dim(x)
  })
  expect_true(is.na(ly[["lyapunov"]]))
  expect_true(is.na(cd[["corr_dim"]]))
})
