test_that("lyapunov returns correctly named scalar or NA", {
  skip_on_cran()
  x      <- make_ar1(1000L)
  result <- rollNonlinear:::.metric_lyapunov(x)

  expect_named(result, "lyapunov")
  expect_length(result, 1L)
  expect_true(is.numeric(result))
})

test_that("lyapunov on logistic map (r=4) returns a positive value", {
  skip_on_cran()
  n         <- 1000L
  x         <- numeric(n)
  x[[1L]]   <- 0.5
  for (i in seq_len(n - 1L)) x[[i + 1L]] <- 4 * x[[i]] * (1 - x[[i]])

  result <- rollNonlinear:::.metric_lyapunov(x)
  val    <- result[["lyapunov"]]

  # Logistic map at r=4 has positive maximal Lyapunov exponent (~ln 2 ≈ 0.69)
  expect_true(is.na(val) || val > 0)
})

test_that("lyapunov on periodic series returns <= 0.1 or NA", {
  skip_on_cran()
  x      <- sin(seq(0, 20 * pi, length.out = 500L))
  result <- rollNonlinear:::.metric_lyapunov(x)
  val    <- result[["lyapunov"]]

  # Periodic orbit: Lyapunov exponent non-positive (allow small positive due to numerics)
  expect_true(is.na(val) || val <= 0.1)
})

test_that("corr_dim returns correctly named scalar or NA", {
  skip_on_cran()
  x      <- make_ar1(2000L)
  result <- rollNonlinear:::.metric_corr_dim(x)

  expect_named(result, "corr_dim")
  expect_length(result, 1L)
  expect_true(is.numeric(result))
})

test_that("corr_dim on white noise (n=2000) returns finite positive or NA", {
  skip_on_cran()
  set.seed(42L)
  x      <- rnorm(2000L)
  result <- rollNonlinear:::.metric_corr_dim(x)
  val    <- result[["corr_dim"]]

  expect_true(is.na(val) || (is.finite(val) && val > 0))
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
