test_that("sample_entropy returns correctly named scalar", {
  set.seed(1L)
  x      <- rnorm(256L)
  result <- rollNonlinear:::.metric_sample_entropy(x)

  expect_named(result, "sample_entropy")
  expect_true(is.numeric(result))
  expect_length(result, 1L)
})

test_that("approx_entropy returns correctly named scalar", {
  set.seed(1L)
  x      <- rnorm(256L)
  result <- rollNonlinear:::.metric_approx_entropy(x)

  expect_named(result, "approx_entropy")
  expect_true(is.numeric(result))
  expect_length(result, 1L)
})

test_that("entropy is lower on sinusoid than on noise", {
  x_noise <- make_gaussian_noise(256L, seed = 1L)
  x_sin   <- make_sine_wave(256L, cycles = 5)

  se_noise <- rollNonlinear:::.metric_sample_entropy(x_noise)[["sample_entropy"]]
  se_sin   <- rollNonlinear:::.metric_sample_entropy(x_sin)[["sample_entropy"]]

  # Noise entropy must be finite; sinusoid entropy must be NA or less than noise
  expect_true(is.finite(se_noise))
  expect_true(is.na(se_sin) || se_noise > se_sin)
})

test_that("r_frac = 0.5 produces different result than default (r_frac = 0.2)", {
  set.seed(42L)
  x  <- rnorm(256L)
  r1 <- rollNonlinear:::.metric_sample_entropy(x)[["sample_entropy"]]
  r2 <- rollNonlinear:::.metric_sample_entropy(
    x, .args = list(r_frac = 0.5)
  )[["sample_entropy"]]

  expect_false(isTRUE(all.equal(r1, r2)))
})

test_that("r_frac is respected for approx_entropy", {
  set.seed(42L)
  x  <- rnorm(256L)
  r1 <- rollNonlinear:::.metric_approx_entropy(x)[["approx_entropy"]]
  r2 <- rollNonlinear:::.metric_approx_entropy(
    x, .args = list(r_frac = 0.5)
  )[["approx_entropy"]]

  expect_false(isTRUE(all.equal(r1, r2)))
})

test_that("constant input returns NA without throwing for both entropy wrappers", {
  x <- rep(1.0, 100L)

  expect_no_error({
    r1 <- rollNonlinear:::.metric_sample_entropy(x)
    r2 <- rollNonlinear:::.metric_approx_entropy(x)
  })
  expect_true(is.na(r1[["sample_entropy"]]))
  expect_true(is.na(r2[["approx_entropy"]]))
})
