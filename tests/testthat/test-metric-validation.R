test_that("metric validation helper rejects expectation names outside metric outputs", {
  expect_error(
    mv_case(
      name = "bad-hurst-case",
      metric = "hurst",
      generator = function() make_fbm(256L, hurst = 0.7, seed = 1L),
      expectations = list(
        dfa_alpha = mv_expect_tolerance(0.7, 0.2)
      )
    ),
    regexp = "expectations"
  )
})

test_that("metric validation helper accepts matching tolerance checks", {
  problems <- rollNonlinear:::.metric_validation_problems(
    result = c(hurst = 0.72),
    expectations = list(
      hurst = mv_expect_tolerance(0.7, 0.05)
    )
  )

  expect_length(problems, 0L)
})

test_that("metric validation helper respects allow_na", {
  problems <- rollNonlinear:::.metric_validation_problems(
    result = c(corr_dim = NA_real_),
    expectations = list(
      corr_dim = mv_expect_range(0, Inf, allow_na = TRUE)
    )
  )

  expect_length(problems, 0L)
})

test_that("built-in metrics pass canonical synthetic validation cases", {
  cases <- list(
    mv_case(
      name = "hurst-fbm-h07",
      metric = "hurst",
      generator = function() make_fbm(512L, hurst = 0.7, seed = 42L),
      expectations = list(
        hurst = mv_expect_tolerance(0.7, 0.15)
      )
    ),
    mv_case(
      name = "dfa-fbm-h07",
      metric = "dfa",
      generator = function() make_fbm(1024L, hurst = 0.7, seed = 7L),
      expectations = list(
        dfa_alpha = mv_expect_tolerance(1.7, 0.2)
      )
    ),
    mv_case(
      name = "sample-entropy-gaussian",
      metric = "sample_entropy",
      generator = function() make_gaussian_noise(512L, seed = 42L),
      expectations = list(
        sample_entropy = mv_expect_range(0.5, 2.5)
      )
    ),
    mv_case(
      name = "approx-entropy-gaussian",
      metric = "approx_entropy",
      generator = function() make_gaussian_noise(256L, seed = 42L),
      expectations = list(
        approx_entropy = mv_expect_range(0.5, 2.5)
      )
    ),
    mv_case(
      name = "lyapunov-logistic-r4",
      metric = "lyapunov",
      generator = function() make_logistic_map(1000L, r = 4, x0 = 0.12345),
      expectations = list(
        lyapunov = mv_expect_predicate(
          fn = function(x) x > 0,
          label = "positive"
        )
      )
    ),
    mv_case(
      name = "lyapunov-periodic-sine",
      metric = "lyapunov",
      generator = function() make_sine_wave(500L, cycles = 10),
      expectations = list(
        lyapunov = mv_expect_predicate(
          fn = function(x) x <= 0.1,
          label = "<= 0.1",
          allow_na = TRUE
        )
      )
    ),
    mv_case(
      name = "corr-dim-white-noise",
      metric = "corr_dim",
      generator = function() make_gaussian_noise(2000L, seed = 42L),
      expectations = list(
        corr_dim = mv_expect_predicate(
          fn = function(x) is.finite(x) && x > 0,
          label = "finite positive",
          allow_na = TRUE
        )
      )
    )
  )

  for (case in cases) {
    expect_metric_validation(case)
  }
})
