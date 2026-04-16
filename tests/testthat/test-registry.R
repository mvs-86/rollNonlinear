test_that("register_metric() adds a new metric retrievable via get_metric()", {
  my_fn <- function(x, .args = list()) c(my_metric = mean(x, na.rm = TRUE))
  register_metric("my_test_metric_reg", my_fn, outputs = "my_metric")

  expect_true("my_test_metric_reg" %in% available_metrics())
  spec <- rollNonlinear:::get_metric("my_test_metric_reg")
  expect_equal(spec[["outputs"]], "my_metric")
  expect_true(is.function(spec[["fn"]]))

  rm("my_test_metric_reg", envir = rollNonlinear:::.metric_registry)
})

test_that("Registering a built-in name emits an info message and overwrites", {
  orig <- rollNonlinear:::get_metric("hurst")
  new_fn <- function(x, .args = list()) c(hurst = 0.5)

  expect_message(
    register_metric("hurst", new_fn, outputs = "hurst"),
    regexp = "hurst"
  )
  expect_identical(rollNonlinear:::get_metric("hurst")[["fn"]], new_fn)

  # Restore original entry
  assign("hurst", orig, envir = rollNonlinear:::.metric_registry)
})

test_that("Invalid fn type aborts with informative message", {
  expect_error(
    register_metric("bad_metric", "not_a_function", outputs = "x"),
    class = "rlang_error"
  )
})

test_that("Empty outputs vector aborts", {
  fn <- function(x, .args = list()) c(x = 1)
  expect_error(
    register_metric("bad_metric", fn, outputs = character(0L)),
    class = "rlang_error"
  )
})

test_that("Duplicate outputs vector aborts", {
  fn <- function(x, .args = list()) c(x = 1)
  expect_error(
    register_metric("bad_metric", fn, outputs = c("a", "a")),
    class = "rlang_error"
  )
})

test_that("fn without required formals aborts", {
  bad_fn <- function(x) c(val = 1)
  expect_error(
    register_metric("bad_metric", bad_fn, outputs = "val"),
    class = "rlang_error"
  )
})

test_that("User-registered metric works end-to-end in roll_nonlinear_one()", {
  my_fn <- function(x, .args = list()) c(my_mean = mean(x, na.rm = TRUE))
  register_metric("my_mean_e2e", my_fn, outputs = "my_mean",
                  min_len = function(a) 2L)

  x   <- make_ar1(200L)
  tms <- make_times(200L)

  out <- roll_nonlinear_one(x, tms, metrics = "my_mean_e2e",
                            window_size = 50L, stride = 10L)

  expect_s3_class(out, "data.table")
  expect_true("my_mean" %in% names(out))
  expect_true(all(is.finite(out[["my_mean"]])))

  rm("my_mean_e2e", envir = rollNonlinear:::.metric_registry)
})

test_that("available_metrics() lists all 6 built-in metrics", {
  am <- available_metrics()
  expect_gte(length(am), 6L)
  expect_true(all(c("hurst", "dfa", "sample_entropy", "approx_entropy",
                    "lyapunov", "corr_dim") %in% am))
})
