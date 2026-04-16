# Tests for the parallel execution path.
# Requires mirai to be installed; setup-mirai.R starts 2 daemons before
# any test in this file runs.

skip_if_not_installed("mirai")

# ---------------------------------------------------------------------------
# Shared synthetic dataset
# ---------------------------------------------------------------------------

make_multi_symbol_dt <- function(n_syms = 5L, n_bars = 100L) {
  set.seed(123L)
  data.table::rbindlist(lapply(seq_len(n_syms), function(i) {
    data.table::data.table(
      symbol = paste0("SYM", i),
      time   = make_times(n_bars),
      close  = cumsum(rnorm(n_bars))
    )
  }))
}

dt5 <- make_multi_symbol_dt()

# ---------------------------------------------------------------------------
# Equivalence: sequential == parallel
# ---------------------------------------------------------------------------

test_that("parallel and sequential produce identical output", {
  seq_out <- roll_nonlinear(
    dt5,
    metrics     = c("hurst", "dfa"),
    window_size = 64L,
    stride      = 10L,
    parallel    = FALSE
  )
  par_out <- roll_nonlinear(
    dt5,
    metrics     = c("hurst", "dfa"),
    window_size = 64L,
    stride      = 10L,
    parallel    = TRUE
  )

  # Sort both the same way before comparison
  data.table::setkeyv(seq_out, c("symbol", "time"))
  data.table::setkeyv(par_out, c("symbol", "time"))

  expect_equal(seq_out, par_out, ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# Daemon ownership: pre-existing daemons untouched
# ---------------------------------------------------------------------------

test_that("pre-existing daemons are not torn down after parallel run", {
  # setup-mirai.R already started 2 daemons
  before <- mirai::status()$connections
  expect_gte(before, 1L)

  roll_nonlinear(
    dt5,
    metrics     = "hurst",
    window_size = 64L,
    stride      = 10L,
    parallel    = TRUE
  )

  after <- mirai::status()$connections
  expect_equal(after, before)
})

# ---------------------------------------------------------------------------
# Daemon auto-setup and teardown
# ---------------------------------------------------------------------------

test_that("package sets up and tears down its own daemons when none are running", {
  # Stop any running daemons first
  try(mirai::daemons(0L), silent = TRUE)
  Sys.sleep(0.1)

  roll_nonlinear(
    dt5,
    metrics     = "hurst",
    window_size = 64L,
    stride      = 10L,
    parallel    = TRUE,
    n_workers   = 1L
  )

  after <- tryCatch(mirai::status()$connections, error = function(e) 0L)
  expect_equal(after, 0L)

  # Restart 2 daemons for remaining tests in session
  mirai::daemons(2L)
})

# ---------------------------------------------------------------------------
# Single-symbol short-circuit
# ---------------------------------------------------------------------------

test_that(".choose_strategy returns sequential for 1 symbol", {
  strategy <- rollNonlinear:::.choose_strategy(TRUE, 1L)
  expect_equal(strategy, "sequential")
})

test_that(".choose_strategy returns sequential when parallel = FALSE", {
  strategy <- rollNonlinear:::.choose_strategy(FALSE, 10L)
  expect_equal(strategy, "sequential")
})

test_that(".choose_strategy returns parallel for multi-symbol when mirai available", {
  strategy <- rollNonlinear:::.choose_strategy(TRUE, 3L)
  expect_equal(strategy, "parallel")
})

# ---------------------------------------------------------------------------
# Function-valued series under parallel execution
# ---------------------------------------------------------------------------

test_that("function-valued series works correctly in parallel mode", {
  set.seed(99L)
  dt <- data.table::rbindlist(lapply(1:3, function(i) {
    data.table::data.table(
      symbol = paste0("S", i),
      time   = make_times(100L),
      close  = cumsum(rnorm(100L)) + 100
    )
  }))

  seq_out <- roll_nonlinear(
    dt,
    metrics     = "hurst",
    window_size = 64L,
    stride      = 10L,
    series      = series_log_returns(),
    parallel    = FALSE
  )
  par_out <- roll_nonlinear(
    dt,
    metrics     = "hurst",
    window_size = 64L,
    stride      = 10L,
    series      = series_log_returns(),
    parallel    = TRUE
  )

  data.table::setkeyv(seq_out, c("symbol", "time"))
  data.table::setkeyv(par_out, c("symbol", "time"))

  expect_equal(seq_out, par_out, ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# User-registered metric under parallel execution
# ---------------------------------------------------------------------------

test_that("user-registered metric works under parallel execution", {
  register_metric(
    "mean_x",
    fn      = function(x, .args = list()) c(mean_x = mean(x, na.rm = TRUE)),
    outputs = "mean_x"
  )
  on.exit(
    suppressWarnings(try(
      rollNonlinear:::.metric_registry[["mean_x"]] <- NULL, silent = TRUE
    )),
    add = TRUE
  )

  par_out <- roll_nonlinear(
    dt5,
    metrics     = "mean_x",
    window_size = 64L,
    stride      = 10L,
    parallel    = TRUE
  )

  expect_true("mean_x" %in% names(par_out))
  expect_false(anyNA(par_out[["mean_x"]]))
})

# ---------------------------------------------------------------------------
# Error propagation from workers
# ---------------------------------------------------------------------------

test_that("worker errors surface with chunk context when on_error = 'stop'", {
  register_metric(
    "always_fails",
    fn      = function(x, .args = list()) stop("deliberate worker error"),
    outputs = "always_fails"
  )
  on.exit(
    suppressWarnings(try(
      rollNonlinear:::.metric_registry[["always_fails"]] <- NULL, silent = TRUE
    )),
    add = TRUE
  )

  expect_error(
    roll_nonlinear(
      dt5,
      metrics     = "always_fails",
      window_size = 64L,
      stride      = 10L,
      parallel    = TRUE,
      on_error    = "stop"
    )
  )
})
