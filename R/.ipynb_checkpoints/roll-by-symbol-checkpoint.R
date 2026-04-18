# ---------------------------------------------------------------------------
# Multi-symbol dispatcher (sequential)
# ---------------------------------------------------------------------------
# roll_nonlinear() splits the input data by symbol, calls roll_nonlinear_one()
# on each chunk, and rbinds the results.
#
# The parallel argument is accepted but ignored with a cli_inform() notice.
# This is the forward-compat hook for Phase 3.
# ---------------------------------------------------------------------------

#' Rolling nonlinear metrics for multiple symbols
#'
#' Computes rolling window nonlinear time-series metrics for one or more
#' symbols. Splits the input `data` by `symbol_col`, resolves the series via
#' `resolve_series()`, and calls [roll_nonlinear_one()] on each chunk
#' sequentially.
#'
#' @param data A `data.table` (or `data.frame`, which will be coerced). Must
#'   contain at least `symbol_col`, `time_col`, and the column(s) implied by
#'   `series`.
#' @param metrics Character vector of metric names. Must be a subset of
#'   [available_metrics()]. Default: `c("hurst", "dfa")`.
#' @param window_size Positive integer. Number of observations per window.
#'   Default: `100L`.
#' @param stride Positive integer. Step between window anchors. Default: `1L`.
#' @param series Either a string (column name) or a function
#'   `function(dt) -> numeric` returning the series to analyse. Built-in
#'   constructors: [series_close()], [series_log_returns()],
#'   [series_typical()], [series_range()]. Default: `"close"`.
#' @param symbol_col String. Name of the symbol grouping column.
#'   Default: `"symbol"`.
#' @param time_col String. Name of the time column. Must be strictly
#'   increasing *within each symbol*. Default: `"time"`.
#' @param min_obs Positive integer (`<= window_size`). Minimum observations
#'   required per window. Default: `window_size`.
#' @param metric_args Named list of per-metric argument overrides. Names must
#'   be a subset of `metrics`. Default: `list()`.
#' @param on_error Error-handling policy passed to [roll_nonlinear_one()].
#'   One of `"warn_na"` (default), `"stop"`, or `"silent_na"`.
#' @param parallel Logical. When `TRUE` and more than one symbol is present,
#'   execution is dispatched to local `mirai` daemons. If `mirai` is not
#'   available, falls back to sequential with a warning. Default: `FALSE`.
#' @param n_workers `NULL` (default) or a positive integer. Number of local
#'   `mirai` daemons to start when none are already running. `NULL` uses
#'   `parallelly::availableCores(omit = 1L)`. Ignored when `parallel = FALSE`
#'   or when daemons are already running.
#'
#' @return A `data.table` keyed by `(symbol_col, time_col)` with one row per
#'   valid rolling window per symbol and columns:
#'   \describe{
#'     \item{`<symbol_col>`}{Symbol identifier.}
#'     \item{`time`}{Anchor time (`times[end]`).}
#'     \item{`window_start_idx`}{Integer. Window start index.}
#'     \item{`window_end_idx`}{Integer. Window end index.}
#'     \item{`<metric columns>`}{One or more numeric columns per metric.}
#'   }
#'
#' @export
#' @examples
#' \donttest{
#' dt <- data.table::data.table(
#'   symbol = rep(c("A", "B"), each = 200L),
#'   time   = c(
#'     seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = 200L),
#'     seq(as.POSIXct("2024-06-01 09:30:00", tz = "UTC"), by = 60, length.out = 200L)
#'   ),
#'   close  = c(cumsum(rnorm(200L)), cumsum(rnorm(200L)))
#' )
#' out <- roll_nonlinear(dt, metrics = "hurst", window_size = 100L, stride = 20L)
#' print(out)
#' }
roll_nonlinear <- function(
    data,
    metrics     = c("hurst", "dfa"),
    window_size = 100L,
    stride      = 1L,
    series      = "close",
    symbol_col  = "symbol",
    time_col    = "time",
    min_obs     = window_size,
    metric_args = list(),
    on_error    = c("warn_na", "stop", "silent_na"),
    parallel    = FALSE,
    n_workers   = NULL
) {
  on_error <- match.arg(on_error)

  # --- integer coercion ------------------------------------------------------
  window_size <- .as_int(window_size)
  stride      <- .as_int(stride)
  min_obs     <- .as_int(min_obs)

  # --- coerce data.frame to data.table ---------------------------------------
  if (is.data.frame(data) && !data.table::is.data.table(data)) {
    cli::cli_inform(
      "{.arg data} is a data.frame; coercing to data.table."
    )
    data <- data.table::as.data.table(data)
  }
  if (!data.table::is.data.table(data)) {
    cli::cli_abort("{.arg data} must be a data.table or data.frame.")
  }

  # --- required columns ------------------------------------------------------
  required_cols <- c(symbol_col, time_col)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Required column(s) missing from {.arg data}: {.val {missing_cols}}."
    )
  }

  # --- validate metrics against registry ------------------------------------
  m_check <- checkmate::check_character(metrics, min.len = 1L,
                                        any.missing = FALSE)
  if (!isTRUE(m_check)) {
    cli::cli_abort(c("{.arg metrics} is invalid.", "x" = "{m_check}"))
  }
  bad_metrics <- setdiff(metrics, available_metrics())
  if (length(bad_metrics) > 0L) {
    cli::cli_abort(
      "Unknown metric(s): {.val {bad_metrics}}. \\
      Available: {.val {available_metrics()}}."
    )
  }

  # --- validate metric_args --------------------------------------------------
  if (length(metric_args) > 0L) {
    ma_check <- checkmate::check_list(metric_args, names = "named")
    if (!isTRUE(ma_check)) {
      cli::cli_abort(
        c("{.arg metric_args} must be a named list.", "x" = "{ma_check}")
      )
    }
    bad_ma <- setdiff(names(metric_args), metrics)
    if (length(bad_ma) > 0L) {
      cli::cli_abort(
        "Names in {.arg metric_args} must be a subset of {.arg metrics}: \\
        {.val {bad_ma}} not in {.val {metrics}}."
      )
    }
  }

  # --- min-length advisories (one warning per metric, before main loop) ------
  for (m in metrics) {
    spec     <- get_metric(m)
    m_args   <- utils::modifyList(spec[["defaults"]],
                                  metric_args[[m]] %||% list())
    advisory <- spec[["min_len"]](m_args)
    if (window_size < advisory) {
      cli::cli_warn(
        "Metric {.val {m}} recommends {.arg window_size} >= {advisory}; \\
        got {window_size}. Results may be unreliable."
      )
    }
  }

  # --- split by symbol -------------------------------------------------------
  chunks <- split(data, by = symbol_col, keep.by = TRUE, sorted = FALSE)

  # --- choose strategy and dispatch ------------------------------------------
  strategy <- .choose_strategy(parallel, length(chunks))
  cli::cli_inform(c(
    "i" = "Rolling {length(metrics)} metric{?s} across \\
           {length(chunks)} symbol{?s} ({strategy} mode)."
  ))

  results <- switch(strategy,
    sequential = .roll_sequential(
      chunks      = chunks,
      metrics     = metrics,
      window_size = window_size,
      stride      = stride,
      series      = series,
      time_col    = time_col,
      symbol_col  = symbol_col,
      min_obs     = min_obs,
      metric_args = metric_args,
      on_error    = on_error
    ),
    parallel = .roll_parallel(
      chunks      = chunks,
      metrics     = metrics,
      window_size = window_size,
      stride      = stride,
      series      = series,
      time_col    = time_col,
      symbol_col  = symbol_col,
      min_obs     = min_obs,
      metric_args = metric_args,
      on_error    = on_error,
      n_workers   = n_workers
    )
  )

  # --- combine and key -------------------------------------------------------
  out <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  data.table::setkeyv(out, c(symbol_col, time_col))
  out
}

