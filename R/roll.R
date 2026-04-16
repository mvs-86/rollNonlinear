#' Rolling nonlinear metrics for a single symbol
#'
#' Computes rolling window nonlinear time-series metrics for a single price
#' (or any numeric) series using sequential execution.
#'
#' @param x Numeric vector. The time series values (NAs allowed in the series;
#'   handling is per-window based on `on_error`).
#' @param times A strictly-increasing vector of timestamps corresponding to
#'   each element of `x`. Must be `POSIXct`, `Date`, or integer.
#' @param metrics Character vector of metric names to compute. Must be a
#'   subset of [available_metrics()]. Defaults to all available metrics.
#' @param window_size Positive integer. Number of observations per rolling
#'   window. Default: `100L`.
#' @param stride Positive integer. Step between consecutive window anchors.
#'   Default: `1L`.
#' @param min_obs Positive integer (<= `window_size`). Minimum observations
#'   required for a window to be computed. Default: `window_size` (fully
#'   formed windows only).
#' @param on_error Error-handling policy for metric computation failures
#'   (including windows containing NAs):
#'   \describe{
#'     \item{`"warn_na"`}{Emit a deduplicated [cli::cli_warn()] and fill with
#'       `NA_real_` (default).}
#'     \item{`"stop"`}{Re-throw via [cli::cli_abort()] including window
#'       indices.}
#'     \item{`"silent_na"`}{Fill with `NA_real_` silently.}
#'   }
#'
#' @return A keyed `data.table` (keyed by `time`) with one row per valid
#'   rolling window and the following columns:
#'   \describe{
#'     \item{`time`}{Anchor time (`times[end]`).}
#'     \item{`window_start_idx`}{Integer. 1-based start index of the window.}
#'     \item{`window_end_idx`}{Integer. 1-based end index (anchor).}
#'     \item{`hurst`}{Numeric. Hurst exponent (if `"hurst"` in `metrics`).}
#'     \item{`dfa_alpha`}{Numeric. DFA scaling exponent (if `"dfa"` in
#'       `metrics`).}
#'   }
#'   The schema is stable even when all metric values are `NA`. Returns a
#'   0-row `data.table` with the same schema when there are no valid windows.
#'
#' @export
#' @examples
#' \donttest{
#' set.seed(1L)
#' n   <- 300L
#' x   <- cumsum(rnorm(n))
#' tms <- seq(
#'   as.POSIXct("2024-01-02 09:30:00", tz = "UTC"),
#'   by = 60,
#'   length.out = n
#' )
#' out <- roll_nonlinear_one(x, tms, window_size = 128L, stride = 32L)
#' print(out)
#' }
roll_nonlinear_one <- function(
    x,
    times,
    metrics     = c("hurst", "dfa"),
    window_size = 100L,
    stride      = 1L,
    min_obs     = window_size,
    on_error    = c("warn_na", "stop", "silent_na")
) {
  # --- match arguments -------------------------------------------------------
  on_error <- match.arg(on_error)

  # --- coerce integer-ish scalars --------------------------------------------
  window_size <- .as_int(window_size)
  stride      <- .as_int(stride)
  min_obs     <- .as_int(min_obs)

  # --- validate --------------------------------------------------------------
  .validate_roll_args(x, times, metrics, window_size, stride, min_obs,
                      on_error)

  # --- build output schema ---------------------------------------------------
  # Determine all output columns for the requested metrics
  all_metric_cols <- unlist(
    lapply(metrics, function(m) .METRIC_REGISTRY[[m]][["cols"]]),
    use.names = FALSE
  )

  .empty_result <- function() {
    dt <- data.table::data.table(
      time             = times[integer(0L)],
      window_start_idx = integer(0L),
      window_end_idx   = integer(0L)
    )
    for (col in all_metric_cols) {
      dt[, (col) := numeric(0L)]
    }
    data.table::setkey(dt, time)
    dt
  }

  # --- build windows ---------------------------------------------------------
  wins <- make_windows(length(x), window_size, stride, min_obs)

  valid_wins <- wins[valid == TRUE]
  if (nrow(valid_wins) == 0L) {
    return(.empty_result())
  }

  # --- deduplication state for warn_na: set of already-warned keys ----------
  # Key format: "<metric>::<error_category>" (no window-specific info)
  seen_warn_keys <- character(0L)

  # --- per-window computation ------------------------------------------------
  rows <- vector("list", nrow(valid_wins))

  for (i in seq_len(nrow(valid_wins))) {
    s     <- valid_wins[["start"]][[i]]
    e     <- valid_wins[["end"]][[i]]
    x_win <- x[s:e]

    row_vals <- list(
      time             = times[[e]],
      window_start_idx = s,
      window_end_idx   = e
    )

    has_na <- anyNA(x_win)

    for (m in metrics) {
      entry    <- .METRIC_REGISTRY[[m]]
      out_cols <- entry[["cols"]]
      na_vals  <- stats::setNames(rep(NA_real_, length(out_cols)), out_cols)

      if (has_na) {
        dedup_key <- paste0(m, "::NA_in_window")
        seen_warn_keys <- .emit_or_skip(
          on_error  = on_error,
          warn_key  = dedup_key,
          seen_keys = seen_warn_keys,
          metric    = m,
          s         = s,
          e         = e,
          err_msg   = "Window contains NA values."
        )
        for (col in out_cols) row_vals[[col]] <- na_vals[[col]]
        next
      }

      # Normal computation path
      metric_result <- tryCatch(
        entry[["fn"]](x_win, .args = list()),
        error = function(err) {
          structure(list(msg = conditionMessage(err)), class = "metric_error")
        }
      )

      if (inherits(metric_result, "metric_error")) {
        err_msg   <- metric_result[["msg"]]
        dedup_key <- paste0(m, "::", err_msg)
        seen_warn_keys <- .emit_or_skip(
          on_error  = on_error,
          warn_key  = dedup_key,
          seen_keys = seen_warn_keys,
          metric    = m,
          s         = s,
          e         = e,
          err_msg   = err_msg
        )
        for (col in out_cols) row_vals[[col]] <- na_vals[[col]]
      } else {
        for (col in out_cols) row_vals[[col]] <- metric_result[[col]]
      }
    }

    rows[[i]] <- row_vals
  }

  # --- assemble output -------------------------------------------------------
  out <- data.table::rbindlist(rows)
  # Ensure all metric columns are present and numeric (schema stability)
  for (col in all_metric_cols) {
    if (!col %in% names(out)) {
      out[, (col) := NA_real_]
    } else {
      out[, (col) := as.numeric(get(col))]
    }
  }

  data.table::setkey(out, time)
  out
}

# ---------------------------------------------------------------------------
# Internal: emit warning or abort, with deduplication
# Returns the updated seen_keys vector
# ---------------------------------------------------------------------------

#' @noRd
.emit_or_skip <- function(on_error, warn_key, seen_keys,
                          metric, s, e, err_msg) {
  if (on_error == "stop") {
    cli::cli_abort(c(
      "Metric {.val {metric}} failed on window [{s}, {e}].",
      "x" = "{err_msg}"
    ))
  } else if (on_error == "warn_na") {
    if (!warn_key %in% seen_keys) {
      seen_keys <- c(seen_keys, warn_key)
      cli::cli_warn(c(
        "Metric {.val {metric}} failed (window [{s}, {e}]); filling {.code NA}.",
        "x" = "{err_msg}"
      ))
    }
  }
  # silent_na: do nothing
  seen_keys
}
