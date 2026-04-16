#' Validate arguments for `roll_nonlinear_one`
#'
#' @param x Numeric vector (NAs allowed).
#' @param times POSIXct, Date, or integer vector of the same length as `x`.
#' @param metrics Character vector of metric names.
#' @param window_size Positive integer.
#' @param stride Positive integer.
#' @param min_obs Positive integer (<= window_size).
#' @param on_error One of `"warn_na"`, `"stop"`, `"silent_na"`.
#'
#' @return Invisibly `TRUE` on success; calls [cli::cli_abort()] on failure.
#' @noRd
.validate_roll_args <- function(x, times, metrics,
                                window_size, stride, min_obs,
                                on_error) {
  # --- x --------------------------------------------------------------------
  x_check <- checkmate::check_numeric(x, min.len = 2L, any.missing = TRUE)
  if (!isTRUE(x_check)) {
    cli::cli_abort(
      c("{.arg x} is invalid.", "x" = "{x_check}")
    )
  }

  # --- times ----------------------------------------------------------------
  if (length(times) != length(x)) {
    cli::cli_abort(
      "{.arg times} must have the same length as {.arg x} \\
      ({length(times)} vs {length(x)})."
    )
  }
  if (!inherits(times, c("POSIXct", "Date")) &&
      !checkmate::test_integerish(times, any.missing = FALSE)) {
    cli::cli_abort(
      "{.arg times} must be a {.cls POSIXct}, {.cls Date}, or integer vector."
    )
  }
  times_num <- as.numeric(times)
  if (length(times_num) >= 2L && any(diff(times_num) <= 0)) {
    cli::cli_abort(
      "{.arg times} must be strictly increasing."
    )
  }

  # --- metrics --------------------------------------------------------------
  m_check <- checkmate::check_character(metrics, min.len = 1L,
                                        any.missing = FALSE)
  if (!isTRUE(m_check)) {
    cli::cli_abort(
      c("{.arg metrics} is invalid.", "x" = "{m_check}")
    )
  }
  bad <- setdiff(metrics, available_metrics())
  if (length(bad) > 0L) {
    cli::cli_abort(
      "Unknown metric(s) in {.arg metrics}: {.val {bad}}. \\
      Available: {.val {available_metrics()}}."
    )
  }

  # --- window_size ----------------------------------------------------------
  ws_check <- checkmate::check_int(window_size, lower = 1L)
  if (!isTRUE(ws_check)) {
    cli::cli_abort(
      c("{.arg window_size} must be a positive integer.", "x" = "{ws_check}")
    )
  }

  # --- stride ---------------------------------------------------------------
  st_check <- checkmate::check_int(stride, lower = 1L)
  if (!isTRUE(st_check)) {
    cli::cli_abort(
      c("{.arg stride} must be a positive integer.", "x" = "{st_check}")
    )
  }

  # --- min_obs --------------------------------------------------------------
  mo_check <- checkmate::check_int(min_obs, lower = 1L)
  if (!isTRUE(mo_check)) {
    cli::cli_abort(
      c("{.arg min_obs} must be a positive integer.", "x" = "{mo_check}")
    )
  }
  if (min_obs > window_size) {
    cli::cli_abort(
      "{.arg min_obs} ({min_obs}) must be <= {.arg window_size} ({window_size})."
    )
  }

  # --- on_error -------------------------------------------------------------
  oe_check <- checkmate::check_choice(
    on_error, choices = c("warn_na", "stop", "silent_na")
  )
  if (!isTRUE(oe_check)) {
    cli::cli_abort(
      c("{.arg on_error} is invalid.", "x" = "{oe_check}")
    )
  }

  invisible(TRUE)
}
