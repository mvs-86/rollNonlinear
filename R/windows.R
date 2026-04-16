#' Generate rolling window index table
#'
#' @param n Integer. Length of the input series.
#' @param window_size Integer. Number of bars per window (>= 2).
#' @param stride Integer. Step between consecutive window anchors (>= 1).
#' @param min_obs Integer. Minimum observations required for a valid window
#'   (>= 2, <= window_size).
#'
#' @return A `data.table` with columns:
#'   \describe{
#'     \item{start}{Integer. 1-based inclusive start index of window.}
#'     \item{end}{Integer. 1-based inclusive end index (anchor).}
#'     \item{valid}{Logical. `TRUE` if `(end - start + 1) >= min_obs`.}
#'   }
#' @noRd
make_windows <- function(n, window_size, stride, min_obs) {
  # --- validate inputs -------------------------------------------------------
  if (!checkmate::test_int(window_size, lower = 2L)) {
    cli::cli_abort(
      "{.arg window_size} must be an integer >= 2, not {.val {window_size}}."
    )
  }
  if (!checkmate::test_int(stride, lower = 1L)) {
    cli::cli_abort(
      "{.arg stride} must be an integer >= 1, not {.val {stride}}."
    )
  }
  if (!checkmate::test_int(min_obs, lower = 2L)) {
    cli::cli_abort(
      "{.arg min_obs} must be an integer >= 2, not {.val {min_obs}}."
    )
  }
  if (min_obs > window_size) {
    cli::cli_abort(
      "{.arg min_obs} ({min_obs}) must be <= {.arg window_size} ({window_size})."
    )
  }

  # --- empty schema when series is shorter than one full window ---------------
  empty_dt <- data.table::data.table(
    start = integer(0L),
    end   = integer(0L),
    valid = logical(0L)
  )

  if (n < window_size) {
    return(empty_dt)
  }

  # --- build anchor sequence --------------------------------------------------
  anchors <- seq(from = window_size, to = n, by = stride)
  starts  <- anchors - window_size + 1L

  data.table::data.table(
    start = as.integer(starts),
    end   = as.integer(anchors),
    valid = ((as.integer(anchors) - as.integer(starts) + 1L) >= min_obs)
  )
}
