# ---------------------------------------------------------------------------
# Metric dispatch table
# ---------------------------------------------------------------------------
# Each entry is a list with:
#   fn    - the wrapper function (x, .args = list()) -> named numeric
#   cols  - character vector of output column names
#
# Phase 2 can replace this with a proper registry without changing
# roll_nonlinear_one() internals.
# ---------------------------------------------------------------------------

.METRIC_REGISTRY <- list(
  hurst = list(
    fn   = NULL,  # filled in below after function definitions
    cols = "hurst"
  ),
  dfa = list(
    fn   = NULL,
    cols = "dfa_alpha"
  )
)

# ---------------------------------------------------------------------------
# Wrapper: Hurst exponent (via DFA scaling exponent)
#
# Uses Detrended Fluctuation Analysis to estimate the Hurst exponent:
#   - For non-stationary (fBm-like) series where alpha > 1:  H = alpha - 1
#   - For stationary (fGn-like) series where alpha <= 1:     H = alpha
# This approach uses only exported functions from nonlinearTseries.
# ---------------------------------------------------------------------------

#' @noRd
.metric_hurst <- function(x, .args = list()) {
  defaults <- list(
    window.size.range = c(10L, max(11L, floor(length(x) / 4L))),
    npoints           = 15L,
    do.plot           = FALSE
  )
  args <- utils::modifyList(defaults, .args)
  args[["do.plot"]] <- FALSE

  result <- tryCatch(
    {
      dfa_obj <- do.call(
        nonlinearTseries::dfa,
        c(list(time.series = x), args)
      )
      alpha <- nonlinearTseries::estimate(dfa_obj, do.plot = FALSE)
      if (is.null(alpha) || length(alpha) == 0L || !is.numeric(alpha) ||
          is.na(alpha[[1L]])) {
        NA_real_
      } else {
        a <- as.numeric(alpha[[1L]])
        # Hurst from DFA: H = alpha - 1 for fBm-like (non-stationary),
        # H = alpha for fGn-like (stationary)
        if (a > 1) a - 1 else a
      }
    },
    error = function(e) NA_real_
  )

  c(hurst = result)
}

# ---------------------------------------------------------------------------
# Wrapper: DFA (Detrended Fluctuation Analysis)
# ---------------------------------------------------------------------------

#' @noRd
.metric_dfa <- function(x, .args = list()) {
  defaults <- list(
    window.size.range = c(10L, max(11L, floor(length(x) / 4L))),
    npoints           = 15L,
    do.plot           = FALSE
  )
  args <- utils::modifyList(defaults, .args)
  # Ensure do.plot is always FALSE — never allow plotting
  args[["do.plot"]] <- FALSE

  result <- tryCatch(
    {
      dfa_obj <- do.call(
        nonlinearTseries::dfa,
        c(list(time.series = x), args)
      )
      alpha <- nonlinearTseries::estimate(dfa_obj, do.plot = FALSE)
      if (is.null(alpha) || length(alpha) == 0L || !is.numeric(alpha)) {
        NA_real_
      } else {
        as.numeric(alpha[[1L]])
      }
    },
    error = function(e) NA_real_
  )

  c(dfa_alpha = result)
}

# ---------------------------------------------------------------------------
# Wire wrappers into the dispatch table
# ---------------------------------------------------------------------------

.METRIC_REGISTRY[["hurst"]][["fn"]] <- .metric_hurst
.METRIC_REGISTRY[["dfa"]][["fn"]]   <- .metric_dfa

# ---------------------------------------------------------------------------
# User-facing helper
# ---------------------------------------------------------------------------

#' List available metrics
#'
#' Returns the names of all metrics supported in Phase 1.
#'
#' @return A sorted character vector of metric names.
#' @export
#' @examples
#' available_metrics()
available_metrics <- function() {
  sort(names(.METRIC_REGISTRY))
}
