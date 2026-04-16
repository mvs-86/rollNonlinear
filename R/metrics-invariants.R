# ---------------------------------------------------------------------------
# Invariant metric wrappers (fragile — fully defensive)
# ---------------------------------------------------------------------------
# Implements lyapunov (maximal Lyapunov exponent) and corr_dim (correlation
# dimension) using nonlinearTseries.
#
# Both wrappers:
#   - Pass do.plot = FALSE to every nonlinearTseries call.
#   - Wrap the entire computation in tryCatch so they never throw.
#   - Extract regression.range from .args and pass it to estimate(), NOT to
#     the primary computation function.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Wrapper: maximal Lyapunov exponent
# ---------------------------------------------------------------------------

#' @noRd
.metric_lyapunov <- function(x, .args = list()) {
  defaults <- list(
    min.embedding.dim = 2L,
    max.embedding.dim = 5L,
    time.lag          = 1L,
    theiler.window    = 10L,
    min.neighs        = 5L,
    min.ref.points    = 10L,
    max.time.steps    = 20L,
    do.plot           = FALSE,
    radius            = NULL
  )
  args <- utils::modifyList(defaults, .args)
  args[["do.plot"]] <- FALSE

  # Pull out regression.range before forwarding (it goes to estimate())
  regression.range <- args[["regression.range"]] %||%
    c(1L, args[["max.time.steps"]])
  args[["regression.range"]] <- NULL

  # Auto-set radius from data range if not provided
  if (is.null(args[["radius"]])) {
    rng <- diff(range(x, na.rm = TRUE))
    args[["radius"]] <- 0.05 * rng
  }

  result <- tryCatch(
    {
      ml  <- do.call(
        nonlinearTseries::maxLyapunov,
        c(list(time.series = x), args)
      )
      val <- nonlinearTseries::estimate(
        ml,
        regression.range = regression.range,
        do.plot          = FALSE
      )
      if (is.null(val) || length(val) == 0L || !is.numeric(val)) {
        NA_real_
      } else {
        as.numeric(val[[1L]])
      }
    },
    error = function(e) NA_real_
  )

  c(lyapunov = result)
}

# ---------------------------------------------------------------------------
# Wrapper: correlation dimension
# ---------------------------------------------------------------------------

#' @noRd
.metric_corr_dim <- function(x, .args = list()) {
  defaults <- list(
    min.embedding.dim = 2L,
    max.embedding.dim = 5L,
    time.lag          = 1L,
    n.points.radius   = 20L,
    theiler.window    = 10L,
    do.plot           = FALSE,
    min.radius        = NULL,
    max.radius        = NULL
  )
  args <- utils::modifyList(defaults, .args)
  args[["do.plot"]] <- FALSE

  rng <- diff(range(x, na.rm = TRUE))

  if (is.null(args[["min.radius"]])) {
    args[["min.radius"]] <- 0.01 * rng
  }
  if (is.null(args[["max.radius"]])) {
    args[["max.radius"]] <- 0.5 * rng
  }

  min_r <- args[["min.radius"]]
  max_r <- args[["max.radius"]]

  regression.range <- args[["regression.range"]] %||%
    c(min_r * 2, max_r / 2)
  args[["regression.range"]] <- NULL

  result <- tryCatch(
    {
      cd  <- do.call(
        nonlinearTseries::corrDim,
        c(list(time.series = x), args)
      )
      val <- nonlinearTseries::estimate(
        cd,
        regression.range = regression.range,
        do.plot          = FALSE
      )
      if (is.null(val) || length(val) == 0L || !is.numeric(val)) {
        NA_real_
      } else {
        as.numeric(val[[1L]])
      }
    },
    error = function(e) NA_real_
  )

  c(corr_dim = result)
}
