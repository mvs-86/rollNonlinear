# ---------------------------------------------------------------------------
# Package load hook: register built-in metrics
# ---------------------------------------------------------------------------

#' @noRd
.onLoad <- function(libname, pkgname) {
  .register(
    "hurst",
    .metric_hurst,
    outputs  = "hurst",
    defaults = list(),
    min_len  = function(a) 32L
  )

  .register(
    "dfa",
    .metric_dfa,
    outputs  = "dfa_alpha",
    defaults = list(),
    min_len  = function(a) 64L
  )

  .register(
    "sample_entropy",
    .metric_sample_entropy,
    outputs  = "sample_entropy",
    defaults = list(dimension = 2L, r_frac = 0.2),
    min_len  = function(a) 10L * (a[["dimension"]] %||% 2L)^2
  )

  .register(
    "approx_entropy",
    .metric_approx_entropy,
    outputs  = "approx_entropy",
    defaults = list(dimension = 2L, r_frac = 0.2),
    min_len  = function(a) 10L * (a[["dimension"]] %||% 2L)^2
  )

  .register(
    "lyapunov",
    .metric_lyapunov,
    outputs  = "lyapunov",
    defaults = list(),
    min_len  = function(a) 200L
  )

  .register(
    "corr_dim",
    .metric_corr_dim,
    outputs  = "corr_dim",
    defaults = list(),
    min_len  = function(a) 500L
  )
}
