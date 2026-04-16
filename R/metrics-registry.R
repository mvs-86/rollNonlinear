# ---------------------------------------------------------------------------
# Metric registry
# ---------------------------------------------------------------------------
# Environment-based registry keyed by metric name. Each entry is a list with:
#   fn       - wrapper function(x, .args = list()) -> named numeric
#   outputs  - character vector of output column names
#   defaults - named list of default args
#   min_len  - function(args) -> integer advisory minimum window size
# ---------------------------------------------------------------------------

#' @noRd
.metric_registry <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.register <- function(name, fn, outputs, defaults = list(),
                       min_len = function(a) 2L) {
  .metric_registry[[name]] <- list(
    fn       = fn,
    outputs  = outputs,
    defaults = defaults,
    min_len  = min_len
  )
  invisible(NULL)
}

#' @noRd
get_metric <- function(name) {
  if (!exists(name, envir = .metric_registry, inherits = FALSE)) {
    cli::cli_abort(
      "Unknown metric {.val {name}}. \\
      Available: {.val {available_metrics()}}."
    )
  }
  .metric_registry[[name]]
}

# ---------------------------------------------------------------------------
# Exported: user-facing registration
# ---------------------------------------------------------------------------

#' Register a custom rolling metric
#'
#' Adds a new metric (or replaces an existing one) in the package metric
#' registry. Once registered, the metric name is available for use in
#' [roll_nonlinear_one()] and [roll_nonlinear()].
#'
#' @param name A single string. The metric name (key in the registry).
#' @param fn A function with formals `(x, .args = list())` that accepts a
#'   numeric vector `x` and a list of named arguments `.args`, and returns a
#'   named numeric vector with names matching `outputs`.
#' @param outputs Non-empty, unique character vector of output column names
#'   that `fn` returns.
#' @param defaults Named list of default arguments passed to `fn` as `.args`.
#'   User-supplied `metric_args` override these via [utils::modifyList()].
#' @param min_len A function `function(args) -> integer` that returns the
#'   advisory minimum window size for the given resolved args. If `NULL`,
#'   defaults to `function(a) 2L`.
#'
#' @return Invisibly `NULL`. Called for side effects.
#' @export
#' @examples
#' my_fn <- function(x, .args = list()) c(my_mean = mean(x, na.rm = TRUE))
#' register_metric("my_mean", my_fn, outputs = "my_mean")
#' available_metrics()
#' # cleanup
#' rm("my_mean", envir = rollNonlinear:::.metric_registry)
register_metric <- function(name, fn, outputs,
                             defaults = list(), min_len = NULL) {
  # --- validate inputs -------------------------------------------------------
  checkmate::assert_string(name, min.chars = 1L)

  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function, not {.cls {class(fn)}}.")
  }
  fn_args <- names(formals(fn))
  if (!all(c("x", ".args") %in% fn_args)) {
    cli::cli_abort(
      "{.arg fn} must have formals {.code x} and {.code .args}."
    )
  }

  out_check <- checkmate::check_character(
    outputs, min.len = 1L, any.missing = FALSE, unique = TRUE
  )
  if (!isTRUE(out_check)) {
    cli::cli_abort(c("{.arg outputs} is invalid.", "x" = "{out_check}"))
  }

  checkmate::assert_list(defaults, names = "unique")

  if (is.null(min_len)) {
    min_len <- function(a) 2L
  } else if (!is.function(min_len)) {
    cli::cli_abort("{.arg min_len} must be a function or NULL.")
  }

  # --- warn if overwriting existing entry ------------------------------------
  if (exists(name, envir = .metric_registry, inherits = FALSE)) {
    cli::cli_inform(
      "Overwriting existing metric {.val {name}} in the registry."
    )
  }

  .register(name, fn, outputs, defaults, min_len)
  invisible(NULL)
}
