# ---------------------------------------------------------------------------
# Worker: serialization and per-chunk execution
# ---------------------------------------------------------------------------
# Functions:
#   .freeze_series(series)       - serialize a series spec for transport
#   .thaw_series(spec)           - reconstruct series function from spec
#   .strip_env(f)                - strip closure environment for safety
#   .worker_roll_one(...)        - per-symbol computation (runs in worker)
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Environment stripping
# ---------------------------------------------------------------------------

#' @noRd
.strip_env <- function(f) {
  environment(f) <- baseenv()
  f
}

# ---------------------------------------------------------------------------
# Series freeze / thaw
# ---------------------------------------------------------------------------

#' @noRd
.freeze_series <- function(series) {
  if (is.character(series) && length(series) == 1L) {
    return(list(kind = "col", value = series))
  }
  if (is.function(series)) {
    # Store as-is; mirai serializes functions with their closure envs correctly
    # since workers have the package loaded.
    return(list(kind = "fn", value = series))
  }
  cli::cli_abort(
    "{.arg series} must be a single string (column name) or a function; \\
    got {.cls {class(series)}}."
  )
}

#' @noRd
.thaw_series <- function(spec) {
  if (spec$kind == "col") {
    col <- spec$value
    function(dt) dt[[col]]
  } else {
    spec$value
  }
}

# ---------------------------------------------------------------------------
# Worker payload
# ---------------------------------------------------------------------------

#' @noRd
.worker_roll_one <- function(chunk, metrics, window_size, stride, series_spec,
                             time_col, symbol_col, min_obs, metric_args,
                             on_error, metric_specs = list(),
                             source_path = NULL,
                             register_metric_fn = NULL,
                             thaw_series_fn = NULL,
                             roll_one_fn = NULL) {
  if (!"rollNonlinear" %in% loadedNamespaces()) {
    if (requireNamespace("rollNonlinear", quietly = TRUE)) {
      loadNamespace("rollNonlinear")
    } else if (!is.null(source_path) &&
               file.exists(file.path(source_path, "DESCRIPTION")) &&
               requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(source_path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    } else {
      cli::cli_abort(
        "Worker could not load the {.pkg rollNonlinear} namespace."
      )
    }
  }

  worker_ns <- asNamespace("rollNonlinear")
  register_metric_internal <- register_metric_fn %||%
    get(".register", envir = worker_ns, inherits = FALSE)
  thaw_series_internal <- thaw_series_fn %||%
    get(".thaw_series", envir = worker_ns, inherits = FALSE)
  roll_one_internal <- roll_one_fn %||%
    get("roll_nonlinear_one", envir = worker_ns, inherits = FALSE)

  # Register any custom/extra metrics that aren't in this worker's registry yet
  for (nm in names(metric_specs)) {
    spec <- metric_specs[[nm]]
    register_metric_internal(
      nm,
      spec$fn,
      spec$outputs,
      spec$defaults,
      spec$min_len
    )
  }

  series_fn <- thaw_series_internal(series_spec)
  sym       <- chunk[[symbol_col]][1L]

  # validate time monotonicity
  t_num <- as.numeric(chunk[[time_col]])
  if (length(t_num) >= 2L && any(diff(t_num) <= 0)) {
    cli::cli_abort(
      "Time column {.val {time_col}} is not strictly increasing \\
      for symbol {.val {sym}}."
    )
  }

  x <- series_fn(chunk)

  res <- roll_one_internal(
    x           = x,
    times       = chunk[[time_col]],
    metrics     = metrics,
    window_size = window_size,
    stride      = stride,
    min_obs     = min_obs,
    on_error    = on_error,
    metric_args = metric_args
  )

  res[[symbol_col]] <- sym
  data.table::setcolorder(res, c(symbol_col, setdiff(names(res), symbol_col)))
  res
}
