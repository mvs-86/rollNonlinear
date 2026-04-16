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
                              on_error, metric_specs = list()) {
  # Register any custom/extra metrics that aren't in this worker's registry yet
  for (nm in names(metric_specs)) {
    spec <- metric_specs[[nm]]
    .register(nm, spec$fn, spec$outputs, spec$defaults, spec$min_len)
  }

  series_fn <- .thaw_series(series_spec)
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

  res <- roll_nonlinear_one(
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
