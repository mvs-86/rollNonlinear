# ---------------------------------------------------------------------------
# Parallel execution infrastructure
# ---------------------------------------------------------------------------
# Provides:
#   .choose_strategy()   - sequential vs parallel decision
#   .ensure_daemons()    - start mirai daemons if none running
#   .release_daemons()   - stop daemons that *we* started
#   .roll_sequential()   - sequential loop with progress bar
#   .roll_parallel()     - parallel dispatch via mirai
#   .collect_with_progress() - collect mirai tasks with progress
#   rn_daemons()         - exported helper to query/set daemons
#   rn_stop_daemons()    - exported helper to stop daemons
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Strategy selection
# ---------------------------------------------------------------------------

#' @noRd
.choose_strategy <- function(parallel, n_chunks) {
  if (!isTRUE(parallel)) {
    return("sequential")
  }
  if (n_chunks <= 1L) {
    return("sequential")
  }
  if (!requireNamespace("mirai", quietly = TRUE)) {
    cli::cli_warn(
      "{.pkg mirai} is not installed; falling back to sequential execution."
    )
    return("sequential")
  }
  "parallel"
}

# ---------------------------------------------------------------------------
# Daemon lifecycle
# ---------------------------------------------------------------------------

#' @noRd
.ensure_daemons <- function(n_workers) {
  current <- tryCatch(
    mirai::status()$connections,
    error = function(e) 0L
  )

  if (current > 0L) {
    return(list(owned = FALSE, n = current))
  }

  n <- if (is.null(n_workers)) {
    max(1L, parallelly::availableCores(omit = 1L))
  } else {
    as.integer(n_workers)
  }

  mirai::daemons(n)
  list(owned = TRUE, n = n)
}

#' @noRd
.release_daemons <- function(state) {
  if (isTRUE(state$owned)) {
    try(mirai::daemons(0L), silent = TRUE)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Exported daemon helpers
# ---------------------------------------------------------------------------

#' Query or set mirai daemons
#'
#' @title Query or set mirai daemons
#' @description
#' Convenience wrapper around [mirai::daemons()] and [mirai::status()].
#' With no arguments, returns the current daemon status. With an integer
#' argument, sets the number of daemons and returns `n` invisibly.
#'
#' @param n `NULL` (default) to query status, or a positive integer to set
#'   the number of local daemons.
#'
#' @return When `n = NULL`, the list returned by [mirai::status()].
#'   Otherwise, `n` (invisibly).
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mirai", quietly = TRUE)) {
#'   rn_daemons(2L)   # start 2 daemons
#'   rn_daemons()     # query status
#'   rn_stop_daemons()
#' }
#' }
rn_daemons <- function(n = NULL) {
  if (is.null(n)) {
    return(mirai::status())
  }
  mirai::daemons(as.integer(n))
  invisible(n)
}

#' Stop mirai daemons
#'
#' @title Stop mirai daemons
#' @description
#' Calls `mirai::daemons(0L)` to stop all currently running local daemons.
#' Safe to call even when no daemons are running.
#'
#' @return `NULL` invisibly.
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mirai", quietly = TRUE)) {
#'   rn_daemons(2L)
#'   rn_stop_daemons()
#' }
#' }
rn_stop_daemons <- function() {
  mirai::daemons(0L)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Sequential execution path (extracted from Phase 2)
# ---------------------------------------------------------------------------

#' @noRd
.roll_sequential <- function(chunks, metrics, window_size, stride, series,
                              time_col, symbol_col, min_obs, metric_args,
                              on_error) {
  pb <- cli::cli_progress_bar(
    name  = "Processing symbols",
    total = length(chunks),
    clear = FALSE
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  results   <- vector("list", length(chunks))
  sym_names <- names(chunks)

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    sym   <- sym_names[[i]]

    # validate time monotonicity
    t_num <- as.numeric(chunk[[time_col]])
    if (length(t_num) >= 2L && any(diff(t_num) <= 0)) {
      cli::cli_abort(
        "Time column {.val {time_col}} is not strictly increasing \\
        for symbol {.val {sym}}."
      )
    }

    x   <- resolve_series(chunk, series)
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
    results[[i]] <- res

    cli::cli_progress_update(id = pb)
  }

  results
}

# ---------------------------------------------------------------------------
# Parallel execution path
# ---------------------------------------------------------------------------

#' @noRd
.roll_parallel <- function(chunks, metrics, window_size, stride, series,
                            time_col, symbol_col, min_obs, metric_args,
                            on_error, n_workers) {
  daemon_state <- .ensure_daemons(n_workers)
  on.exit(.release_daemons(daemon_state), add = TRUE)

  series_spec  <- .freeze_series(series)
  n_chunks     <- length(chunks)
  sym_names    <- names(chunks)

  # Capture worker function as data (avoids ::: in source, serializes cleanly)
  wfn <- .worker_roll_one

  # Capture metric specs so workers can register any user-added metrics
  metric_specs <- setNames(lapply(metrics, get_metric), metrics)

  # Submit tasks
  tasks <- vector("list", n_chunks)
  for (i in seq_len(n_chunks)) {
    chunk <- chunks[[i]]
    tasks[[i]] <- mirai::mirai(
      wfn(
        chunk        = chunk,
        metrics      = metrics,
        window_size  = window_size,
        stride       = stride,
        series_spec  = series_spec,
        time_col     = time_col,
        symbol_col   = symbol_col,
        min_obs      = min_obs,
        metric_args  = metric_args,
        on_error     = on_error,
        metric_specs = metric_specs
      ),
      wfn          = wfn,
      chunk        = chunk,
      metrics      = metrics,
      window_size  = window_size,
      stride       = stride,
      series_spec  = series_spec,
      time_col     = time_col,
      symbol_col   = symbol_col,
      min_obs      = min_obs,
      metric_args  = metric_args,
      on_error     = on_error,
      metric_specs = metric_specs
    )
  }

  .collect_with_progress(tasks, sym_names)
}

# ---------------------------------------------------------------------------
# Task collection with progress bar
# ---------------------------------------------------------------------------

#' @noRd
.collect_with_progress <- function(tasks, sym_names) {
  n      <- length(tasks)
  pb     <- cli::cli_progress_bar("Symbols", total = n, clear = FALSE)
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  results   <- vector("list", n)
  remaining <- seq_len(n)

  while (length(remaining) > 0L) {
    newly_done <- integer(0L)
    for (i in remaining) {
      if (!mirai::unresolved(tasks[[i]])) {
        newly_done <- c(newly_done, i)
      }
    }

    for (i in newly_done) {
      val <- tasks[[i]]$data
      if (inherits(val, "mirai_error") || inherits(val, "errorValue")) {
        cli::cli_abort(
          c(
            "Worker failed for symbol {.val {sym_names[[i]]}} (chunk {i}).",
            "x" = conditionMessage(val)
          )
        )
      }
      results[[i]] <- val
      cli::cli_progress_update(id = pb)
    }

    remaining <- setdiff(remaining, newly_done)
    if (length(remaining) > 0L) {
      Sys.sleep(0.05)
    }
  }

  results
}
