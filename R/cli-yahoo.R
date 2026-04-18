# ---------------------------------------------------------------------------
# Yahoo Finance CLI helpers
# ---------------------------------------------------------------------------

#' @noRd
.parse_cli_date <- function(x, arg) {
  if (length(x) == 0L || is.na(x) || identical(x, "")) {
    return(NULL)
  }

  out <- suppressWarnings(as.Date(x))
  if (is.na(out)) {
    cli::cli_abort(
      "{.arg {arg}} must be an ISO date like {.val 2024-01-01}; got {.val {x}}."
    )
  }
  out
}

#' @noRd
.coerce_cli_value <- function(x) {
  checkmate::assert_string(x)

  trimmed <- trimws(x)
  if (grepl("^(['\"]).*\\1$", trimmed)) {
    return(substring(trimmed, 2L, nchar(trimmed) - 1L))
  }

  lowered <- tolower(trimmed)
  if (lowered %in% c("true", "false")) {
    return(identical(lowered, "true"))
  }
  if (lowered %in% c("na", "null")) {
    return(NA)
  }
  if (grepl("^[+-]?[0-9]+$", trimmed)) {
    return(as.integer(trimmed))
  }
  if (grepl("^[+-]?([0-9]*\\.[0-9]+|[0-9]+\\.?)([eE][+-]?[0-9]+)?$", trimmed)) {
    return(as.numeric(trimmed))
  }

  trimmed
}

#' @noRd
.parse_metric_args_cli <- function(entries) {
  if (length(entries) == 0L || all(entries == "")) {
    return(list())
  }

  out <- list()
  for (entry in entries) {
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\.([A-Za-z][A-Za-z0-9_]*)=(.*)$", entry)
    parts <- regmatches(entry, m)[[1L]]

    if (length(parts) == 0L) {
      cli::cli_abort(
        c(
          "Invalid {.arg metric_arg} entry {.val {entry}}.",
          "i" = "Use the form {.code metric_name.arg_name=value}."
        )
      )
    }

    metric_name <- parts[[2L]]
    arg_name    <- parts[[3L]]
    arg_value   <- .coerce_cli_value(parts[[4L]])
    current     <- out[[metric_name]] %||% list()
    current[[arg_name]] <- arg_value
    out[[metric_name]]  <- current
  }

  out
}

#' @noRd
.resolve_cli_series <- function(series) {
  checkmate::assert_choice(
    series,
    choices = c("close", "log_returns", "typical", "range")
  )

  switch(
    series,
    close       = "close",
    log_returns = series_log_returns(),
    typical     = series_typical(),
    range       = series_range()
  )
}

#' @noRd
.parse_yahoo_index <- function(x) {
  date_idx <- suppressWarnings(as.Date(x))
  if (!anyNA(date_idx)) {
    return(date_idx)
  }

  posix_idx <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (!anyNA(posix_idx)) {
    return(posix_idx)
  }

  cli::cli_abort("Could not parse Yahoo Finance index values into {.cls Date} or {.cls POSIXct}.")
}

#' @noRd
.normalize_yahoo_ohlc <- function(x, symbol) {
  checkmate::assert_string(symbol, min.chars = 1L)

  dt <- data.table::as.data.table(x, keep.rownames = ".time_raw")
  if (nrow(dt) == 0L) {
    cli::cli_abort("Yahoo Finance returned no rows for symbol {.val {symbol}}.")
  }

  dt[["time"]] <- .parse_yahoo_index(dt[[".time_raw"]])
  dt[[".time_raw"]] <- NULL

  raw_names <- names(dt)
  suffixes  <- tolower(sub("^.*\\.", "", raw_names))
  col_map   <- setNames(raw_names, suffixes)

  required <- c("open", "high", "low", "close")
  missing  <- setdiff(required, names(col_map))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Downloaded data for {.val {symbol}} is missing OHLC column(s): {.val {missing}}."
    )
  }

  out <- data.table::data.table(
    symbol = symbol,
    time   = dt[["time"]],
    open   = as.numeric(dt[[col_map[["open"]]]]),
    high   = as.numeric(dt[[col_map[["high"]]]]),
    low    = as.numeric(dt[[col_map[["low"]]]]),
    close  = as.numeric(dt[[col_map[["close"]]]])
  )

  if ("volume" %in% names(col_map)) {
    out[["volume"]] <- as.numeric(dt[[col_map[["volume"]]]])
  }
  if ("adjusted" %in% names(col_map)) {
    out[["adjusted"]] <- as.numeric(dt[[col_map[["adjusted"]]]])
  }

  data.table::setorderv(out, "time")
  data.table::setkeyv(out, c("symbol", "time"))
  out
}

#' @noRd
.download_yahoo_ohlc <- function(symbols, start = NULL, end = NULL,
                                 downloader = quantmod::getSymbols) {
  symbol_check <- checkmate::check_character(
    symbols,
    min.len = 1L,
    any.missing = FALSE,
    unique = TRUE
  )
  if (!isTRUE(symbol_check)) {
    cli::cli_abort(c("{.arg symbols} is invalid.", "x" = "{symbol_check}"))
  }

  if (!is.null(start) && !inherits(start, "Date")) {
    cli::cli_abort("{.arg start} must be {.cls Date} or NULL.")
  }
  if (!is.null(end) && !inherits(end, "Date")) {
    cli::cli_abort("{.arg end} must be {.cls Date} or NULL.")
  }
  if (!is.null(start) && !is.null(end) && end < start) {
    cli::cli_abort("{.arg end} must be on or after {.arg start}.")
  }

  pb <- cli::cli_progress_bar(
    name  = "Downloading Yahoo Finance OHLC",
    total = length(symbols),
    clear = FALSE
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  tables <- vector("list", length(symbols))

  for (i in seq_along(symbols)) {
    symbol <- symbols[[i]]
    raw <- tryCatch(
      downloader(
        Symbols     = symbol,
        src         = "yahoo",
        from        = start,
        to          = end,
        auto.assign = FALSE,
        warnings    = FALSE
      ),
      error = function(e) {
        structure(
          list(message = conditionMessage(e)),
          class = "rollnonlinear_download_error"
        )
      }
    )

    if (inherits(raw, "rollnonlinear_download_error")) {
      cli::cli_abort(
        c(
          "Failed to download Yahoo Finance data for symbol {.val {symbol}}.",
          "x" = "{raw$message}"
        )
      )
    }

    tables[[i]] <- .normalize_yahoo_ohlc(raw, symbol = symbol)
    cli::cli_progress_update(id = pb)
  }

  out <- data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)
  data.table::setkeyv(out, c("symbol", "time"))
  out
}

#' @noRd
.write_cli_output <- function(x, path) {
  checkmate::assert_string(path, min.chars = 1L)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sep <- if (tolower(tools::file_ext(path)) == "tsv") "\t" else ","
  data.table::fwrite(x, path, sep = sep)
  invisible(path)
}

#' @noRd
.cli_roll_yahoo_main <- function() {
  if (!requireNamespace("GetoptLong", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The Yahoo Finance CLI requires {.pkg GetoptLong}.",
        "i" = "Install it with {.code install.packages('GetoptLong')}."
      )
    )
  }

  opt <- new.env(parent = emptyenv())
  opt$start       <- format(Sys.Date() - 365, "%Y-%m-%d")
  opt$end         <- format(Sys.Date(), "%Y-%m-%d")
  opt$window_size <- 128L
  opt$stride      <- 1L
  opt$min_obs     <- NA_integer_
  opt$metrics     <- c("hurst", "dfa")
  opt$series      <- "close"
  opt$metric_arg  <- character(0L)
  opt$on_error    <- "warn_na"
  opt$parallel    <- FALSE
  opt$n_workers   <- NA_integer_
  opt$output      <- "rolling_nonlinear_metrics.csv"

  GetoptLong::GetoptLong.options(help_style = "two-column")
  GetoptLong::GetoptLong(
    "symbols=s@",     "One or more Yahoo Finance symbols. [mandatory]",
    "start=s",        "Download start date in YYYY-MM-DD format.",
    "end=s",          "Download end date in YYYY-MM-DD format.",
    "window_size=i",  "Rolling window size. [default: 128]",
    "stride=i",       "Rolling stride / step size. [default: 1]",
    "min_obs=i",      "Minimum observations required per window. Defaults to window size.",
    "metrics=s@",     "Metric names to compute. [default: hurst dfa]",
    "series=s",       "Series transform: close, log_returns, typical, range. [default: close]",
    "metric_arg=s@",  "Metric override(s) as metric.arg=value. Repeat as needed.",
    "on_error=s",     "Window error policy: warn_na, stop, silent_na. [default: warn_na]",
    "parallel!",      "Enable mirai-based parallel execution across symbols.",
    "n_workers=i",    "Number of mirai workers when --parallel is used.",
    "output=s",       "Output file path (.csv or .tsv). [default: rolling_nonlinear_metrics.csv]",
    envir     = opt,
    help_head = "Download Yahoo Finance OHLC data and compute rolling nonlinear metrics.",
    help_foot = paste(
      "Example:",
      "Rscript inst/scripts/roll-nonlinear-yahoo.R --symbols AAPL MSFT",
      "--start 2024-01-01 --end 2024-12-31 --window-size 128 --stride 16",
      "--metrics hurst dfa --series log_returns",
      "--metric-arg sample_entropy.dimension=3 --output output/metrics.csv"
    )
  )

  start_date  <- .parse_cli_date(opt$start, "start")
  end_date    <- .parse_cli_date(opt$end, "end")
  min_obs     <- if (is.na(opt$min_obs)) opt$window_size else opt$min_obs
  metric_args <- .parse_metric_args_cli(opt$metric_arg)
  series      <- .resolve_cli_series(opt$series)
  n_workers   <- if (is.na(opt$n_workers)) NULL else opt$n_workers

  data <- .download_yahoo_ohlc(
    symbols = opt$symbols,
    start   = start_date,
    end     = end_date
  )

  out <- roll_nonlinear(
    data        = data,
    metrics     = opt$metrics,
    window_size = opt$window_size,
    stride      = opt$stride,
    series      = series,
    symbol_col  = "symbol",
    time_col    = "time",
    min_obs     = min_obs,
    metric_args = metric_args,
    on_error    = opt$on_error,
    parallel    = isTRUE(opt$parallel),
    n_workers   = n_workers
  )

  .write_cli_output(out, opt$output)

  cli::cli_inform(c(
    "i" = "Downloaded {length(unique(data[['symbol']]))} symbol{?s}.",
    "i" = "Wrote {nrow(out)} rolling row{?s} to {.file {opt$output}}."
  ))

  invisible(out)
}
