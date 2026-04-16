# ---------------------------------------------------------------------------
# Series transforms
# ---------------------------------------------------------------------------
# Built-in constructors return functions of a data.table (or data.frame).
# Each returned function:
#   - Accepts a data.table and returns a numeric vector of length nrow(dt).
#   - Validates required columns exist; aborts via cli::cli_abort() if missing.
#   - Carries only the minimum needed state in its closure (no heavy objects).
#     The closure env has the package namespace as parent for Phase 3 safety.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Internal: build a clean closure environment
# ---------------------------------------------------------------------------

#' @noRd
.make_fn_env <- function(...) {
  e <- new.env(parent = asNamespace("rollNonlinear"))
  args <- list(...)
  for (nm in names(args)) e[[nm]] <- args[[nm]]
  e
}

# ---------------------------------------------------------------------------
# series_close
# ---------------------------------------------------------------------------

#' Extract a single column as the series
#'
#' Returns a function that extracts column `col` from a `data.table` or
#' data.frame as a numeric vector.
#'
#' @param col String. Column name. Default: `"close"`.
#' @return A function `function(dt) -> numeric`.
#' @export
#' @examples
#' dt <- data.table::data.table(close = c(1.0, 2.0, 3.0))
#' fn <- series_close()
#' fn(dt)
series_close <- function(col = "close") {
  checkmate::assert_string(col, min.chars = 1L)
  e  <- .make_fn_env(col = col)
  fn <- function(dt) {
    if (!col %in% names(dt)) {
      cli::cli_abort("Column {.val {col}} not found in data.")
    }
    as.numeric(dt[[col]])
  }
  environment(fn) <- e
  fn
}

# ---------------------------------------------------------------------------
# series_log_returns
# ---------------------------------------------------------------------------

#' Log-returns of a price column
#'
#' Returns a function that computes `diff(log(x))` on column `col`.
#' The first element of the returned vector is `NA_real_`.
#'
#' @param col String. Column name. Default: `"close"`.
#' @return A function `function(dt) -> numeric` of length `nrow(dt)`.
#' @export
#' @examples
#' dt <- data.table::data.table(close = c(100, 101, 102, 103))
#' fn <- series_log_returns()
#' fn(dt)
series_log_returns <- function(col = "close") {
  checkmate::assert_string(col, min.chars = 1L)
  e  <- .make_fn_env(col = col)
  fn <- function(dt) {
    if (!col %in% names(dt)) {
      cli::cli_abort("Column {.val {col}} not found in data.")
    }
    x <- as.numeric(dt[[col]])
    c(NA_real_, diff(log(x)))
  }
  environment(fn) <- e
  fn
}

# ---------------------------------------------------------------------------
# series_typical
# ---------------------------------------------------------------------------

#' Typical price: (High + Low + Close) / 3
#'
#' Returns a function that computes the typical price `(H + L + C) / 3`.
#'
#' @param high  String. High column name. Default: `"high"`.
#' @param low   String. Low column name.  Default: `"low"`.
#' @param close String. Close column name. Default: `"close"`.
#' @return A function `function(dt) -> numeric` of length `nrow(dt)`.
#' @export
#' @examples
#' dt <- data.table::data.table(high = c(10, 20), low = c(8, 18), close = c(9, 19))
#' fn <- series_typical()
#' fn(dt)
series_typical <- function(high = "high", low = "low", close = "close") {
  checkmate::assert_string(high,  min.chars = 1L)
  checkmate::assert_string(low,   min.chars = 1L)
  checkmate::assert_string(close, min.chars = 1L)
  e  <- .make_fn_env(high = high, low = low, close = close)
  fn <- function(dt) {
    missing_cols <- setdiff(c(high, low, close), names(dt))
    if (length(missing_cols) > 0L) {
      cli::cli_abort(
        "Required column(s) not found in data: {.val {missing_cols}}."
      )
    }
    (as.numeric(dt[[high]]) + as.numeric(dt[[low]]) +
       as.numeric(dt[[close]])) / 3
  }
  environment(fn) <- e
  fn
}

# ---------------------------------------------------------------------------
# series_range
# ---------------------------------------------------------------------------

#' Price range: High - Low
#'
#' Returns a function that computes `High - Low`.
#'
#' @param high String. High column name. Default: `"high"`.
#' @param low  String. Low column name.  Default: `"low"`.
#' @return A function `function(dt) -> numeric` of length `nrow(dt)`.
#' @export
#' @examples
#' dt <- data.table::data.table(high = c(10, 20), low = c(8, 18))
#' fn <- series_range()
#' fn(dt)
series_range <- function(high = "high", low = "low") {
  checkmate::assert_string(high, min.chars = 1L)
  checkmate::assert_string(low,  min.chars = 1L)
  e  <- .make_fn_env(high = high, low = low)
  fn <- function(dt) {
    missing_cols <- setdiff(c(high, low), names(dt))
    if (length(missing_cols) > 0L) {
      cli::cli_abort(
        "Required column(s) not found in data: {.val {missing_cols}}."
      )
    }
    as.numeric(dt[[high]]) - as.numeric(dt[[low]])
  }
  environment(fn) <- e
  fn
}

# ---------------------------------------------------------------------------
# resolve_series: internal dispatcher
# ---------------------------------------------------------------------------

#' Resolve a series specification against a data.table
#'
#' @param dt A data.table or data.frame.
#' @param series Either a string (column name) or a function
#'   `function(dt) -> numeric`.
#'
#' @return Numeric vector of length `nrow(dt)`.
#' @noRd
resolve_series <- function(dt, series) {
  if (is.character(series)) {
    if (length(series) != 1L) {
      cli::cli_abort(
        "{.arg series} string must be a scalar column name, \\
        not length {length(series)}."
      )
    }
    if (!series %in% names(dt)) {
      cli::cli_abort(
        "Column {.val {series}} not found in data. \\
        Available: {.val {names(dt)}}."
      )
    }
    return(as.numeric(dt[[series]]))
  }

  if (is.function(series)) {
    out <- series(dt)
    if (!is.numeric(out)) {
      cli::cli_abort(
        "The {.arg series} function must return a numeric vector, \\
        not {.cls {class(out)}}."
      )
    }
    if (length(out) != nrow(dt)) {
      cli::cli_abort(
        "The {.arg series} function must return a vector of length \\
        {nrow(dt)}, not {length(out)}."
      )
    }
    return(out)
  }

  cli::cli_abort(
    "{.arg series} must be a string or a function, \\
    not {.cls {class(series)}}."
  )
}
