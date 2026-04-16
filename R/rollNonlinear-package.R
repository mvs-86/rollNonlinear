#' rollNonlinear: Rolling Nonlinear Time-Series Metrics Engine
#'
#' Computes rolling window nonlinear time-series metrics (Hurst exponent,
#' Detrended Fluctuation Analysis, sample and approximate entropy, Lyapunov
#' exponent, correlation dimension) for one or more symbols in OHLC-style
#' data.tables. Provides an extensible metric registry via
#' [register_metric()], built-in series transforms, and optional parallel
#' execution via [mirai][mirai::mirai] for multi-symbol workloads.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom data.table data.table setkey setkeyv := rbindlist as.data.table is.data.table
#' @importFrom rlang `%||%`
#' @importFrom stats sd setNames
utils::globalVariables(c("start", "end", "valid", "time"))
