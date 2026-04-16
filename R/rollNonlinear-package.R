#' rollNonlinear: Rolling Nonlinear Time-Series Metrics Engine
#'
#' Computes rolling window nonlinear time-series metrics for a single symbol
#' using sequential execution. Supports Hurst exponent estimation and
#' Detrended Fluctuation Analysis (DFA) via the \pkg{nonlinearTseries} package.
#' Designed as Phase 1 of a larger engine; later phases will add multi-symbol
#' dispatch, parallelism, additional metrics, and a metric registry.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom data.table data.table setkey setkeyv :=
#' @importFrom stats sd
utils::globalVariables(c("start", "end", "valid", "time"))
