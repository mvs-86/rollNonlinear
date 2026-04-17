# ---------------------------------------------------------------------------
# Plotting utilities for roll_nonlinear() output
# ---------------------------------------------------------------------------

#' @importFrom rlang .data
NULL

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ggplot2} is required for {.fn rn_plot}.",
        "i" = "Install it with: {.code install.packages('ggplot2')}"
      )
    )
  }
}

#' @noRd
.check_patchwork <- function() {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg patchwork} is required for {.code type = \"panel\"}.",
        "i" = "Install it with: {.code install.packages('patchwork')}"
      )
    )
  }
}

#' @noRd
.detect_metric_cols <- function(result, symbol_col, time_col) {
  exclude <- c(symbol_col, time_col, "window_start_idx", "window_end_idx")
  setdiff(names(result), exclude)
}

#' @noRd
.melt_metrics <- function(result, metrics, symbol_col, time_col) {
  has_symbol <- symbol_col %in% names(result)
  id_vars    <- if (has_symbol) c(symbol_col, time_col) else time_col

  long <- data.table::melt(
    result,
    id.vars       = id_vars,
    measure.vars  = metrics,
    variable.name = "metric_name",
    value.name    = "value"
  )
  long[["metric_name"]] <- as.character(long[["metric_name"]])

  if (!has_symbol) {
    long[["symbol"]] <- "(single)"
  } else if (symbol_col != "symbol") {
    data.table::setnames(long, symbol_col, "symbol")
  }
  # Normalise time column to "time" so plot helpers always use that name
  if (time_col != "time") {
    data.table::setnames(long, time_col, "time")
  }
  long
}

# Built-in regime threshold defaults
.auto_regime_thresholds <- list(
  hurst     = list(lo = 0.45, hi = 0.55,
                   labels = c("mean-rev", "random", "trending")),
  dfa_alpha = list(lo = 0.45, hi = 0.55,
                   labels = c("mean-rev", "random", "trending"))
)

#' @noRd
.regime_band_layers <- function(metric_name, thresholds) {
  spec <- thresholds[[metric_name]]
  if (is.null(spec)) return(list())

  lo <- spec[["lo"]]
  hi <- spec[["hi"]]

  list(
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = lo,
      fill = "#4393c3", alpha = 0.08
    ),
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = hi, ymax = Inf,
      fill = "#d6604d", alpha = 0.08
    ),
    ggplot2::geom_hline(yintercept = lo, linetype = "dashed",
                        colour = "#4393c3", linewidth = 0.4),
    ggplot2::geom_hline(yintercept = hi, linetype = "dashed",
                        colour = "#d6604d", linewidth = 0.4)
  )
}

#' @noRd
.resolve_thresholds <- function(regime_thresholds, metrics) {
  if (is.null(regime_thresholds)) return(NULL)
  if (identical(regime_thresholds, "auto")) {
    return(.auto_regime_thresholds[intersect(metrics, names(.auto_regime_thresholds))])
  }
  if (is.list(regime_thresholds)) {
    bad <- setdiff(names(regime_thresholds), metrics)
    if (length(bad) > 0L) {
      cli::cli_warn(
        "Names in {.arg regime_thresholds} not in {.arg metrics}: {.val {bad}}. Ignored."
      )
    }
    return(regime_thresholds[intersect(names(regime_thresholds), metrics)])
  }
  cli::cli_abort(
    "{.arg regime_thresholds} must be {.val NULL}, {.val \"auto\"}, or a named list."
  )
}

# ---------------------------------------------------------------------------
# type = "time_series"
# ---------------------------------------------------------------------------

#' @noRd
.plot_time_series <- function(long, metrics, thresholds, facet_scales, ncol, title) {
  gg <- ggplot2::ggplot(long,
    ggplot2::aes(
      x      = .data[["time"]],
      y      = .data[["value"]],
      colour = .data[["symbol"]]
    )
  )

  # regime bands — applied per metric facet via geom layers (global for single metric)
  if (!is.null(thresholds) && length(metrics) == 1L) {
    gg <- gg + .regime_band_layers(metrics, thresholds)
  }

  gg <- gg + ggplot2::geom_line(linewidth = 0.5)

  if (length(metrics) > 1L) {
    gg <- gg + ggplot2::facet_wrap(
      ggplot2::vars(.data[["metric_name"]]),
      scales = facet_scales,
      ncol   = ncol
    )
  }

  gg <- gg +
    ggplot2::labs(
      x      = "Time",
      y      = if (length(metrics) == 1L) metrics else "Value",
      colour = "Symbol",
      title  = title
    ) +
    ggplot2::theme_bw()

  gg
}

# ---------------------------------------------------------------------------
# type = "symbols"
# ---------------------------------------------------------------------------

#' @noRd
.plot_symbols <- function(long, metric, thresholds, facet_scales, ncol, title) {
  n_sym <- data.table::uniqueN(long[["symbol"]])
  if (n_sym > 12L) {
    cli::cli_warn(
      "There are {n_sym} symbols - the faceted plot may be crowded. \\
      Consider filtering to a subset."
    )
  }

  gg <- ggplot2::ggplot(long,
    ggplot2::aes(x = .data[["time"]], y = .data[["value"]])
  )

  if (!is.null(thresholds)) {
    gg <- gg + .regime_band_layers(metric, thresholds)
  }

  gg <- gg +
    ggplot2::geom_line(linewidth = 0.5, colour = "#2166ac") +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["symbol"]]),
      scales = facet_scales,
      ncol   = ncol
    ) +
    ggplot2::labs(
      x     = "Time",
      y     = metric,
      title = title
    ) +
    ggplot2::theme_bw()

  gg
}

# ---------------------------------------------------------------------------
# type = "distribution"
# ---------------------------------------------------------------------------

#' @noRd
.plot_distribution <- function(long, metrics, facet_scales, ncol, title) {
  gg <- ggplot2::ggplot(long,
    ggplot2::aes(x = .data[["value"]], fill = .data[["symbol"]])
  ) +
    ggplot2::geom_histogram(position = "identity", alpha = 0.5, bins = 30L) +
    ggplot2::labs(
      x     = "Value",
      y     = "Count",
      fill  = "Symbol",
      title = title
    ) +
    ggplot2::theme_bw()

  if (length(metrics) > 1L) {
    gg <- gg + ggplot2::facet_wrap(
      ggplot2::vars(.data[["metric_name"]]),
      scales = facet_scales,
      ncol   = ncol
    )
  }

  gg
}

# ---------------------------------------------------------------------------
# type = "panel"
# ---------------------------------------------------------------------------

#' @noRd
.plot_panel <- function(result, long, metrics, original_data,
                        price_col, time_col, symbol_col,
                        thresholds, facet_scales, ncol, title) {

  .check_patchwork()

  # single symbol guard
  sym_vals <- if (symbol_col %in% names(result)) unique(result[[symbol_col]]) else "(single)"
  if (length(sym_vals) > 1L) {
    cli::cli_abort(
      c(
        "{.code type = \"panel\"} requires a single-symbol {.arg result}.",
        "i" = "Filter to one symbol before calling {.fn rn_plot}."
      )
    )
  }

  if (is.null(original_data)) {
    cli::cli_abort(
      c(
        "{.code type = \"panel\"} requires {.arg original_data}.",
        "i" = "Pass the original OHLC data.table used to produce {.arg result}."
      )
    )
  }

  if (!price_col %in% names(original_data)) {
    cli::cli_abort(
      "Column {.val {price_col}} not found in {.arg original_data}."
    )
  }

  # shared x limits
  t_range <- range(c(original_data[[time_col]], result[[time_col]]), na.rm = TRUE)

  price_dt <- if (!data.table::is.data.table(original_data)) {
    data.table::as.data.table(original_data)
  } else {
    data.table::copy(original_data)
  }
  # Normalise time column to "time" in price_dt for consistent aes mapping
  if (time_col != "time" && time_col %in% names(price_dt)) {
    data.table::setnames(price_dt, time_col, "time")
  }

  p_price <- ggplot2::ggplot(price_dt,
    ggplot2::aes(x = .data[["time"]], y = .data[[price_col]])
  ) +
    ggplot2::geom_line(linewidth = 0.4, colour = "grey30") +
    ggplot2::coord_cartesian(xlim = t_range) +
    ggplot2::labs(x = NULL, y = price_col, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  # metrics panel (time_series style, no legend)
  p_metrics <- .plot_time_series(long, metrics, thresholds, facet_scales, ncol, title = NULL) +
    ggplot2::coord_cartesian(xlim = t_range) +
    ggplot2::theme(legend.position = "none")

  patchwork::wrap_plots(
    list(p_price, p_metrics),
    ncol    = 1L,
    heights = c(1L, max(1L, length(metrics)))
  )
}

# ---------------------------------------------------------------------------
# Exported: rn_plot()
# ---------------------------------------------------------------------------

#' Plot the output of `roll_nonlinear()`
#'
#' Visualises rolling nonlinear metric results using **ggplot2**. Four display
#' types are available:
#'
#' \describe{
#'   \item{`"time_series"`}{One line per symbol over time, faceted by metric
#'     when multiple metrics are requested.}
#'   \item{`"symbols"`}{Single metric across all symbols, each symbol in its
#'     own facet panel.}
#'   \item{`"distribution"`}{Histogram of metric values per symbol, faceted by
#'     metric when multiple metrics are requested.}
#'   \item{`"panel"`}{Price series on top + metric(s) below in a stacked
#'     layout (requires **patchwork** and `original_data`).}
#' }
#'
#' @param result A `data.table` returned by [roll_nonlinear()] or
#'   [roll_nonlinear_one()].
#' @param metrics Character vector of metric column names to plot. `NULL`
#'   (default) uses all metric columns detected in `result`.
#' @param type Display type: one of `"time_series"` (default), `"symbols"`,
#'   `"distribution"`, or `"panel"`.
#' @param symbol_col Name of the symbol column in `result`. Default: `"symbol"`.
#' @param time_col Name of the time column in `result`. Default: `"time"`.
#' @param regime_thresholds `NULL` (no overlay), `"auto"` (built-in thresholds
#'   for `hurst` and `dfa_alpha`), or a named list of the form
#'   `list(metric_name = list(lo = <num>, hi = <num>))` for custom horizontal
#'   reference bands.
#' @param original_data A `data.table` of the original OHLC data used to
#'   produce `result`. Required when `type = "panel"`.
#' @param price_col Column name for the price series in `original_data`.
#'   Default: `"close"`.
#' @param facet_scales Passed to [ggplot2::facet_wrap()] `scales` argument.
#'   Default: `"free_y"`.
#' @param ncol Number of facet columns. `NULL` (default) lets ggplot2 decide.
#' @param title Optional plot title string.
#'
#' @return A `ggplot` object (`type %in% c("time_series","symbols","distribution")`)
#'   or a `patchwork` object (`type = "panel"`). Both can be further customised
#'   with standard ggplot2 layers and themes.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(data.table)
#'
#' set.seed(42L)
#' n <- 200L
#' dt <- data.table(
#'   symbol = rep(c("A", "B"), each = n),
#'   time   = c(
#'     seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = n),
#'     seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = n)
#'   ),
#'   close  = c(cumsum(rnorm(n)) + 100, cumsum(rnorm(n)) + 200)
#' )
#'
#' result <- roll_nonlinear(dt, metrics = c("hurst", "dfa"),
#'                          window_size = 64L, stride = 8L)
#'
#' # Time-series of both metrics, one line per symbol
#' rn_plot(result, type = "time_series")
#'
#' # Regime threshold overlay (built-in for hurst / dfa_alpha)
#' rn_plot(result, metrics = "hurst", type = "time_series",
#'         regime_thresholds = "auto")
#'
#' # All symbols in separate facets for a single metric
#' rn_plot(result, metrics = "hurst", type = "symbols")
#'
#' # Distribution of metric values
#' rn_plot(result, type = "distribution")
#'
#' # Stacked price + metric panel (single symbol, requires patchwork)
#' single <- dt[symbol == "A"]
#' result_a <- roll_nonlinear(single, metrics = "hurst",
#'                            window_size = 64L, stride = 8L)
#' rn_plot(result_a, type = "panel", original_data = single,
#'         title = "Symbol A — Hurst exponent")
#' }
rn_plot <- function(
    result,
    metrics           = NULL,
    type              = c("time_series", "symbols", "distribution", "panel"),
    symbol_col        = "symbol",
    time_col          = "time",
    regime_thresholds = NULL,
    original_data     = NULL,
    price_col         = "close",
    facet_scales      = "free_y",
    ncol              = NULL,
    title             = NULL
) {
  .check_ggplot2()

  type <- match.arg(type)

  if (!data.table::is.data.table(result)) {
    result <- data.table::as.data.table(result)
  }

  # resolve metrics
  if (is.null(metrics)) {
    metrics <- .detect_metric_cols(result, symbol_col, time_col)
  }
  if (length(metrics) == 0L) {
    cli::cli_abort("No metric columns found in {.arg result}.")
  }
  missing_cols <- setdiff(metrics, names(result))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Metric column(s) not found in {.arg result}: {.val {missing_cols}}.")
  }

  # "symbols" requires exactly one metric
  if (type == "symbols" && length(metrics) != 1L) {
    cli::cli_abort(
      c(
        '{.code type = "symbols"} requires exactly one metric.',
        "i" = "Pass a single metric name via the {.arg metrics} argument."
      )
    )
  }

  thresholds <- .resolve_thresholds(regime_thresholds, metrics)
  long       <- .melt_metrics(result, metrics, symbol_col, time_col)

  switch(type,
    time_series  = .plot_time_series(long, metrics, thresholds, facet_scales, ncol, title),
    symbols      = .plot_symbols(long, metrics, thresholds, facet_scales, ncol, title),
    distribution = .plot_distribution(long, metrics, facet_scales, ncol, title),
    panel        = .plot_panel(result, long, metrics, original_data,
                               price_col, time_col, symbol_col,
                               thresholds, facet_scales, ncol, title)
  )
}
