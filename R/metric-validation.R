# ---------------------------------------------------------------------------
# Internal metric-validation helpers
# ---------------------------------------------------------------------------
# These helpers support package-internal validation of metric wrappers against
# synthetic series with explicit expectations.
# ---------------------------------------------------------------------------

#' @noRd
.metric_validation_expect_tolerance <- function(target, tolerance,
                                                allow_na = FALSE) {
  if (!is.numeric(target) || length(target) != 1L || is.na(target)) {
    cli::cli_abort("{.arg target} must be a single non-missing numeric value.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) ||
      tolerance < 0) {
    cli::cli_abort(
      "{.arg tolerance} must be a single non-negative numeric value."
    )
  }
  checkmate::assert_flag(allow_na)

  list(
    type      = "tolerance",
    target    = as.numeric(target),
    tolerance = as.numeric(tolerance),
    allow_na  = allow_na
  )
}

#' @noRd
.metric_validation_expect_range <- function(lower = -Inf, upper = Inf,
                                            allow_na = FALSE) {
  if (!is.numeric(lower) || length(lower) != 1L || is.na(lower)) {
    cli::cli_abort("{.arg lower} must be a single non-missing numeric value.")
  }
  if (!is.numeric(upper) || length(upper) != 1L || is.na(upper)) {
    cli::cli_abort("{.arg upper} must be a single non-missing numeric value.")
  }
  if (lower > upper) {
    cli::cli_abort("{.arg lower} must be <= {.arg upper}.")
  }
  checkmate::assert_flag(allow_na)

  list(
    type     = "range",
    lower    = as.numeric(lower),
    upper    = as.numeric(upper),
    allow_na = allow_na
  )
}

#' @noRd
.metric_validation_expect_predicate <- function(fn, label,
                                                allow_na = FALSE) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function.")
  }
  checkmate::assert_string(label, min.chars = 1L)
  checkmate::assert_flag(allow_na)

  list(
    type     = "predicate",
    fn       = fn,
    label    = label,
    allow_na = allow_na
  )
}

#' @noRd
.metric_validation_case <- function(name, metric, generator, expectations,
                                    metric_args = list()) {
  checkmate::assert_string(name, min.chars = 1L)
  checkmate::assert_string(metric, min.chars = 1L)

  if (!is.function(generator) && !is.numeric(generator)) {
    cli::cli_abort(
      "{.arg generator} must be a function returning numeric data or a numeric vector."
    )
  }

  ex_check <- checkmate::check_list(expectations, min.len = 1L, names = "named")
  if (!isTRUE(ex_check)) {
    cli::cli_abort(c("{.arg expectations} is invalid.", "x" = "{ex_check}"))
  }

  ma_check <- checkmate::check_list(metric_args, names = "unique")
  if (!isTRUE(ma_check)) {
    cli::cli_abort(c("{.arg metric_args} is invalid.", "x" = "{ma_check}"))
  }

  spec    <- get_metric(metric)
  outputs <- spec[["outputs"]]

  if (!identical(sort(names(expectations)), sort(outputs))) {
    cli::cli_abort(
      "{.arg expectations} must be named exactly with the metric outputs: {.val {outputs}}."
    )
  }

  normalized_expectations <- lapply(expectations, .normalize_metric_validation_expectation)

  list(
    name         = name,
    metric       = metric,
    generator    = generator,
    metric_args  = metric_args,
    outputs      = outputs,
    expectations = normalized_expectations
  )
}

#' @noRd
.normalize_metric_validation_expectation <- function(x) {
  type <- x[["type"]] %||% NA_character_
  if (!identical(length(type), 1L) || is.na(type)) {
    cli::cli_abort("Validation expectation entries must include a scalar {.arg type}.")
  }

  switch(
    type,
    tolerance = .metric_validation_expect_tolerance(
      target    = x[["target"]],
      tolerance = x[["tolerance"]],
      allow_na  = x[["allow_na"]] %||% FALSE
    ),
    range = .metric_validation_expect_range(
      lower    = x[["lower"]] %||% -Inf,
      upper    = x[["upper"]] %||% Inf,
      allow_na = x[["allow_na"]] %||% FALSE
    ),
    predicate = .metric_validation_expect_predicate(
      fn       = x[["fn"]],
      label    = x[["label"]] %||% "custom predicate",
      allow_na = x[["allow_na"]] %||% FALSE
    ),
    cli::cli_abort(
      "Unknown validation expectation type {.val {type}}. Expected one of {.val {c('tolerance', 'range', 'predicate')}}."
    )
  )
}

#' @noRd
.materialize_metric_validation_series <- function(generator) {
  x <- if (is.function(generator)) generator() else generator

  if (!is.numeric(x)) {
    cli::cli_abort("Validation generator must produce a numeric vector.")
  }
  if (length(x) < 2L) {
    cli::cli_abort("Validation generator must produce at least 2 observations.")
  }
  as.numeric(x)
}

#' @noRd
.metric_validation_problems <- function(result, expectations) {
  issues <- character(0L)

  if (!is.numeric(result)) {
    return("Metric result must be a numeric vector.")
  }

  missing_names <- setdiff(names(expectations), names(result))
  if (length(missing_names) > 0L) {
    issues <- c(
      issues,
      sprintf("Missing result output(s): %s.", paste(missing_names, collapse = ", "))
    )
  }

  for (nm in names(expectations)) {
    if (!nm %in% names(result)) {
      next
    }

    value <- result[[nm]]
    if (!is.numeric(value) || length(value) != 1L) {
      issues <- c(
        issues,
        sprintf("Output `%s` must be a single numeric value.", nm)
      )
      next
    }

    rule <- expectations[[nm]]
    if (is.na(value)) {
      if (!isTRUE(rule[["allow_na"]])) {
        issues <- c(issues, sprintf("Output `%s` is NA but NA is not allowed.", nm))
      }
      next
    }

    if (rule[["type"]] == "tolerance") {
      diff <- abs(as.numeric(value) - rule[["target"]])
      if (diff > rule[["tolerance"]]) {
        issues <- c(
          issues,
          sprintf(
            "Output `%s` = %.6f is outside target %.6f +/- %.6f.",
            nm, as.numeric(value), rule[["target"]], rule[["tolerance"]]
          )
        )
      }
    } else if (rule[["type"]] == "range") {
      if (value < rule[["lower"]] || value > rule[["upper"]]) {
        issues <- c(
          issues,
          sprintf(
            "Output `%s` = %.6f is outside range [%.6f, %.6f].",
            nm, as.numeric(value), rule[["lower"]], rule[["upper"]]
          )
        )
      }
    } else if (rule[["type"]] == "predicate") {
      if (!isTRUE(rule[["fn"]](as.numeric(value)))) {
        issues <- c(
          issues,
          sprintf(
            "Output `%s` = %.6f failed predicate: %s.",
            nm, as.numeric(value), rule[["label"]]
          )
        )
      }
    }
  }

  issues
}

#' @noRd
.run_metric_validation_case <- function(case) {
  spec <- get_metric(case[["metric"]])
  x    <- .materialize_metric_validation_series(case[["generator"]])
  args <- utils::modifyList(spec[["defaults"]], case[["metric_args"]] %||% list())

  result <- spec[["fn"]](x, .args = args)
  problems <- .metric_validation_problems(result, case[["expectations"]])

  list(
    case     = case,
    input    = x,
    result   = result,
    problems = problems
  )
}
