mv_case <- function(...) {
  rollNonlinear:::.metric_validation_case(...)
}

mv_expect_tolerance <- function(...) {
  rollNonlinear:::.metric_validation_expect_tolerance(...)
}

mv_expect_range <- function(...) {
  rollNonlinear:::.metric_validation_expect_range(...)
}

mv_expect_predicate <- function(...) {
  rollNonlinear:::.metric_validation_expect_predicate(...)
}

expect_metric_validation <- function(case) {
  run <- rollNonlinear:::.run_metric_validation_case(case)

  if (length(run[["problems"]]) > 0L) {
    testthat::fail(paste(
      c(
        sprintf(
          "Metric validation failed for %s (%s).",
          run[["case"]][["name"]],
          run[["case"]][["metric"]]
        ),
        paste0("- ", run[["problems"]])
      ),
      collapse = "\n"
    ))
  } else {
    testthat::succeed()
  }

  invisible(run)
}
