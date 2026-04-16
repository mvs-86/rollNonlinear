#' Null-coalescing operator
#'
#' Returns `lhs` if it is not `NULL`, otherwise `rhs`.
#'
#' @param lhs Left-hand side value.
#' @param rhs Right-hand side default value.
#' @return `lhs` if non-`NULL`, else `rhs`.
#' @noRd
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

#' Safely coerce a value to integer
#'
#' Coerces `x` to integer if it is a whole number, otherwise returns `x`
#' unchanged (validation happens in `.validate_roll_args`).
#'
#' @param x A scalar numeric or integer.
#' @return Integer scalar, or `x` unchanged if coercion is not safe.
#' @noRd
.as_int <- function(x) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x) &&
      x == floor(x)) {
    as.integer(x)
  } else {
    x
  }
}
