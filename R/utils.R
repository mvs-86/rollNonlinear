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
