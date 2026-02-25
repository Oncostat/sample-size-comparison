library(cli)
library(rlang)

#' Check if a value is a probability
#'
#' @param x The value to check.
#' @param with_bounds Logical. Should 0 and 1 be included.
#' Default is `FALSE`.
#' @param na.rm Logical. Should NA be removed. Default is TRUE
#'
#' @returns Logical. `TRUE` if x is a probability
#'
#' @examples
#' check_probability(0.1)
#' check_probability(0, with_bounds = TRUE)
check_probability <- function(
  x,
  with_bounds = FALSE,
  na.rm = TRUE
) {
  if (with_bounds) {
    cond <- (x <= 1 & x >= 0)
  } else {
    cond <- (x < 1 & x > 0)
  }

  if (any(!cond, na.rm = na.rm)) {
    cli_abort(
      "{.var {caller_arg(x)}} is not between 0 and 1",
      "i" = "with_bounds = {.var {with_bounds}}"
    )
  }
}
