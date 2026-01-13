library(cli)
#' Check if a value is a probability
#'
#' @param x The value to check.
#' @param with_bounds Logical. Should 0 and 1 be included.
#' Default is `FALSE`.
#'
#' @returns Logical. `TRUE` if x is a probability
#' 
#' @examples
#' check_probability(0.1)
#' check_probability(0, with_bounds = TRUE)
check_probability <- function(x, with_bounds = FALSE){
  if (with_bounds){
    cond <- (x <= 1 & x >= 0)
  } else {
    cond <- (x < 1 & x > 0)
  }

  if (!cond){
    cli_abort(
      "{.var {x}} is not between 0 and 1",
      "i" = "with_bounds = {.var {with_bounds}}"
    )
  }
}