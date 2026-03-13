#' Evaluate the relevancy of parameters.
#'
#' Evaluate if the parameters are realistic in real-life clinical
#' trials or if it is just theorical values to inspect model limitations.
#'
#' @param alpha Type I error, a numerical value in ]0, 1[.
#' @param power Power, a numerical value in ]0, 1[. Represent (1 - beta),
#' where beta is the Type II error.
#' @param hr Hazard ratio. a numerical value in ]0, 1[.
#' Represent the fraction of lambdas from two groups, saying if a group
#' has better or worse survival probability
#'
#' @returns One of `c("low", "medium", "high")`.
#'
#' @examples
#' evaluate_relevancy(
#'   alpha = 0.05,
#'   power = 0.9,
#'   hr = 0.8
#' )
evaluate_relevancy_surv <- function(
  alpha,
  power,
  hr
) {
  # Check that all parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr))

  relevancy = case_when(
    alpha > 0.2 | power < 0.6 | power >= 0.99 | hr < 0.5 | hr > 0.9 ~ "low",
    alpha > 0.02 & alpha < 0.15 & power > 0.75 & hr >= 0.7 & hr < 0.99 ~ "high",
    .default = "medium"
  )

  relevancy
}

evaluate_relevancy_bin <- function(
  alpha,
  power
) {
  # Check that all parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power))

  relevancy = case_when(
    alpha > 0.2 | power < 0.6 | power >= 0.99 ~ "low",
    alpha > 0.02 & alpha < 0.15 & power > 0.75 ~ "high",
    .default = "medium"
  )

  relevancy
}

# See https://github.com/dasonk/docstring for package like documentation.
# https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html
