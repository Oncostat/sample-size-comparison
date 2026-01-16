source("R/checks.R")
library(dplyr)
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
evaluate_relevancy <- function(
  alpha,
  power,
  hr
){
  # Check that all parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr))

  relevancy = case_when(
  # Not really relevant parameter
  alpha >= 0.4 | power <= 0.51 | power >= 0.99 | hr >= 0.99 ~ "low",
  # Quite extreme paramters but could occur in some specific trials
  alpha >= 0.20 | hr <= 0.1 | hr >= 0.9 ~ "medium",
  # Common parameters used in clinical trials
  .default = "high"
  )

  relevancy
}

# See https://github.com/dasonk/docstring for package like documentation.
# https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html 