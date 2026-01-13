source("R/checks.R")
library(purrr)
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
#' evaluate_relevancy_one(
#'   alpha = 0.05,
#'   power = 0.9,
#'   hr = 0.8
#' )
evaluate_relevancy_one <- function(
  alpha,
  power,
  hr
){
  # Check that all parameters are between 0 and 1 (excluded).
  map(c(alpha, power, hr), check_probability)

  # Not really relevant parameters
  if (alpha >= 0.4 || power <= 0.51 || power >= 0.99 || hr >= 0.99){
    relevancy <- "low"
  # Quite extreme paramters but could occur in some specific trials
  } else if (alpha >= 0.20 || hr <= 0.1 || hr >= 0.9){
    relevancy <- "medium"
  # Common parameters used in clinical trials
  } else {
    relevancy <- "high"
  }

  relevancy
}

#' Vectorized version of evaluate_relevancy_one.
evaluate_relevancy <- Vectorize(evaluate_relevancy_one)

# See https://github.com/dasonk/docstring for package like documentation.
# https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html 