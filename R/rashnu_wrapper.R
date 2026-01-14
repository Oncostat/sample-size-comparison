source("R/checks.R")
library(purrr)
library(rlang)
library(rashnu)
library(tibble)
#' Wrapper around rashnu's `lakatosSampleSize`.
#'
#' @param alpha Type I error, a numerical value in ]0, 1[.
#' @param power Power, a numerical value in ]0, 1[. Represent (1 - beta),
#' where beta is the Type II error.
#' @param hr Hazard ratio. a numerical value in ]0, 1[.
#' Represent the fraction of lambdas from two groups, saying if a group
#' has better or worse survival probability.
#' @param surv_t Survival probability of control group at event time t.
#' Default value for `event_time` is 3.
#' So without event time specification, correspond to 3-year survival.
#' @param event_time Time at which we evaluate survival probability. Default is 3.
#' @param accrual_time Time of accrual. Default is 3.
#' @param follow_up_time (Minimal) follow-up time. Default is 3.
#' Study duration correspond to `accrual_time` + `follow_up_time`.
#' @param sided One of c(1, 2). Is the test one-sided or two-sided. Default is 2
#' @param method One of c("logrank", "gehan", "tarone-ware").
#' @param allocation_ratio Allocation ratio of sample size between control and testing group (Test/control).
#' Default is 1 corresponding to same allocation in control and testing group.
#' @param error What should be returned for needed number of events and sample size
#' when an error occured. Default is `NA_real_`.
#'
#' @returns A tibble with e = number of needed event and n = required sample size.
#'
#' @examples
#' rashnu_wrapper(
#'   alpha = 0.05,
#'   power = 0.9,
#'   hr = 0.8,
#'   surv_t = 0.6,
#'   event_time = 3
#' )
rashnu_wrapper <- function(
  alpha,
  power,
  hr,
  surv_t,
  event_time = 3, 
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  allocation_ratio = 1,
  method = c("logrank", "gehan", "tarone-ware"),
  b = 24,
  error = NA_real_
){
  # Check that those parameters are between 0 and 1 (excluded).
  map(c(alpha, power, hr, surv_t), check_probability)

  method <- arg_match(method)

  tryCatch({
    sample_size_info <- lakatosSampleSize(
      alpha = alpha,
      power = power,
      syear = event_time,
      yrsurv1 = surv_t,
      yrsurv2 = surv_t**hr,
      side = ifelse(sided == 2, "two.sided", "one.sided"),
      accrualTime = accrual_time,
      followTime = follow_up_time,
      alloc = allocation_ratio,
      method = method,
      b = b

    )
    return(
      tibble(
        e = ceiling(sample_size_info$Total_expected_event_numbers),
        n = ceiling(sample_size_info$Total_sample_size)
      )
    )},

  error = function(er){
    return(tibble(e = error, n = error))
  })
}