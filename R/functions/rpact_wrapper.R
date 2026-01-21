#' Wrapper around rpact's `getSampleSizeSurvival`.
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
#' @param computation One of "c("Schoenfeld", "Freedman", "HsiehFreedman")".
#' @param allocation_ratio Allocation ratio of sample size between control and testing group (Test/control).
#' Default is 1 corresponding to same allocation in control and testing group.
#' @param dropout_rate_1 Rate of droupout in control group. Default is 0.
#' @param dropout_rate_2 Rate of droupout in testing group. Default is 0.
#' @param error What should be returned for needed number of events and sample size
#' when an error occured. Default is `NA_real_`.
#'
#' @returns A tibble with e = number of needed event and n = required sample size.
#'
#' @references
#' The formula of Kim & Tsiatis (Biometrics, 1990) is used to calculate the expected number of events
#' under the alternative (see also Lakatos & Lan, Statistics in Medicine, 1992).
#' @examples
#' rpact_wrapper(
#'   alpha = 0.05,
#'   power = 0.9,
#'   hr = 0.8,
#'   surv_t = 0.6,
#'   event_time = 3
#' )
rpact$surv$fixed$wrapper <- memoise(function(
  alpha,
  power,
  hr,
  surv_t,
  event_time = 3, 
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  computation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
  allocation_ratio = 1,
  dropout_rate_1 = 0,
  dropout_rate_2 = 0,
  error = NA_real_
  ){
   # Check that those parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr, surv_t))
  # Check that those parameters are between 0 and 1 (included).
  check_probability(c(dropout_rate_1, dropout_rate_2), with_bounds = TRUE)
  
  computation <- arg_match(computation)

  tryCatch({
  # Times are in year NOT months
  sample_size_info <- getSampleSizeSurvival(
    alpha = alpha,
    beta = 1 - power,
    hazardRatio = hr,
    pi2 = 1 - surv_t,
    eventTime = event_time,
    typeOfComputation = computation,
    sided = sided,
    followUpTime = follow_up_time,
    accrualTime = c(0, accrual_time),
    allocationRatioPlanned = allocation_ratio,
    dropoutRate1 = dropout_rate_1,
    dropoutRate2 = dropout_rate_2,
  )

  return(tibble(
    e = ceiling(sample_size_info$eventsFixed),
    n = ceiling(sample_size_info$nFixed)))
  },

  error = function(er){
    return(tibble(e = error, n = error))
  })
})


rpact$surv$gs$wrapper <- memoise(function(
  alpha,
  power,
  hr,
  surv_t,
  k = 4,
  alpha_spending = c("asOF", "asP", "noEarlyEfficacy"),
  beta_spending = c( "bsOF", "bsP", "none"),
  equally_spaced = TRUE,
  manual_information_rates = NA,
  binding_futility = FALSE,
  event_time = 3, 
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  computation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
  allocation_ratio = 1,
  dropout_rate_1 = 0,
  dropout_rate_2 = 0,
  futility_bounds_scale = c("zValue", "pValue", "reverseCondPower", "condPowerAtObserved",
    "predictivePower"),
  error = NA_real_
  ){
  # Check that those parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr, surv_t))
  # Check that those parameters are between 0 and 1 (included).
  check_probability(c(dropout_rate_1, dropout_rate_2), with_bounds = TRUE)
  
  computation <- arg_match(computation)
  tryCatch({
  # Times are in year NOT months
  sample_size_info <- getSampleSizeSurvival(
    design = getDesignGroupSequential(
      sided = sided,
      alpha = alpha,
      beta = 1-power,
      kMax = k, 
      informationRates = ifelse(equally_spaced, NA_real_, manual_information_rates), # also the values by default.
      typeOfDesign = alpha_spending,
      typeBetaSpending = beta_spending, 
      bindingFutility = binding_futility,
      futilityBoundsScale = futility_bounds_scale
    ),
    hazardRatio = hr,
    pi2 = 1 - surv_t,
    eventTime = event_time,
    typeOfComputation = computation,
    followUpTime = follow_up_time,
    accrualTime = c(0, accrual_time),
    allocationRatioPlanned = allocation_ratio,
    dropoutRate1 = dropout_rate_1,
    dropoutRate2 = dropout_rate_2
  )

  return(tibble(
    e = ceiling(sample_size_info$maxNumberOfEvents),
    n = ceiling(sample_size_info$maxNumberOfSubjects)))
  },

  error = function(er){
    return(tibble(e = error, n = error))
  })
})

rpact$bin$fixed$wrapper <- function(
  alpha,
  power,
  pi_c,
  delta_pi,
  sided = 2,
  error = NA_real_
){
  tryCatch({
  sample_size_info <- getSampleSizeRates(
    alpha = alpha,
    beta = 1 - power, 
    pi1 = pi_c, 
    pi2 = pi_c + delta_pi,
    sided = sided
  )
   return(ceiling(sample_size_info$nFixed))
  },
   error = function(er){
    return(error)
  })   
}
