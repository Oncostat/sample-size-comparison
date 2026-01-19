gsdesign2_wrapper <- function(
  alpha,
  power,
  hr,
  surv_t,
  event_time = 3, 
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  allocation_ratio = 1,
  dropout_rate = 0,
  error = NA_real_
  ){
  # Check that those parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr, surv_t))
  # Check that those parameters are between 0 and 1 (included).
  check_probability(dropout_rate, with_bounds = TRUE)

  tryCatch({
  enroll_rate <- define_enroll_rate(duration = accrual_time, rate = 1)
  fail_rate <- define_fail_rate(
    duration = event_time,
    fail_rate = 1-surv_t,
    hr = hr,
    dropout_rate = dropout_rate
  )
  fd <- fixed_design_ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    alpha = alpha/sided, # always 1-sided
    power = power,
    study_duration = event_time + follow_up_time,
    ratio = allocation_ratio
  )

  return(tibble(
    e = ceiling(fd$analysis$event),
    n = ceiling(fd$analysis$n)))
  },

  error = function(er){
    return(tibble(e = error, n = error))
  })
}