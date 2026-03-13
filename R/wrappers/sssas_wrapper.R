wrapper$sssas <- function(
  alpha,
  power,
  surv_t,
  hr,
  event_time = 3,
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  error = NA_real_
) {
  # Check that those parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr, surv_t))

  tryCatch(
    {
      # Times are in year NOT months
      sample_size_info <- SampleSizeSingleArmSurvival::calcSampleSizeArcsine(
        S0 = surv_t,
        S1 = surv_t**hr,
        alpha = alpha / sided, # always one-sided
        power = power,
        accrual = accrual_time,
        followup = follow_up_time,
        timePoint = event_time
      )
      return(tibble(
        n = ceiling(sample_size_info)
      ))
    },

    error = function(er) {
      return(tibble(e = error, n = error))
    }
  )
}
