wrapper$gsdesign2_surv_fixed <- function(
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
) {
  # Check that those parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr, surv_t))
  # Check that those parameters are between 0 and 1 (included).
  check_probability(dropout_rate, with_bounds = TRUE)

  tryCatch(
    {
      enroll_rate <- define_enroll_rate(duration = accrual_time, rate = 1)
      fail_rate <- define_fail_rate(
        duration = event_time,
        fail_rate = 1 - surv_t,
        hr = hr,
        dropout_rate = dropout_rate
      )
      fd <- fixed_design_ahr(
        enroll_rate = enroll_rate,
        fail_rate = fail_rate,
        alpha = alpha / sided, # always 1-sided
        power = power,
        study_duration = event_time + follow_up_time,
        ratio = allocation_ratio
      )

      return(tibble(
        e = ceiling(fd$analysis$event),
        n = ceiling(fd$analysis$n)
      ))
    },

    error = function(er) {
      return(tibble(e = error, n = error))
    }
  )
}

wrapper$gsdesign2_surv_gs <- function(
  alpha,
  power,
  hr,
  surv_t,
  k = 4,
  equally_spaced = TRUE,
  manual_information_rates = NA,
  binding_futility = FALSE,
  event_time = 3,
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  allocation_ratio = 1,
  dropout_rate = 0,
  error = NA_real_
) {
  # Check that those parameters are between 0 and 1 (excluded).
  check_probability(c(alpha, power, hr, surv_t))
  # Check that those parameters are between 0 and 1 (included).
  check_probability(dropout_rate, with_bounds = TRUE)

  tryCatch(
    {
      enroll_rate <- define_enroll_rate(duration = accrual_time, rate = 1)
      fail_rate <- define_fail_rate(
        duration = event_time,
        fail_rate = 1 - surv_t,
        hr = hr,
        dropout_rate = dropout_rate
      )
      upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha / 2, param = NULL)
      lpar <- list(sf = gsDesign::sfLDOF, total_spend = alpha / 2, param = NULL)
      trial_duration <- accrual_time + follow_up_time # Planned trial duration
      # Information fraction at analyses
      info_frac <- if (equally_spaced) {
        seq(0, 1, length.out = k + 1)[-1]
      } else {
        manual_information_rates
      }
      gsd <- gs_design_ahr(
        enroll_rate = enroll_rate,
        fail_rate = fail_rate,
        ratio = allocation_ratio,
        beta = 1 - power,
        # Information fraction at analyses and trial duration
        info_frac = info_frac,
        analysis_time = trial_duration,
        # Use NULL information for Type I error, H1 information for power
        info_scale = "h0_h1_info", # Default
        # Function and parameter(s) for upper spending bound
        upper = gs_spending_bound,
        upar = upar,
        lower = gs_spending_bound,
        lpar = lpar,
        # Symmetric designs use binding bounds
        binding = binding_futility,
        h1_spending = FALSE # Use null hypothesis spending for lower bound
      )

      return(tibble(
        e = ceiling(max(gsd$analysis$event)),
        n = ceiling(max(gsd$analysis$n))
      ))
    },

    error = function(er) {
      return(tibble(e = error, n = error))
    }
  )
}
