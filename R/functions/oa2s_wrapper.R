wrapper$oa2s <- function(
  alpha, 
  power,
  surv_t,
  hr,
  event_time = 3,
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  error = NA_real_
){
   tryCatch(
    {
      # Times are in year NOT months
      sample_size_info <- OneArm2stage::phase2.TTE(
        shape = 1, # Exponential survival assumption
        S0 = surv_t,
        x0 = event_time,
        hr = hr,
        tf = follow_up_time,
        ta = accrual_time,
        alpha = alpha/2 * sided, #always two-sided
        beta = 1 - power
      )
      return(tibble(e = NA_real_, n = sample_size_info$Single_stage$nsingle))
    },

    error = function(er) {
      return(tibble(e = error, n = error))
    }
  )
}
