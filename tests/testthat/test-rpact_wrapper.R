library(rpact)
library(testthat)
library(tibble)

test_that("rpact_wrapper works", {
  expect_error(
    # all parameters should be between 0 and 1
    rpact_wrapper(
      alpha = 0.05,
      power = 90,
      hr = 0.8,
      surv_t = 0.6
    ),
    regexp = "`90` is not between 0 and 1"
  )

  # Handle extreme parameters that lead to less than a patient.
  expect_equal(
    rpact_wrapper(
        alpha = 0.49,
        power = 0.51,
        hr = 0.01,
        surv_t = 0.6,
        error = NA_real_
    ),
    tibble(e = NA_real_, n = NA_real_)
  )

  # Realistic parameters 
  alpha =  runif(n = 1, min = 0, max = 0.1)
  power = runif(n = 1, min = 0.7, max = 0.9)
  hr = runif(n = 1, min = 0.2, max = 0.9)
  surv_t = runif(n = 1, min = 0.2, max = 0.9)

  # equal to getSampleSizeSurvival with accrual = 3 and minimal follow-up time = 3.
  sample_size_info <- 
    getSampleSizeSurvival(
      typeOfComputation = "Schoenfeld",
      alpha = alpha,
      beta = 1 - power,
      hazardRatio = hr,
      pi2 = 1 - surv_t,
      eventTime = 3,
      accrualTime = c(0, 3),
      followUpTime = 3,
      sided = 2 # equal 1 by default
    )
  
  expect_equal(
    rpact_wrapper(alpha, power, hr, surv_t),
    tibble(
    e = ceiling(sample_size_info$eventsFixed),
    n = ceiling(sample_size_info$nFixed))
  )

})
