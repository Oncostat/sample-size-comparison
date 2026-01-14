library(rashnu)
library(testthat)
library(tibble)

source("R/rashnu_wrapper.R")

test_that("rashnu_wrapper works", {
  expect_error(
    # all parameters should be between 0 and 1
    rashnu_wrapper(
      alpha = 0.05,
      power = 90,
      hr = 0.8,
      surv_t = 0.6
    ),
    regexp = "`90` is not between 0 and 1"
  )

  # Realistic parameters 
  alpha =  runif(n = 1, min = 0, max = 0.1)
  power = runif(n = 1, min = 0.7, max = 0.9)
  hr = runif(n = 1, min = 0.2, max = 0.9)
  surv_t = runif(n = 1, min = 0.2, max = 0.9)

  # equal to getSampleSizeSurvival with accrual = 3 and minimal follow-up time = 3.
  sample_size_info <- 
    lakatosSampleSize(
      alpha = alpha,
      power = power,
      syear = 3,
      yrsurv1 = surv_t,
      yrsurv2 = surv_t**hr,
      alloc = 1, # No default
      accrualTime = 3,
      followTime = 3,
    )
  
  expect_equal(
    rashnu_wrapper(alpha, power, hr, surv_t),
    tibble(
    e = ceiling(sample_size_info$Total_expected_event_numbers),
    n = ceiling(sample_size_info$Total_sample_size))
  )

})

