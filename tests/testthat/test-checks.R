library(testthat)

source("R/checks.R")

test_that("check_probability works", {
  prob_with_bounds = runif(n = 200, min = 0, max = 1) %>% c(0, 1)
  prob_without_bounds = runif(n = 200, min = 0, max = 1) %>% setdiff(c(0, 1))

  expect_error(
    check_probability(prob_with_bounds, with_bounds = FALSE),
    "is not between 0 and 1"
  )
  expect_invisible(
    check_probability(prob_without_bounds, with_bounds = TRUE)
  )
  expect_invisible(
    check_probability(prob_without_bounds, with_bounds = FALSE)
  )
  expect_invisible(
    check_probability(prob_with_bounds, with_bounds = TRUE)
  )
})
