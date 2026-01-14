library(testthat)

source("R/checks.R")

test_that("check_probability works", {
  
  # With 0.
  expect_error(
    check_probability(0),
    "`0` is not between 0 and 1"
  )
  expect_invisible(
    check_probability(0, with_bounds = TRUE)
  )

  # With 1.
  expect_error(
    check_probability(1),
    "`1` is not between 0 and 1"
  )
  expect_invisible(
    check_probability(1, with_bounds = TRUE)
  )

  # With something in between.
  prob = runif(n = 1, min = 0, max = 1)
  expect_invisible(
    check_probability(prob)
  )

  # With something outside.
  per <- runif(n = 1, min = 1, max = 100)
  expect_error(
    check_probability(per),
    "is not between 0 and 1"
  )
})
