library(testthat)

test_that("evaluate_relevancy_one works", {
  # parameters should be between 0 and 1
  expect_error(
    evaluate_relevancy_one(
      alpha = 0.05,
      power = 90,
      hr = 0.7
    ),
    regex = "`90` is not between 0 and 1"
  )

  expect_in(
    evaluate_relevancy_one(
      alpha = runif(n = 1, min = 0, max = 1),
      power = runif(n = 1, min = 0, max = 1),
      hr = runif(n = 1, min = 0, max = 1)
    ),
    c("low", "medium", "high")
  )
})


test_that("evaluate_relevancy works", {
  alpha = runif(n = 3, min = 0, max = 1)
  power = runif(n = 3, min = 0, max = 1)
  hr = runif(n = 3, min = 0, max = 1)
  expect_equal(
    evaluate_relevancy(alpha, power, hr),
    c(
      evaluate_relevancy_one(alpha[1], power[1], hr[1]),
      evaluate_relevancy_one(alpha[2], power[2], hr[2]),
      evaluate_relevancy_one(alpha[3], power[3], hr[3])
    )
  )
})
