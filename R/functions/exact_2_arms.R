#' Compute sample size for a Two-Sample Exact Test
#'
#' @description
#' Wrapper around [Exact::power.exact.test] to compute total 
#' sample size (for a 1:1 randomization)
#'
#' @author Dan Chaltiel
sample_size_extact_test <- function(
  p1,
  p2,
  alpha,
  power,
  interval = c(2, 500),
  alternative = c("two.sided", "less", "greater"),
  ...){
  f <- function(n){
    n_split <- round(n/2)
    p <- Exact::power.exact.test(
      p1,
      p2,
      n_split,
      n_split,
      alpha = alpha, 
      alternative = alternative, 
      ...)$power
    
    if(p < power) return(.Machine$double.xmax)
    abs(p - power)
  }

  n_optimal <- 
    f |>
    optimize(interval) |> 
    purrr::pluck("minimum")|> 
    ceiling()

  power_computed <- 
    Exact::power.exact.test(
      p1,
      p2,
      n_optimal/2,
      n_optimal/2,
      alpha = alpha, 
      alternative = alternative,
      ...
    )$power
  
  if(power_computed < power){
    m <-
      c(
        "Optimization failed for {.val p1={p1}}, {.val p2={p2}}, {.val alpha={alpha}}, {.val power={power}}.",
        i = "Increase the interval upper limit (current={interval[2]})."
      )
    cli::cli_warn(m)
  }
  res <-
    tibble::tibble(
      power_computed = power_computed,
      n = n_optimal
    )

  res
}

sset_wrapper <- function(
  pi_c,
  delta_pi,
  alpha,
  power,
  interval = c(2, 500),
  alternative = c("two.sided", "less", "greater"),
  error = NA,
  ...){
tryCatch(
    {
      return(
        sample_size_extact_test(
          p1 = pi_c,
          p2 = pi_c + delta_pi,
          alpha = alpha,
          power = power,
          interval = interval,
          alternative = alternative
        )$n
      )
    },
    error = function(er) {
      return(error)
    }
  )
}
