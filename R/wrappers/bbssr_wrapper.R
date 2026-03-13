wrapper$bbssr_bin_fixed <- function(
  alpha,
  power,
  pi_c,
  delta_pi,
  sided = 2,
  allocation_ratio = 1,
  test = c('Fisher', 'Chisq', 'Fisher-midP', 'Z-pool', 'Boschloo'),
  error = NA_real_
) {
  test <- arg_match(test)
  tryCatch(
    {
      sample_size_info <- bbssr::BinarySampleSize(
        alpha = alpha / sided,
        tar.power = power,
        p1 = pi_c + delta_pi,
        p2 = pi_c,
        r = allocation_ratio,
        Test = test
      )
      return(ceiling(sample_size_info$N))
    },
    error = function(er) {
      return(error)
    }
  )
}
