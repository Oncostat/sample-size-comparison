wrapper$ahern <- function(
  alpha,
  power,
  pi_c,
  delta_pi,
  sided = 1,
  n_min=1,
  n_max=400, 
  tolerance=0.00005,
  error = NA_real_
){
  stopifnot(delta_pi >= 0)
  rtn = expand_grid(n = n_min:n_max, r = 0:n_max) |>
    filter(r <= n) |>
    mutate(
      alpha_real = 1 - pbinom(r - 1, size = n, prob = pi_c),
      power_real = 1 - pbinom(r - 1, size = n, prob = pi_c + delta_pi),
    ) |>
    filter(
      alpha_real <= alpha/sided+tolerance,
      power_real >= power-tolerance
    ) |>
    slice_min(r, by=n) |>
    slice_min(n) |>
    as.data.frame()
  if(nrow(rtn)==0) {
    warning(
      "Increase `n_max` ",
      "pi_c=",
      pi_c,
      ", pi_e=",
      pi_c + delta_pi,
      ", alpha=",
      alpha,
      ", power=",
      power
    )
  }
  rtn$n
}
