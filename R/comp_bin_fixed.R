param_list_bin_fixed <- list(
  alpha = c(0.01, 0.05, 0.1, 0.20, 0.49),
  power = c(0.51, 0.8, 0.9, 0.99),
  pi_c = c(0.1, 0.3, 0.5, 0.8, 0.9),
  delta_pi = c(0.05, 0.15, 0.25, 0.49)
)

param_table_bin_fixed <-
  param_list_bin_fixed |> 
  expand.grid() |> 
  as_tibble() |> 
  filter(delta_pi + pi_c < 1)

## Rpact ----
rpact_res_bin_fixed <-
  param_table_bin_fixed |> 
  mutate(n_rpact = pmap(param_table_bin_fixed, rpact_bin_wrapper)) |> 
  unnest(n_rpact)

## East ----
 
east_res_bin_fixed_raw <- read_csv("data-raw/east_bin_fixed.csv")

east_res_bin_fixed <- 
  east_res_bin_fixed_raw |> 
  select(
    alpha = "Specified α",
    power = "Power",
    pi_c = "πc",
    delta_pi = "δ1",
    n_east = "Sample Size"
  ) |>  
  mutate(power = signif(power, 1), alpha = alpha*2)

## Comparison
combined_res_bin_fixed <- 
  reduce(
    list(
      rpact_res_bin_fixed,
      east_res_bin_fixed),
    \(x, y){inner_join(x, y, by = join_by(alpha, power, pi_c, delta_pi))}
  )
