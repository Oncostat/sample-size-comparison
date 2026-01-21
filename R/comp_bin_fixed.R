params$bin$fixed$list <- list(
  alpha = c(0.01, 0.05, 0.1, 0.20, 0.49),
  power = c(0.51, 0.8, 0.9, 0.99),
  pi_c = c(0.1, 0.3, 0.5, 0.8, 0.9),
  delta_pi = c(0.05, 0.15, 0.25, 0.49)
)

params$bin$fixed$table <-
  params$bin$fixed$list |> 
  expand.grid() |> 
  as_tibble() |> 
  filter(delta_pi + pi_c < 1)

## Rpact ----
rpact$bin$fixed$res <-
  params$bin$fixed$table |> 
  mutate(n_rpact = pmap(params$bin$fixed$table, rpact$bin$fixed$wrapper)) |> 
  unnest(n_rpact)

## East ----
 
east$bin$fixed$raw <- bind_rows(
  read_csv("data-raw/east_bin_fixed_pooled_dp_005.csv"),
  read_csv("data-raw/east_bin_fixed_pooled_dp_015.csv"),
  read_csv("data-raw/east_bin_fixed_pooled_dp_025.csv"),
  read_csv("data-raw/east_bin_fixed_pooled_dp_049.csv"),
)

east$bin$fixed$res <- 
  east$bin$fixed$raw |> 
  select(
    alpha = "Specified α",
    power = "Power",
    pi_c = "πc",
    delta_pi = "δ1",
    n_east = "Sample Size"
  ) |>  
  mutate(alpha = 2*alpha) |> #One-sided test
  mutate(
    across(names(params$bin$fixed$list),
    \(x){closest(x, params$bin$fixed$list[[cur_column()]])})
  )

## nQuery ----
nquery$bin$fixed$raw <- read.csv2("data-raw/nquery_bin_fixed_pooled.csv", dec = ",")

transposed <- data.frame(t(nquery$bin$fixed$raw[-1]))
colnames(transposed) <- nquery$bin$fixed$raw[,1] 

nquery$bin$fixed$res <- 
  transposed |> 
  tibble() |> 
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    pi_c = "Group 2 Proportion, ???", #Group2 is control groupe
    delta_pi = "Difference between Proportions, D = ?? - ???",
    n_nquery_1 = "Group 1 Sample Size, n??",
    n_nquery_2 = "Group 2 Sample Size, n??"
  ) |> 
  mutate(
    across(c(alpha, power, pi_c, delta_pi), \(x){parse_number(x, locale = locale(decimal_mark = ","))}),
    across(c(n_nquery_1, n_nquery_2), as.numeric),
    power = power/100,
    n_nquery = n_nquery_1 + n_nquery_2
  ) |> 
  select(-c(n_nquery_1, n_nquery_2)) |> 
  mutate(
    across(names(params$bin$fixed$list),
    \(x){closest(x, params$bin$fixed$list[[cur_column()]])})
  )

## Comparison
combined$bin$fixed$res <- 
  reduce(
    list(
      rpact$bin$fixed$res,
      nquery$bin$fixed$res,
      east$bin$fixed$res
    ),
    \(x, y){inner_join(x, y, by = join_by(alpha, power, pi_c, delta_pi))}
  )

