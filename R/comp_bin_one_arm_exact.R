cli_rule(center = "One-armed binary design exact")
# Params ----
params <- lst()
params$list <- list(
  alpha = c(0.005, 0.025, 0.05, 0.1, 0.245),
  power = c(0.51, 0.8, 0.9, 0.99),
  pi_c = c(0.1, 0.3, 0.5, 0.8, 0.9),
  delta_pi = c(0.05, 0.15, 0.25, 0.49)
)

params$additional <- list(sided = 1)

params$table <-
  params$list |>
  expand.grid() |>
  as_tibble() |>
  filter(delta_pi + pi_c < 1)

# Design ----
design_bin_one_arm_exact <- ssc_design(
  endpoint = "binary",
  type = "fixed",
  params = params,
  computation = "exact"
)
cli_alert_success("Params & design")

## A'Hern ----
ahern_wrapper <- partial(wrapper$ahern, !!!params$additional)
ahern <-
  params$table |>
  mutate(n = pmap(params$table, ahern_wrapper, .progress = TRUE)) |>
  unnest(n) |> 
  mutate(relevancy = evaluate_relevancy_bin(alpha, power)) |>
  mutate(relevancy = fct_relevel(relevancy, c("high", "medium", "low"))) |>
  ssc_results(design = design_bin_one_arm_exact, method = "ahern")
cli_alert_success("A'Hern results")

## East ----
filelist_east <- list.files(
  path = "data-raw/East_bin_one_arm_exact",
  full.names = TRUE
)

east_raw <- read_csv(
  filelist_east,
  name_repair = "unique_quiet",
  show_col_types = FALSE
)

east <-
  east_raw |>
  select(
    alpha = "Specified α",
    power = "Power",
    pi_c = "π0",
    pi_e = "π1",
    n = "Sample Size"
  ) |>
  mutate(delta_pi = pi_e - pi_c) |>  
  select(-pi_e) |> 
  mutate(power = case_when(
      power < 0.75 ~ 0.51,
      power < 0.86 ~ 0.8,
      power < 0.99 ~ 0.9,
      .default = 0.99
    )
  ) |>
  distinct(alpha, power, pi_c, delta_pi, .keep_all = TRUE) |> 
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_bin_one_arm_exact, method = "east")
cli_alert_success("East results")

## Comparison
combined <-
  lst(ahern, east) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(\(x, y) full_join(x, y, by = join_by(alpha, power, pi_c, delta_pi))) |>
  mutate(relevancy = evaluate_relevancy_bin(alpha*2, power)) |> # Relevancy is computed for 2-sided alpha 
  mutate(relevancy = fct_relevel(relevancy, c("high", "medium", "low"))) |>
  ssc_results(design = design_bin_one_arm_exact, method = "combined")
cli_alert_success("Combined results")

# Tables & figures
cli_alert_success("Tables & figures")

ssc$bin$one_arm_exact$res <- lst(ahern, east)
ssc$bin$one_arm_exact$raw <- lst("east" = east_raw)
ssc$bin$one_arm_exact$params <- params
ssc$bin$one_arm_exact$combined <- combined