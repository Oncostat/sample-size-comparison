cli_rule(center = "One-armed binary design")
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
design_bin_one_arm <- ssc_design(
  endpoint = "binary",
  type = "fixed",
  params = params,
  computation = "pooled"
)
cli_alert_success("Params & design")

## Rpact ----
rpact_wrapper <- partial(wrapper$rpact_bin_one_arm, !!!params$additional)
rpact <-
  params$table |>
  mutate(n = pmap(params$table, rpact_wrapper, .progress = TRUE)) |>
  unnest(n) |>
  ssc_results(design = design_bin_one_arm, method = "rpact")
cli_alert_success("Rpact results")

# TODO: hardcode cases with far from input power.
## East ----
filelist_east <- list.files(
  path = "data-raw/East_bin_one_arm_pooled",
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
      power < 0.80 ~ 0.51,
      power < 0.90 ~ 0.8,
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
  ssc_results(design = design_bin_one_arm, method = "east")
cli_alert_success("East results")

## nQuery ----
nquery_raw <- read.csv2("data-raw/nquery_bin_one_arm.csv")

transposed <- data.frame(t(nquery_raw[-1]))
colnames(transposed) <- nquery_raw[, 1]

nquery <-
  transposed |>
  tibble() |>
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    pi_c = "Null Hypothesis Proportion, ???",
    pi_e = "Alternative Proportion, ???",
    n = "Sample Size, n?"
  ) |>
  mutate(
    across(c(alpha, power, pi_c, pi_e), \(x) {
      parse_number(x, locale = locale(decimal_mark = ","))
    }),
    n = as.numeric(n),
    power = power / 100,
    delta_pi = pi_e - pi_c
  ) |>
  select(-pi_e) |>
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_bin_one_arm, method = "nquery")
cli_alert_success("nQuery results")

## Comparison
combined <-
  lst(rpact, east, nquery) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(\(x, y) full_join(x, y, by = join_by(alpha, power, pi_c, delta_pi))) |>
  ssc_results(design = design_bin_one_arm, method = "combined")
cli_alert_success("Combined results")

# Tables & figures
cli_alert_success("Tables & figures")

ssc$bin$one_arm$res <- lst(rpact, east, nquery)
ssc$bin$one_arm$raw <- lst("east" = east_raw, "nquery" = nquery_raw)
ssc$bin$one_arm$params <- params
ssc$bin$one_arm$combined <- combined