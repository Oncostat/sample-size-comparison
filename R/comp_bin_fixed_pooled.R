cli_rule(center = "Binary fixed design")
# Params ----
params <- lst()
params$list <- list(
  alpha = c(0.01, 0.05, 0.1, 0.20, 0.49),
  power = c(0.51, 0.8, 0.9, 0.99),
  pi_c = c(0.1, 0.3, 0.5, 0.8, 0.9),
  delta_pi = c(0.05, 0.15, 0.25, 0.49)
)

params$additional <- list(sided = 2)

params$table <-
  params$list |>
  expand.grid() |>
  as_tibble() |>
  filter(delta_pi + pi_c < 1)

# Design ----
design_bin_fixed_pooled <- ssc_design(
  endpoint = "binary",
  type = "fixed",
  params = params,
  computation = "pooled"
)
cli_alert_success("Params & design")

## Rpact ----
rpact_wrapper <- partial(wrapper$rpact_bin_fixed, !!!params$additional)
rpact <-
  params$table |>
  mutate(n = pmap(params$table, rpact_wrapper, .progress = TRUE)) |>
  unnest(n) |>
  ssc_results(design = design_bin_fixed_pooled, method = "rpact")
cli_alert_success("Rpact results")

## East ----
filelist_east <- list.files(
  path = "data-raw/East_bin_fixed_pooled",
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
    pi_c = "πc",
    delta_pi = "δ1",
    n = "Sample Size"
  ) |>
  mutate(alpha = 2 * alpha) |> #One-sided test
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_bin_fixed_pooled, method = "east")
cli_alert_success("East results")

## nQuery ----
nquery_raw <- read.csv2("data-raw/nquery_bin_fixed_pooled.csv")

transposed <- data.frame(t(nquery_raw[-1]))
colnames(transposed) <- nquery_raw[, 1]

nquery <-
  transposed |>
  tibble() |>
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    pi_c = "Group 2 Proportion, ???", #Group2 is control groupe
    delta_pi = "Difference between Proportions, D = ?? - ???",
    n_1 = "Group 1 Sample Size, n??",
    n_2 = "Group 2 Sample Size, n??"
  ) |>
  mutate(
    across(c(alpha, power, pi_c, delta_pi), \(x) {
      parse_number(x, locale = locale(decimal_mark = ","))
    }),
    across(c(n_1, n_2), as.numeric),
    power = power / 100,
    n = n_1 + n_2
  ) |>
  select(-c(n_1, n_2)) |>
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_bin_fixed_pooled, method = "nquery")
cli_alert_success("nQuery results")

## Comparison
combined <-
  lst(rpact, east, nquery) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(\(x, y) full_join(x, y, by = join_by(alpha, power, pi_c, delta_pi))) |>
  ssc_results(design = design_bin_fixed_pooled, method = "combined")
cli_alert_success("Combined results")

# Tables & figures
cli_alert_success("Tables & figures")

ssc$bin$fixed$res <- lst(rpact, east, nquery)
ssc$bin$fixed$raw <- lst("east" = east_raw, "nquery" = nquery_raw)
ssc$bin$fixed$params <- params
ssc$bin$fixed$combined <- combined
