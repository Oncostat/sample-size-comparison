cli_rule(center = "Binary group-sequential design")
# Params ----
params <- lst()
params$list <- list(
  alpha = c(0.005, 0.025, 0.05, 0.1, 0.245), #one-sided
  power = c(0.51, 0.8, 0.9, 0.99),
  pi_c = c(0.1, 0.3, 0.5, 0.8, 0.9),
  delta_pi = c(0.05, 0.15, 0.25, 0.49)
)

params$additional <- list(sided = 1, k = 4, equally_spaced = TRUE)

params$table <-
  params$list |>
  expand.grid() |>
  as_tibble() |>
  filter(delta_pi + pi_c < 1)

# Design ----
design_bin_gs_pooled <- ssc_design(
  endpoint = "binary",
  type = "gs",
  params = params,
  computation = "pooled"
)
cli_alert_success("Params & design")

## Rpact ----
rpact_wrapper <- partial(wrapper$rpact_bin_gs, !!!params$additional)
rpact <-
  params$table |>
  mutate(n = pmap_vec(params$table, rpact_wrapper, .progress = TRUE)) |>
  ssc_results(design = design_bin_gs_pooled, method = "rpact")
# cli_alert_success("Rpact results")

## East ----
filelist_east <- list.files(
  path = "data-raw/East_bin_gsdesign_pooled",
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
    alpha = Alpha__AAA,
    power = Power__AAA,
    pi_c = CtrlProp__AAAD,
    delta_pi = EffParam__AB,
    n = RoundSmplSiz__AB
  ) |>
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_bin_gs_pooled, method = "east")
cli_alert_success("East results")

## Comparison
combined <-
  lst(rpact, east) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(\(x, y) full_join(x, y, by = join_by(alpha, power, pi_c, delta_pi))) |>
  ssc_results(design = design_bin_gs_pooled, method = "combined")
cli_alert_success("Combined results")

# Tables & figures
cli_alert_success("Tables & figures")

ssc$bin$gs$res <- lst(rpact, east)
ssc$bin$gs$raw <- lst("east" = east_raw)
ssc$bin$gs$params <- params
ssc$bin$gs$combined <- combined
