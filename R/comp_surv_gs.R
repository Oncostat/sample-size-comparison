cli_rule(center = "Survival group-sequential design")
# Params ----
params <- lst()
params$list = list(
  alpha = c(0.01, 0.05, 0.1),
  power = c(0.7, 0.8, 0.9),
  hr = c(0.3, 0.6, 0.9),
  surv_t = c(0.2, 0.5, 0.8) # Survival at 3-year
)
params$additional <- list(
  k = 4,
  equally_spaced = TRUE,
  event_time = 3,
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2,
  allocation_ratio = 1
)
# Efficacy: Lan-DeMets OF
# Futility: Lan-DeMets OF

# Get all combinaisons of params
params$table <- params$list |> expand.grid() |> as_tibble()

# Design ----
design_surv_gs <- ssc_design(
  endpoint = "survival",
  type = "gs",
  params = params
)
cli_alert_success("Params & design")

# Methods ----
## Rpact ----
#see [rpact doc](https://www.rpact.org/vignettes/planning/rpact_survival_examples/#sample-size-calculation-for-trials-with-interim-analyses)
rpact_wrapper <- partial(wrapper$rpact_surv_gs, !!!params$additional)
rpact <-
  params$table |>
  mutate(
    nested_res = pmap(params$table, rpact_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_gs, method = "rpact")
cli_alert_success("Rpact results")

## gsDesign2 ----
gsdesign2_wrapper <- partial(wrapper$gsdesign2_surv_gs, !!!params$additional)
gsdesign2 <-
  params$table |>
  mutate(
    nested_res = pmap(params$table, gsdesign2_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_gs, method = "gsdesign2")
cli_alert_success("Gsdesign2 results")

## East ----
filelist_east <- list.files(
  path = "data-raw/East_surv_gsdesign",
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
    hr = HazardRatio__AAAMA,
    surv_t = CumPercSurvCtrl__AAAMAA_AE, #In percentages
    e = TotSmplSiz__AB,
    n = A1x0__ABBE_BA
  ) |>
  mutate(surv_t = surv_t / 100) |>
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_surv_gs, method = "east")
cli_alert_success("East results")

# Comparision ----
combined <-
  lst(rpact, east, gsdesign2) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(
    \(x, y) {
      full_join(x, y, by = join_by(alpha, power, hr, surv_t))
    }
  ) |>
  mutate(relevancy = evaluate_relevancy_surv(alpha, power, hr)) |>
  mutate(relevancy = fct_relevel(relevancy, c("high", "medium", "low"))) |>
  ssc_results(design = design_surv_gs, method = "combined")

n_ratio <- 
  combined |>
  get_tbl() |> 
  get_n_ratio(ref = "east")

cli_alert_success("Combined results")

# Table & figures ----
title <- "N-Ratio 2-Arms Time-to-Event GS-design"
## Tables ----
table_n_ratio <- 
  n_ratio |> 
  gt_n_ratio(title = title, ref_name = "East")

tables <- lst(table_n_ratio)

## Figures ----
p_n_ratio <- 
  n_ratio |> 
  plot_n_ratio(title = title, ref_name = "East")

plots <- lst(p_n_ratio)
cli_alert_success("Tables & figures")

# Export results ----
ssc$surv$gs$res <- lst(rpact, east, gsdesign2)
ssc$surv$gs$raw <- lst("east" = east_raw)
ssc$surv$gs$params <- params
ssc$surv$gs$combined <- combined
ssc$bin$gs$tables <- tables
ssc$bin$gs$plots <- plots

comp_surv_gs <- lst(
  params,
  combined,
  n_ratio,
  tables,
  plots
)

# write_rds(comp_surv_gs, "outputs/comp_surv_gs.rds")

