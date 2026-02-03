cli_rule(center = "Survival One-arm design")
# Params ----
params <- lst()
# List of parameters for grid search
params$list = list(
  alpha = c(0.01, 0.05, 0.1, 0.20, 0.49),
  power = c(0.51, 0.8, 0.9, 0.99),
  hr = c(0.1, 0.5, 0.7, 0.9, 0.99),
  surv_t = c(0.1, 0.3, 0.6, 0.9)
)
params$additional <- list(
  event_time = 3,
  accrual_time = 3,
  follow_up_time = 3,
  sided = 2
)
# Get all combinaisons of params
params$table <- params$list |> expand.grid() |> as_tibble()
# Design ----
design_surv_one_arm <- ssc_design(
  endpoint = "survival",
  type = "fixed",
  params = params,
  computation = "one-arm"
)
cli_alert_success("Params & design")

# Methods ----
## OneArm2stage ----
oa2s_wrapper <- partial(wrapper$oa2s, !!!params$additional)
oa2s <-
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, oa2s_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |> 
  ssc_results(design = design_surv_one_arm, method = "oa2s")
cli_alert_success("OneArm2stage results")

## bbssr ----
sssas_wrapper <- partial(wrapper$sssas, !!!params$additional)
sssas <-
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, sssas_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |> 
  ssc_results(design = design_surv_one_arm, method = "sssas")
cli_alert_success("SampleSizeSingleArmSurvival results")

## Rashnu ----
rashnu_wrapper <- partial(wrapper$rashnu_surv_one_arm, !!!params$additional)
rashnu <-
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, rashnu_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_one_arm, method = "rashnu")
cli_alert_success("Rashnu results")

## nQuery ----
nquery_raw <- read.csv2("data-raw/nquery_surv_one_arm.csv", dec = ",")

transposed <- data.frame(t(nquery_raw[-1]))
colnames(transposed) <- nquery_raw[, 1]

nquery <-
  transposed |>
  tibble() |>
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    hr = "Hazard Ratio, h=??/???",
    mc = "Historic Control Median Survival, m??", # Group 2 is the control arm
    e = "Events Required, e?",
    n = "New Group Sample Size, n?"
  ) |>
  mutate(
    across(c(alpha, power, hr, mc), \(x) {
      parse_number(x, locale = locale(decimal_mark = ","))
    }),
    mc = signif(mc, 1),
    surv_t = case_when(
      mc == 0.9 ~ 0.1,
      mc == 2 ~ 0.3,
      mc == 4 ~ 0.6,
      mc == 20 ~ 0.9,
      .default = NA_real_
    ),
    across(c(e, n), as.numeric)
  ) |> 
  arrange(alpha, hr, surv_t, power)  |> 
  mutate(power = rep(params$list$power, times = n()/length(params$list$power))) |> 
  select(-mc, -e) |>
  ssc_results(design = design_surv_one_arm, method = "nquery")
cli_alert_success("nQuery results")

# Combined results ----
combined <-
  lst(oa2s, sssas, nquery, rashnu) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(
    \(x, y) {
      full_join(x, y, by = join_by(alpha, power, hr, surv_t))
    }
  ) |> 
  select(where(~ !all(is.na(.)))) |> 
  ssc_results(design = design_surv_one_arm, method = "combined")
cli_alert_success("Combined results")

# Tables and figures ----
# n_ratio_by_method_surv_fixed <-
#   combined |>
#   mutate(
#     rpact = n_rpact / n_east,
#     nquery = n_nquery / n_east,
#     rashnu = n_rashnu / n_east
#   ) |>
#   select(c(
#     alpha,
#     power,
#     hr,
#     surv_t,
#     relevancy,
#     n_east,
#     rpact,
#     nquery,
#     rashnu
#   )) |>
#   pivot_longer(
#     c(rpact, nquery, rashnu),
#     names_to = "method",
#     values_to = "n_ratio"
#   )

# p_n_ratio_by_method_surv_fixed <-
#   ggplot(n_ratio_by_method_surv_fixed) +
#   aes(x = n_east, y = n_ratio, color = method) +
#   geom_point() +
#   geom_hline(yintercept = 1) +
#   facet_wrap(~relevancy, scales = "free") +
#   labs(
#     title = "N ratio according to method and relevancy",
#     subtitle = "base sample size from East"
#   )
cli_alert_success("Tables & figures")

# Put into the scc object
