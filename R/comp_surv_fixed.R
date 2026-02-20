cli_rule(center = "Survival fixed design")
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
  sided = 2,
  allocation_ratio = 1
)
# Get all combinaisons of params
params$table <- params$list |> expand.grid() |> as_tibble()

# Design ----
design_surv_fixed <- ssc_design(
  endpoint = "survival",
  type = "fixed",
  params = params
)
cli_alert_success("Params & design")

# Methods ----
## Rpact ----
rpact_wrapper <- partial(wrapper$rpact_surv_fixed, !!!params$additional)
rpact <-
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, rpact_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_fixed, method = "rpact")
cli_alert_success("Rpact results")

## Rashnu ----
rashnu_wrapper <- partial(wrapper$rashnu_surv_fixed, !!!params$additional)
rashnu <-
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, rashnu_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_fixed, method = "rashnu")
cli_alert_success("Rashnu results")

## GsDesign2
gsdesign2_wrapper <- partial(wrapper$gsdesign2_surv_fixed, !!!params$additional)
gsdesign2 <- 
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, gsdesign2_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_fixed, method = "gsdesign2")
cli_alert_success("GsDesign2 results")

## East ----
# As East has problems running the 400 inputs consecutively
# I ran 4*100 by spliting by power value
filelist_east <- list.files(
  path = "data-raw/East_surv_fixed",
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
    hr = "Hazard Ratio (Alt.)",
    e = "Maximum Events",
    n = "Sample Size"
  ) |>
  arrange(alpha, power, hr, n) |>
  mutate(surv_t = rep(c(0.1, 0.3, 0.6, 0.9), n() / 4)) |> # there is no surv_t column in raw data!
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_surv_fixed, method = "east")
cli_alert_success("East results")

## nQuery ----
nquery_raw <- read.csv2("data-raw/nquery_grid_fixed.csv", dec = ",")

transposed <- data.frame(t(nquery_raw[-1]))
colnames(transposed) <- nquery_raw[, 1]

nquery <-
  transposed |>
  tibble() |>
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    hr = "Hazard Ratio, h=??/???",
    lambda2 = "Group 2 Exponential Parameter, ???", # Group 2 is the control arm
    e = "Total Number of Events Required, E?",
    n = "Sample Size per Group, n?"
  ) |>
  mutate(
    across(c(alpha, power, hr, lambda2), \(x) {
      parse_number(x, locale = locale(decimal_mark = ","))
    }),
    lambda2 = signif(lambda2, 1),
    surv_t = case_when(
      lambda2 == 0.80 ~ 0.1,
      lambda2 == 0.40 ~ 0.3,
      lambda2 == 0.20 ~ 0.6,
      lambda2 == 0.04 ~ 0.9,
      .default = NA_real_
    ),
    across(c(e, n), as.numeric),
    power = power / 100,
    n = 2 * n # introduce an error cause all n will be even.
  ) |>
  select(-lambda2) |>
  mutate(
    across(names(params$list), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  ssc_results(design = design_surv_fixed, method = "nquery")
cli_alert_success("nQuery results")

# Combined results ----
combined <-
  lst(rpact, east, nquery, rashnu, gsdesign2) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  reduce(
    \(x, y) {
      full_join(x, y, by = join_by(alpha, power, hr, surv_t))
    }
  ) |>
  mutate(relevancy = evaluate_relevancy_surv(alpha, power, hr)) |>
  mutate(relevancy = fct_relevel(relevancy, c("high", "medium", "low"))) |>
  ssc_results(design = design_surv_fixed, method = "combined")
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
ssc$surv$fixed$res <- lst(rpact, rashnu, gsdesign2, east, nquery)
ssc$surv$fixed$raw <- lst("east" = east_raw, "nquery" = nquery_raw)
ssc$surv$fixed$params <- params
ssc$surv$fixed$combined <- combined

# write_rds(combined, "surv/s-fixed.rds")
