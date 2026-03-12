cli_rule(center = "Survival fixed Lakatos & Lan (1992)")
# Lakatos and Lan table ----
design_table = tribble(
  ~surv_t , ~hr   , ~accrual_time , ~n_L , ~n_F , ~n_RGS , ~power_L , ~power_F , ~power_RGS ,
  0.8     , 0.667 ,             1 , 1617 , 1628 ,   1640 , 90.2     , 90.4     , 90.6       ,
  0.8     , 0.667 ,             5 , 2017 , 2024 ,   2046 , 90.6     , 90.7     , 91.0       ,
  0.8     , 0.667 ,             9 , 2724 , 2709 ,   2764 , 90.3     , 90.1     , 90.7       ,

  0.8     , 0.50  ,             1 ,  638 ,  649 ,    664 , 90.1     , 90.6     , 91.2       ,
  0.8     , 0.50  ,             5 ,  798 ,  807 ,    831 , 90.7     , 91.0     , 91.8       ,
  0.8     , 0.50  ,             9 , 1079 , 1081 ,   1124 , 90.1     , 90.2     , 91.2       ,

  0.8     , 0.25  ,             1 ,  230 ,  241 ,    269 , 91.9     , 93.0     , 95.3       ,
  0.8     , 0.25  ,             5 ,  289 ,  299 ,    338 , 92.2     , 93.0     , 95.5       ,
  0.8     , 0.25  ,             9 ,  392 ,  401 ,    459 , 91.6     , 92.2     , 95.1       ,

  0.2     , 0.667 ,             1 ,  360 ,  370 ,    363 , 89.6     , 90.4     , 89.8       ,
  0.2     , 0.667 ,             5 ,  414 ,  419 ,    418 , 90.5     , 90.8     , 90.8       ,
  0.2     , 0.667 ,             9 ,  528 ,  509 ,    534 , 89.8     , 88.7     , 90.1       ,

  0.2     , 0.50  ,             1 ,  134 ,  144 ,    138 , 89.7     , 91.7     , 90.5       ,
  0.2     , 0.50  ,             5 ,  156 ,  164 ,    161 , 89.9     , 91.3     , 90.8       ,
  0.2     , 0.50  ,             9 ,  200 ,  200 ,    207 , 89.7     , 89.7     , 90.7       ,

  0.2     , 0.25  ,             1 ,   43 ,   53 ,     48 , 90.2     , 95.1     , 93.0       ,
  0.2     , 0.25  ,             5 ,   51 ,   61 ,     58 , 90.6     , 94.8     , 93.8       ,
  0.2     , 0.25  ,             9 ,   66 ,   74 ,     76 , 90.2     , 93.1     , 93.7
)

# Params ----
params <- lst()
params$list = list(
  hr = c(0.667, 0.5, 0.25),
  surv_t = c(0.8, 0.2),
  accrual_time = c(1, 5, 9),
  follow_up_time = c(9, 5, 1)
)
params$additional <- list(
  event_time = 10,
  alpha = 0.05,
  power = 0.9
)
# Get all combinaisons of params with 10 years study duration
params$table <-
  params$list |>
  expand.grid() |>
  as_tibble() |>
  filter(accrual_time + follow_up_time == 10)

# Design ----
design_surv_fixed_lnl <- ssc_design(
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
    nested_res = pmap(params$table, rpact_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_fixed_lnl, method = "rpact")
cli_alert_success("Rpact results")

## Rashnu ----
rashnu_wrapper <- partial(wrapper$rashnu_surv_fixed, !!!params$additional)
rashnu <-
  params$table |>
  mutate(
    nested_res = pmap(params$table, rashnu_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_fixed_lnl, method = "rashnu")
cli_alert_success("Rashnu results")

## GsDesign2
gsdesign2_wrapper <- partial(wrapper$gsdesign2_surv_fixed, !!!params$additional)
gsdesign2 <- 
  params$table |>
  mutate(
    nested_res = pmap_vec(params$table, gsdesign2_wrapper, .progress = TRUE)
  ) |>
  unnest(nested_res) |>
  ssc_results(design = design_surv_fixed_lnl, method = "gsdesign2")
cli_alert_success("GsDesign2 results")

## East ----
east_raw <- read_csv(
  "data-raw/east_lakatos_n_lan.csv",
  name_repair = "unique_quiet",
  show_col_types = FALSE
)
east <-
  east_raw |>
  select(
    alpha = "Specified α",
    power = "Power",
    hr = "Hazard Ratio (Alt.)",
    accrual_time = "Comm. Accr. (Dur.)",
    e = "Maximum Events",
    n = "Sample Size"
  ) |>
  arrange(hr, accrual_time, n) |>
  mutate(surv_t = rep(c(0.2, 0.8), n() / 2)) |>
  mutate(
    across(c(alpha, power, hr, accrual_time), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  mutate(follow_up_time = 10 - accrual_time) |> 
  ssc_results(design = design_surv_fixed_lnl, method = "east")
cli_alert_success("East results")

## nQuery ----
nquery_raw <- read.csv2(
  "data-raw/nquery_lakatos_n_lan.csv",
  dec = ","
)

transposed <- data.frame(t(nquery_raw[-1]))
colnames(transposed) <- nquery_raw[, 1]

nquery <-
  transposed |>
  tibble() |>
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    hr = "Hazard Ratio, h=??/???",
    lambda2 = "Group 2 Exponential Parameter, ???",
    accrual_time = "Length of Accrual Period, a?",
    e = "Total Number of Events Required, E?",
    n = "Sample Size per Group, n?"
  ) |>
  mutate(
    across(
      c(alpha, power, hr, lambda2),
      \(x) {
        parse_number(x, locale = locale(decimal_mark = ","))
      }
    ),
    lambda2 = signif(lambda2, 1),
    hr = signif(hr, 3),
    accrual_time = as.numeric(accrual_time),
    surv_t = case_when(
      lambda2 == 0.2 ~ 0.2,
      lambda2 == 0.02 ~ 0.8,
      .default = NA_real_
    ),
    across(c(e, n), as.numeric),
    power = power / 100,
    n = 2 * n # introduce an error cause all n will be even.
  ) |>
  select(-lambda2) |>
  mutate(
    across(c(alpha, power, hr, accrual_time), \(x) {
      closest(x, params$list[[cur_column()]])
    })
  ) |>
  mutate(follow_up_time = 10 - accrual_time) |> 
  ssc_results(design = design_surv_fixed_lnl, method = "nquery")
cli_alert_success("nQuery results")

# Comparison ----
representation_ll <- function(data_ll) {
  data_ll |>
    select(surv_t, hr, accrual_time, follow_up_time, starts_with("n_")) |>
    arrange(desc(surv_t), desc(hr))
}

combined <-
  lst(rpact, rashnu, gsdesign2, east, nquery) |>
  map(get_tbl) |>
  add_name_as_suffix(c("e", "n")) |>
  map(representation_ll) |>
  reduce(\(x, y) {
    full_join(x, y, by = join_by(hr, surv_t, accrual_time, follow_up_time))
  }) |>
  full_join(design_table, by = join_by(hr, surv_t, accrual_time)) |>
  ssc_results(design = design_surv_fixed_lnl, method = "combined")
cli_alert_success("Combined results")

# Tables & figures
low_cols  <- c("n_rpact","n_rashnu","n_gsdesign2","n_east","n_nquery")
high_cols <- c("n_L","n_F","n_RGS")

gt_lakatos_n_lan <- 
  combined |> 
  get_tbl() |> 
  select(-starts_with("power_"), -follow_up_time) |> 
  rename(Survival = surv_t, HR = hr, Accrual = accrual_time) |> 
  gt() |>
  tab_header(
      title = "Lakatos & Lan (1992) comparison with East, nQuery and R packages",
      subtitle = "Survival at 10 years; Study duration = 10 years; α = 0.05;  β = 0.1"
  ) |> 
  tab_style(
    style = cell_fill(color = "#75cec6ff"),
    location = cells_body(columns = all_of(low_cols))
  ) |>
  tab_style(
    style = cell_fill(color = "#ce9d75ff"),
    location = cells_body(columns = all_of(high_cols))
  ) |> 
  tab_footnote(md("<span style='color:#75cec6ff'>**Blue**</span>: Computed sample sizes")) |> 
  tab_footnote(md("<span style='color:#ce9d75ff'>**Brown**</span>: Sample sizes from the 1992 article"))

plot_lakatos_n_lan <-
  combined  |> 
  get_tbl()  |> 
  rename(n_Freedman = n_F, n_Lakatos = n_L) |> 
  mutate(across(starts_with("n_"), ~ ./n_east)) |> 
  select(-n_east) |> 
  pivot_longer(cols = starts_with("n_"), names_to ="method", values_to = "N")  |> 
  mutate(method = str_remove(method, "n_")) |>
  ggplot() +
  aes(x = paste0("HR: ",hr,", Accrual", accrual_time), y = N, color = method) +
  geom_jitter(height = 0, width = 0.1) +
  geom_hline(yintercept = 1 + c(-1, 1) * er_rate, linetype = "dashed") +
  geom_hline(yintercept = 1) + 
  facet_wrap(surv_t ~ ., scales = "free") +
  scale_color_ssc() +
  labs(
    title ="N-Ratio by method",
    subtitle ="For both 0.2 and 0.8 survival",
    x = NULL,
    y = "N / N-East", 
  )

# Put into the scc object
ssc$surv$fixed$res_lnl <- lst(rpact, rashnu, gsdesign2, east, nquery)
ssc$surv$fixed$raw_lnl <- lst("east" = east_raw, "nquery" = nquery_raw)
ssc$surv$fixed$params_lnl <- params
ssc$surv$fixed$combined_lnl <- combined

lakatos_n_lan <- lst(get_tbl(combined), gt_lakatos_n_lan, plot_lakatos_n_lan)

# write_rds(lakatos_n_lan, "outputs/lakatos_n_lan.rds")
