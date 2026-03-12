cli_rule(center = "Exact computations for Binary Two-Arm")
# Params ----
params <- lst()
params$list <- list(
  alpha = c(0.01, 0.05, 0.1),
  power = c(0.8, 0.9),
  pi_c = c(0.1, 0.3, 0.5, 0.8),
  delta_pi = c(0.15, 0.25)
)
params$additional <- list(sided = 2)
params$table <-
  params$list |>
  expand.grid() |>
  as_tibble() |>
  filter(delta_pi + pi_c < 1)
cli_alert_success("Params")

# Exact methods wrappers ----
fisher_wrapper <- partial(
  wrapper$bbssr_bin_fixed,
  !!!params$additional,
  test = "Fisher"
)
chisq_wrapper <- partial(
  wrapper$bbssr_bin_fixed,
  !!!params$additional,
  test = "Chisq"
)
fisher_midp_wrapper <- partial(
  wrapper$bbssr_bin_fixed,
  !!!params$additional,
  test = "Fisher-midP"
)
z_pool_wrapper <- partial(
  wrapper$bbssr_bin_fixed,
  !!!params$additional,
  test = "Z-pool"
)
boschloo_wrapper <- partial(
  wrapper$bbssr_bin_fixed,
  !!!params$additional,
  test = "Boschloo"
)

# Exact methods results ----
fisher <-
  params$table |>
  mutate(n = pmap_vec(params$table, fisher_wrapper, .progress = TRUE))

chisq <-
  params$table |>
  mutate(n = pmap_vec(params$table, chisq_wrapper, .progress = TRUE))

fisher_midp <-
  params$table |>
  mutate(n = pmap_vec(params$table, fisher_midp_wrapper, .progress = TRUE))

z_pool <-
  params$table |>
  mutate(n = pmap_vec(params$table, z_pool_wrapper, .progress = TRUE))

boschloo <-
  params$table |>
  mutate(n = pmap_vec(params$table, boschloo_wrapper, .progress = TRUE))

cli_alert_success("bbssr exact results")
# East ----
east <-
  list.files(
    path = "data-raw/East_bin_fixed_exact",
    full.names = TRUE
  ) |>
  read_csv(name_repair = "unique_quiet", show_col_types = FALSE) |>
  select(
    alpha = "Specified α",
    power = "Power",
    pi_c = "πc",
    delta_pi = "δ1",
    n = "Sample Size"
  ) |>
  mutate(alpha = 2 * alpha) |> #One-sided test
  arrange(alpha, pi_c, delta_pi, power) |>
  mutate(power = signif(power, 1))

cli_alert_success("East results")
# nQuery ----
nquery_raw <- read.csv2("data-raw/nquery_bin_fixed_exact.csv")
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
    alpha = signif(alpha, 1),
    power = signif(power, 1),
    pi_c = signif(pi_c, 1),
    delta_pi = signif(delta_pi, 2)
  )

cli_alert_success("nQuery results")
# Combined results ----
combined <-
  lst(
    fisher,
    chisq,
    fisher_midp,
    z_pool,
    boschloo,
    east,
    nquery
  ) |>
  add_name_as_suffix("n") |>
  reduce(\(x, y) inner_join(x, y, by = join_by(alpha, power, pi_c, delta_pi)))

n_ratio_east <-
  combined |>
  select(-n_nquery) |>
  mutate(
    across(
      (starts_with("n_") & !all_of("n_east")),
      ~ . / n_east,
      .names = "{sub('^n_', '', .col)}"
    )
  ) |>
  select(-(starts_with("n_") & !all_of("n_east"))) |>
  pivot_longer(
    cols = any_of(
      c("fisher", "chisq", "fisher_midp", "z_pool", "boschloo")
    ),
    names_to = "method",
    values_to = "n_ratio"
  )

n_ratio_nquery <-
  combined |>
  select(-n_east) |>
  mutate(
    across(
      (starts_with("n_") & !all_of("n_nquery")),
      ~ . / n_nquery,
      .names = "{sub('^n_', '', .col)}"
    )
  ) |>
  select(-(starts_with("n_") & !all_of("n_nquery"))) |>
  pivot_longer(
    cols = any_of(
      c("fisher", "chisq", "fisher_midp", "z_pool", "boschloo")
    ),
    names_to = "method",
    values_to = "n_ratio"
  )

cli_alert_success("Combined results")
# Tables & figures ----
## Tables ----
gt_n_ratio_east <-
  n_ratio_east |>
  group_by(method) |>
  summarise(
    Min = min(n_ratio, na.rm = TRUE),
    Q1 = quantile(n_ratio, probs = 0.25, na.rm = TRUE),
    Moyenne = mean(n_ratio, na.rm = TRUE),
    Médiane = median(n_ratio, na.rm = TRUE),
    Q3 = quantile(n_ratio, probs = 0.75, na.rm = TRUE),
    Max = max(n_ratio, na.rm = TRUE)
  ) |>
  arrange(abs(1 - Médiane), abs(1 - Moyenne)) |>
  gt() |>
  tab_header(
    title = "N-Ratio by Exact methods",
    subtitle = "according to East sample sizes"
  ) |>
  fmt_number(decimals = 2)

gt_n_ratio_nquery <-
  n_ratio_nquery |>
  group_by(method) |>
  summarise(
    Min = min(n_ratio, na.rm = TRUE),
    Q1 = quantile(n_ratio, probs = 0.25, na.rm = TRUE),
    Moyenne = mean(n_ratio, na.rm = TRUE),
    Médiane = median(n_ratio, na.rm = TRUE),
    Q3 = quantile(n_ratio, probs = 0.75, na.rm = TRUE),
    Max = max(n_ratio, na.rm = TRUE)
  ) |>
  arrange(abs(1 - Médiane), abs(1 - Moyenne)) |>
  gt() |>
  tab_header(
    title = "N-Ratio by Exact methods",
    subtitle = "according to nQuery sample sizes"
  ) |>
  fmt_number(decimals = 2)

tables <- lst(gt_n_ratio_east, gt_n_ratio_nquery)

## Figures ----
p_n_ratio_east <-
  ggplot(n_ratio_east) +
  aes(x = n_east, y = n_ratio, color = method) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1 - 0.1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1 + 0.1, linetype = "dashed") +
  labs(
    title = "N-Ratio by Exact methods",
    subtitle = "according to East sample sizes",
    x = "N East",
    y = "N-Ratio",
    color = "Methods :"
  ) +
  scale_color_ssc()

p_n_ratio_nquery <-
  ggplot(n_ratio_nquery) +
  aes(x = n_nquery, y = n_ratio, color = method) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1 - 0.1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1 + 0.1, linetype = "dashed") +
  labs(
    title = "N-Ratio by Exact methods",
    subtitle = "according to nQuery sample sizes",
    x = "N nQuery",
    y = "N-Ratio",
    color = "Methods :"
  ) +
  scale_color_ssc()

plots <- lst(p_n_ratio_east, p_n_ratio_nquery)
cli_alert_success("Tables & figures")

# Export Results ----
explore_bin2exact <- lst(
  params,
  combined,
  n_ratio_east,
  n_ratio_nquery,
  tables,
  plots
)

# write_rds(explore_bin2exact, "outputs/explore_bin2exact.rds")
