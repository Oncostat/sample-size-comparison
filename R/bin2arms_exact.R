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

fisher_ <- partial(wrapper$bbssr_bin_fixed, !!!params$additional, test = "Fisher")
chisq_ <- partial(wrapper$bbssr_bin_fixed, !!!params$additional, test = "Chisq")
fisher_midp_ <- partial(wrapper$bbssr_bin_fixed, !!!params$additional, test = "Fisher-midP")
z_pool_ <- partial(wrapper$bbssr_bin_fixed, !!!params$additional, test = "Z-pool")
boschloo_ <- partial(wrapper$bbssr_bin_fixed, !!!params$additional, test = "Boschloo")

fisher <-
  params$table |>
  mutate(n = pmap_vec(params$table, fisher_, .progress = TRUE)) 

chisq <-
  params$table |>
  mutate(n = pmap_vec(params$table, chisq_, .progress = TRUE)) 

fisher_midp <-
  params$table |>
  mutate(n = pmap_vec(params$table, fisher_midp_, .progress = TRUE)) 

z_pool <-
  params$table |>
  mutate(n = pmap_vec(params$table, z_pool_, .progress = TRUE)) 

boschloo <-
  params$table |>
  mutate(n = pmap_vec(params$table, boschloo_, .progress = TRUE)) 

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

compared <- 
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
  
nr <- 
  compared |> 
  mutate(
    across(
      (starts_with("n_") & !all_of("n_east")),
      ~ ./n_east,
      .names = "{sub('^n_', '', .col)}"
    )
  ) |> 
  select(-(starts_with("n_") & !all_of("n_east"))) |> 
  pivot_longer(
    cols = any_of(
      c("fisher", "chisq", "fisher_midp", "z_pool", "boschloo", "nquery"
      )),
    names_to = "method",
    values_to = "n_ratio"
  )

p_nr <- 
  ggplot(nr) +
  aes(x = n_east, y = n_ratio, color = method) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1 - 0.1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1 + 0.1,  linetype = "dashed") +
  labs(
    title = "Exploration of unconditional exact tests for two independent binomials",
    subtitle = "N-Ratio according East",
    x = "N East",
    y = "N-Ratio",
    color = "Methods :"
  )

p_compared <- 
  ggplot(nr) +
  aes(x = paste0("α:", alpha, " Pwr: ", power), y = n_ratio, color = method)+
  geom_point() +
  geom_hline(yintercept = 1 - 0.1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1 + 0.1,  linetype = "dashed") +
  facet_grid(delta_pi ~ pi_c) +
  theme_linedraw(base_size = 15) +
  labs(
    title = "Exploration of unconditional exact tests for two independent binomials",
    subtitle = "N-Ratio according East",
    x = NULL,
    y = "N-Ratio",
    color = "Methods :"
  )
