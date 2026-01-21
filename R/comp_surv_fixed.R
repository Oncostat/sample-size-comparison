# Params ----
# List of parameters for grid search
params$surv$fixed$list = list(
  alpha = c(0.01, 0.05, 0.1, 0.20, 0.49),
  power = c(0.51, 0.8, 0.9, 0.99),
  hr = c(0.1, 0.5, 0.7, 0.9, 0.99),
  surv_t = c(0.1, 0.3, 0.6, 0.9)
)

# Get all combinaisons of params
params$surv$fixed$table <- params$surv$fixed$list |> expand.grid() |> as_tibble()

# Methods ----
## Rpact ----
rpact$surv$fixed$res <- 
  params$surv$fixed$table |> 
  mutate(
    nested_res = pmap(params$surv$fixed$table, rpact$surv$fixed$wrapper)
  ) |> 
  unnest(nested_res) |> 
  rename(e_rpact = e, n_rpact = n)

## East ---- 
# As East has problems running the 400 inputs consecutively
# I ran 4*100 by spliting by power value 
east$surv$fixed$raw <- bind_rows(
  read_csv("data-raw/east_pwr051.csv"),
  read_csv("data-raw/east_pwr08.csv"),
  read_csv("data-raw/east_pwr09.csv"),
  read_csv("data-raw/east_pwr099.csv")
)

east$surv$fixed$res <- 
  east$surv$fixed$raw |> 
  select(
    alpha = "Specified α",
    power = "Power",
    hr = "Hazard Ratio (Alt.)",
    e_east = "Maximum Events",
    n_east = "Sample Size"
  ) |> 
  arrange(alpha, power, hr, n_east) |> 
  mutate(surv_t = rep(c(0.1, 0.3, 0.6, 0.9), n()/4)) |> 
  mutate(
    across(names(params$surv$fixed$list),
    \(x){closest(x, params$surv$fixed$list[[cur_column()]])})
  )

## nQuery ----
nquery$surv$fixed$raw <- read.csv2("data-raw/nquery_grid_fixed.csv", dec = ",")

transposed <- data.frame(t(nquery$surv$fixed$raw[-1]))
colnames(transposed) <- nquery$surv$fixed$raw[,1] 

nquery$surv$fixed$res <- 
  transposed |> 
  tibble() |> 
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    hr = "Hazard Ratio, h=??/???",
    lambda2 = "Group 2 Exponential Parameter, ???", # Group 2 is the control arm
    e_nquery = "Total Number of Events Required, E?",
    n_nquery = "Sample Size per Group, n?"
  ) |> 
  mutate(
    across(c(alpha, power, hr, lambda2), \(x){parse_number(x, locale = locale(decimal_mark = ","))}),
    lambda2 = signif(lambda2, 1),
    surv_t = case_when(
      lambda2 == 0.80 ~ 0.1,
      lambda2 == 0.40 ~ 0.3,
      lambda2 == 0.20 ~ 0.6,
      lambda2 == 0.04 ~ 0.9,
      .default = NA_real_
    ),
    across(c(e_nquery, n_nquery), as.numeric),
    power = power/100,
    n_nquery = 2*n_nquery # introduce an error cause all n will be even.
  ) |> 
  select(-lambda2) |> 
  mutate(
    across(names(params$surv$fixed$list),
    \(x){closest(x, params$surv$fixed$list[[cur_column()]])})
  )


## Rashnu ----
rashnu$surv$fixed$res <- 
  params$surv$fixed$table |> 
  mutate(
    nested_res = pmap(params$surv$fixed$table, rashnu$surv$fixed$wrapper)
  ) |> 
  unnest(nested_res) |> 
  rename(e_rashnu = e, n_rashnu = n)

# Combined results ----
combined$surv$fixed$res <- 
  reduce(
    list(
      rpact$surv$fixed$res,
      east$surv$fixed$res, 
      nquery$surv$fixed$res, 
      rashnu_res_surv_fixed),
    \(x, y){inner_join(x, y, by = join_by(alpha, power, hr, surv_t))}
  ) |> 
  mutate(relevancy = evaluate_relevancy(alpha, power, hr)) |> 
  mutate(relevancy = fct_relevel(relevancy, c("low", "medium", "high")))

# Tables and figures ----
n_ratio_by_method_surv_fixed <- 
  combined$surv$fixed$res |> 
  mutate(
    rpact = n_rpact/n_east,
    nquery = n_nquery/n_east,
    rashnu = n_rashnu/n_east
  ) |> 
  select(c(alpha, power, hr, surv_t, relevancy, n_east, rpact, nquery, rashnu)) |> 
  pivot_longer(c(rpact, nquery, rashnu), names_to = "method", values_to = "n_ratio")

p_n_ratio_by_method_surv_fixed <-
  ggplot(n_ratio_by_method_surv_fixed) +
  aes(x = n_east, y = n_ratio, color = method) +
  geom_point() +
  geom_hline(yintercept=1) +
  facet_wrap(~ relevancy, scales = "free") +
  labs(
    title = "N ratio according to method and relevancy",
    subtitle = "base sample size from East"
  )
