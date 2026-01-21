# Params ----
params$surv$gs$list = list(
  alpha = c(0.01, 0.05, 0.1),
  power = c(0.7, 0.8, 0.9),
  hr = c(0.3, 0.6, 0.9),
  surv_t = c(0.2, 0.5, 0.8) # Survival at 3-year
)
# accrual_time 3, Study duration 6
# Efficacy: Lan-DeMets OF
# Futility: Lan-DeMets OF

# Get all combinaisons of params
params$surv$gs$table <- 
  params$surv$gs$list |> 
  expand.grid() |> 
  as_tibble()

# Methods ----
## Rpact ----
#see [rpact doc](https://www.rpact.org/vignettes/planning/rpact_survival_examples/#sample-size-calculation-for-trials-with-interim-analyses)
rpact$surv$gs$res <- 
  params$surv$gs$table |> 
  mutate(
    nested_res = pmap(params$surv$gs$table, rpact$surv$gs$wrapper)
  ) |> 
  unnest(nested_res) |> 
  rename(e_rpact = e, n_rpact = n)

## gsDesign2 ----
gsdesign2_res_surv_gs <-
  params$surv$gs$table |> 
  mutate(
    nested_res = pmap(params$surv$gs$table, gsdesign2_wrapper_gs)
  ) |> 
  unnest(nested_res) |> 
  rename(e_gsdesign2 = e, n_gsdesign2 = n)

## East ----
filelist_east_surv_gs <- list.files(path = "data-raw/East_gsdesign", full.names = TRUE)

east$surv$gs$raw <- read_csv(filelist_east_surv_gs)

east$surv$gs$res <-
  east$surv$gs$raw |> 
  select(
    alpha = Alpha__AAA,
    power = Power__AAA,
    hr = HazardRatio__AAAMA,
    surv_t = CumPercSurvCtrl__AAAMAA_AE, #In percentages
    e_east = TotSmplSiz__AB,
    n_east = A1x0__ABBE_BA
  ) |> 
  mutate(surv_t = surv_t/100) |> 
  mutate(
    across(names(params$surv$gs$list),
    \(x){closest(x, params$surv$gs$list[[cur_column()]])})
  )


# Comparision ----
combined_res_surv_gs <- 
  reduce(
    list(rpact$surv$gs$res, gsdesign2_res_surv_gs, east$surv$gs$res),
    \(x, y){inner_join(x, y, by = join_by(alpha, power, hr, surv_t))}
  )

p_pairs <-
  combined_res_surv_gs |> 
  select(starts_with("n_")) |> 
  ggpairs() +
  labs(title = "GS-Design Sample-Size by method")
