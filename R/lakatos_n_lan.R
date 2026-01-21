# Lakatos and Lan (1992)

# Lakatos and Lan table ----
design_table = tribble(
  ~surv_t, ~hr, ~accrual_time, ~n_L, ~n_F, ~n_RGS, ~power_L, ~power_F, ~power_RGS,
  0.8, 0.667, 1, 1617, 1628, 1640, 90.2, 90.4, 90.6,
  0.8, 0.667, 5, 2017, 2024, 2046, 90.6, 90.7, 91.0,
  0.8, 0.667, 9, 2724, 2709, 2764, 90.3, 90.1, 90.7,

  0.8, 0.50, 1, 638, 649, 664, 90.1, 90.6, 91.2,
  0.8, 0.50, 5, 798, 807, 831, 90.7, 91.0, 91.8,
  0.8, 0.50, 9, 1079, 1081, 1124, 90.1, 90.2, 91.2,

  0.8, 0.25, 1, 230, 241, 269, 91.9, 93.0, 95.3,
  0.8, 0.25, 5, 289, 299, 338, 92.2, 93.0, 95.5,
  0.8, 0.25, 9, 392, 401, 459, 91.6, 92.2, 95.1,

  0.2, 0.667, 1, 360, 370, 363, 89.6, 90.4, 89.8,
  0.2, 0.667, 5, 414, 419, 418, 90.5, 90.8, 90.8,
  0.2, 0.667, 9, 528, 509, 534, 89.8, 88.7, 90.1,

  0.2, 0.50, 1, 134, 144, 138, 89.7, 91.7, 90.5,
  0.2, 0.50, 5, 156, 164, 161, 89.9, 91.3, 90.8,
  0.2, 0.50, 9, 200, 200, 207, 89.7, 89.7, 90.7,

  0.2, 0.25, 1, 43, 53, 48, 90.2, 95.1, 93.0,
  0.2, 0.25, 5, 51, 61, 58, 90.6, 94.8, 93.8,
  0.2, 0.25, 9, 66, 74, 76, 90.2, 93.1, 93.7
)

# Params ----
params$surv$fixed$list_lnl = list(
  alpha = 0.05,
  power = 0.9,
  hr = c(0.667, 0.5, 0.25),
  surv_t = c(0.8, 0.2),
  event_time = 10,
  accrual_time = c(1, 5, 9),
  follow_up_time = c(9, 5, 1)
)

# Get all combinaisons of params with 10 years study duration
params$surv$fixed$table_lnl <- 
  params$surv$fixed$list_lnl |> 
  expand.grid() |> 
  as_tibble() |> 
  filter(accrual_time + follow_up_time == 10)

# Methods ----

## Rpact ----
rpact$surv$fixed$res_lnl <- 
  params$surv$fixed$table_lnl |> 
  mutate(
    nested_res = pmap(params$surv$fixed$table_lnl, rpact$surv$fixed$wrapper)
    ) |> 
  unnest(nested_res) |> 
  rename(e_rpact = e, n_rpact = n)

## East ----
east$surv$fixed$raw_lnl <- read_csv("data-raw/east_lakatos_n_lan.csv")

east$surv$fixed$res_lnl <- 
  east$surv$fixed$raw_lnl |> 
  select(
    alpha = "Specified α",
    power = "Power",
    hr = "Hazard Ratio (Alt.)",
    accrual_time = "Comm. Accr. (Dur.)",
    e_east = "Maximum Events",
    n_east = "Sample Size"
  ) |> 
  arrange(hr, accrual_time, n_east) |> 
  mutate(surv_t = rep(c(0.2, 0.8), n()/2)) |> 
  mutate(
    across(c(alpha, power, hr, accrual_time),
    \(x){closest(x, params$surv$fixed$list_lnl[[cur_column()]])})
  )

## nQuery ----
nquery$surv$fixed$raw_lnl <- read.csv2("data-raw/nquery_lakatos_n_lan.csv", dec = ",")

transposed <- data.frame(t(nquery$surv$fixed$raw_lnl[-1]))
colnames(transposed) <- nquery$surv$fixed$raw_lnl[,1] 

nquery$surv$fixed$res_lnl <- 
  transposed |> 
  tibble() |> 
  select(
    alpha = "Test Significance Level, ??",
    power = "Power (%)?",
    hr = "Hazard Ratio, h=??/???",
    lambda1 = "Group 1 Exponential Parameter, ???",
    accrual_time = "Length of Accrual Period, a?",
    e_nquery = "Total Number of Events Required, E?",
    n_nquery = "Sample Size per Group, n?"
  ) |> 
  mutate(
    across(
      c(alpha, power, hr, lambda1),
      \(x){parse_number(x, locale = locale(decimal_mark = ","))}
    ),
    lambda1 = signif(lambda1, 1),
    hr = signif(hr, 3),
    accrual_time = as.numeric(accrual_time),
    surv_t = case_when(
      lambda1 == 0.2 ~ 0.2,
      lambda1 == 0.02 ~ 0.8,
      .default = NA_real_
    ),
    across(c(e_nquery, n_nquery), as.numeric),
    power = power/100,
    n_nquery = 2*n_nquery # introduce an error cause all n will be even.
  ) |> 
  select(-lambda1) |> 
  mutate(
    across(c(alpha, power, hr, accrual_time),
    \(x){closest(x, params$surv$fixed$list_lnl[[cur_column()]])})
  )


## Rashnu ----
rashnu$surv$fixed$res_lnl <- 
  params$surv$fixed$table_lnl |> 
  mutate(
    nested_res = pmap(params$surv$fixed$table_lnl, rashnu$surv$fixed$wrapper)
  ) |> 
  unnest(nested_res) |> 
  rename(e_rashnu = e, n_rashnu = n)


# Comparison ----
representation_ll <- function(data_ll){
  data_ll |> 
  select(surv_t, hr, accrual_time, starts_with("n_")) |> 
  arrange(desc(surv_t), desc(hr))
}

combined_res_ll <- 
  list(rpact$surv$fixed$res_lnl, east$surv$fixed$res_lnl, nquery$surv$fixed$res_lnl, rashnu$surv$fixed$res_lnl, design_table) |> 
  map(representation_ll) |> 
  reduce(\(x, y){inner_join(x, y, by = join_by(hr, surv_t, accrual_time))})

#combined_res_ll  |> gt(rowname_col = c("surv_t", "hr", "accrual_time"))
