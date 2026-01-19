library(tibble)
library(readr)

param_list_1 = list(
  alpha = c(0.01, 0.05, 0.1, 0.20, 0.49),
  power = c(0.51, 0.8, 0.9, 0.99),
  hr = c(0.1, 0.5, 0.7, 0.9, 0.99),
  surv_t = c(0.1, 0.3, 0.6, 0.9)
)

# Get all combinaisons of params
param_table_1 <- 
  param_list_1 |> 
  expand.grid() |> 
  as_tibble()

write_csv(param_table, "data/param_table_1.csv")