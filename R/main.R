# Initialisation ----
source("R/init.R")

boxx(
  "SSC",
  float = "center",
  col = "blue",
  border_style = "double",
  padding = c(0, 15, 0, 15)
)

# Binary sample size comparison ----
cli_h1("SSC-Binary")
source("R/comparison/comp_bin_fixed_pooled.R")
source("R/comparison/comp_bin_fixed_exact.R")
source("R/comparison/comp_bin_gs.R")
source("R/comparison/comp_bin_one_arm.R")
source("R/comparison/comp_bin_one_arm_exact.R")

# Survival sample size comparison ----
cli_h1("SSC-Survival")
source("R/comparison/comp_surv_fixed.R")
source("R/comparison/comp_surv_gs.R")
source("R/comparison/comp_surv_one_arm.R")

# Additional analysis ----
cli_h1("SSC-side-analysis")
source("R/side-analysis/lakatos_n_lan.R")
source("R/side-analysis/bin2arms_exact.R")

# Export results ----
# write_rds(ssc, "outputs/ssc.rds")
