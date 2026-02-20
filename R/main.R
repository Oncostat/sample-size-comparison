# Initialisation ----
source("R/init.R")

boxx(
  "SSC",
  float = "center",
  col = "blue",
  border_style = "double",
  padding = c(0, 15, 0, 15)
)

# Survival sample size comparison ----
cli_h1("SSC Survival")
source("R/comp_surv_fixed.R")
source("R/lakatos_n_lan.R")
source("R/comp_surv_gs.R")
source("R/comp_surv_one_arm.R")
# Binary sample size comparison ----
cli_h1("SSC Binary")
source("R/comp_bin_fixed_pooled.R")
# source("R/comp_bin_fixed_exact.R")
source("R/comp_bin_gs.R")
source("R/comp_bin_one_arm.R")
source("R/comp_bin_one_arm_exact.R")

# RDS List
# rds_list <- lst(params, rpact, east, nquery, rashnu, combined)
#write_rds(rds_list, "everything.rds")
# write_rds(ssc, "ssc.rds")

# USING BETTER COMMENTS TO COLORS COMMENTS
#* IMPORTANT : IN GREEN AND BOLD
#? QUESTION IN BLUE
#! An alert
# TODO : here is a todo (can open github issue/ delegate to coding agent)
#
