# Initialisation ----
source("R/init.R")
# Survival sample size comparison ----
cli_h1("SSC Survival")
# source("R/comp_surv_fixed.R")
# source("R/lakatos_n_lan.R")
source("R/comp_surv_gs.R")
# Binary sample size comparison ----
cli_h1("SSC Binary")
source("R/comp_bin_fixed.R")
source("R/comp_bin_gs.R")

# RDS List
# rds_list <- lst(params, rpact, east, nquery, rashnu, combined)
#write_rds(rds_list, "everything.rds")
# write_rds(ssc, "ssc.rds")
