# load packages 
library(tidyverse)
library(rpact)
library(rashnu)
library(gt)
library(cli)
library(rlang)
library(memoise)
library(gsDesign2)
library(GGally)

select = dplyr::select # Also a function select in MASS

# RDS List
rds_list <- lst()

# List to export
params <- lst()
rpact <- lst()
east <- lst()
nquery <- lst()
rashnu <- lst()

# Source functions
source("R/functions/checks.R")
source("R/functions/helpers.R")
source("R/functions/evaluate_relevancy.R")
source("R/functions/rpact_wrapper.R")
source("R/functions/rashnu_wrapper.R")
