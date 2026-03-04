# load packages
suppressPackageStartupMessages(suppressWarnings({
  library(S7)
  library(gt)
  library(cli)
  library(rlang)
  library(tidyverse)
}))

# Error rate 
er_rate <- 0.1 # error rate 10%

# List to export
ssc <- lst()
wrapper <- lst()

# Source functions
## Intermediate functions
source("R/functions/checks.R")
source("R/functions/n_ratio.R")
source("R/functions/helpers.R")
source("R/functions/evaluate_relevancy.R")

# Wrappers
source("R/functions/oa2s_wrapper.R")
source("R/functions/rpact_wrapper.R")
source("R/functions/bbssr_wrapper.R")
source("R/functions/sssas_wrapper.R")
source("R/functions/ahern_wrapper.R")
source("R/functions/rashnu_wrapper.R")
source("R/functions/gsdesign2_wrapper.R")

# Memoise wrappers
wrapper = map(
  wrapper,
  ~ memoise::memoise(.x, cache = cachem::cache_disk("the_cache_of_the_memoise"))
)

# Source S7 classes and methods
source("R/S7/generics.R")
source("R/S7/ssc_design.R")
source("R/S7/ssc_results.R")
