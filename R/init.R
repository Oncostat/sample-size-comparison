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
source("R/functions/e_ratio.R")
source("R/functions/helpers.R")
source("R/functions/evaluate_relevancy.R")

# Wrappers
source("R/wrappers/oa2s_wrapper.R")
source("R/wrappers/rpact_wrapper.R")
source("R/wrappers/bbssr_wrapper.R")
source("R/wrappers/sssas_wrapper.R")
source("R/wrappers/ahern_wrapper.R")
source("R/wrappers/rashnu_wrapper.R")
source("R/wrappers/gsdesign2_wrapper.R")

# Memoise wrappers
wrapper = map(
  wrapper,
  ~ memoise::memoise(.x, cache = cachem::cache_disk("the_cache_of_the_memoise"))
)

# Source S7 classes and methods
source("R/S7/generics.R")
source("R/S7/ssc_design.R")
source("R/S7/ssc_results.R")

# BOXX
boxx(
  "SSC",
  float = "center",
  col = "blue",
  border_style = "double",
  padding = c(0, 15, 0, 15)
) |>
  print()
