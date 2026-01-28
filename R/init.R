# load packages
suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(gt)
  library(cli)
  library(rlang)
  library(memoise)
  library(GGally)
  library(S7)
}))

# List to export
ssc <- lst()
wrapper <- lst()

# Source functions
source("R/functions/checks.R")
source("R/functions/helpers.R")
source("R/functions/evaluate_relevancy.R")
source("R/functions/rpact_wrapper.R")
source("R/functions/rashnu_wrapper.R")
source("R/functions/gsdesign2_wrapper.R")

# Memoise wrappers
wrapper = map(
  wrapper,
  ~ memoise(.x, cache = cachem::cache_disk("the_cache_of_the_memoise"))
)

# Source S7 classes and methods
source("R/S7/generics.R")
source("R/S7/ssc_design.R")
source("R/S7/ssc_results.R")
