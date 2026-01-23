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
library(S7)

select = dplyr::select # Also a function select in MASS

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

boxx(
  "SSC",
  float = "center",
  col = "blue",
  border_style = "double",
  padding = c(0, 15, 0, 15)
)
