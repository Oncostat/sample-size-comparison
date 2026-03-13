#* SETUP FILE FOR ADDITIONAL-CONTENT QMD

suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(gt)
  library(S7)
  library(plotly)
  library(crosstalk)
  library(bslib)
  library(bsicons)
  library(DT)
}))

# Source S7 classes and methods
# source("../../R/S7/generics.R")
# source("../../R/S7/ssc_design.R")
# source("../../R/S7/ssc_results.R")

# Theming
source("../theming_flatly.R")
source("../theming_darkly.R")
