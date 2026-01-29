

<!-- README.md is generated from README.Qmd. Please edit that file -->

# Oncostat’s Sample Size Comparison <a href='https://github.com/Oncostat/sample-size-comparison'><img src='man\figures\SCC-HexSticker.png' align="right" height="175" /></a>

<!-- badges: start -->

[![](https://img.shields.io/badge/Oncostat-green.svg)](https://oncostat.github.io/)

## R as an alternative to East and nQuery for computing sample size.

The article of the comparison is available
[here](https://oncostat.github.io/sample-size-comparison/).

### Designs & compared method

| Endpoints | Design Type | Computation | Software | R package | Progression |
|:---|:--:|---:|:---|:---|:---|
| Survival | Fixed |  | 🟧East, 🟦nQuery | 🟥Rpact, 🟪Rashnu | 🟢 DONE |
| Survival | Group-Sequential |  | 🟧East, 🟦nQuery | 🟥Rpact, 🟫gsDesign2 | 🟢 DONE |
| Binary | Fixed | Exact | 🟧East, 🟦nQuery | 🟨bbssr | 🟢 DONE |
| Binary | Fixed | Pooled | 🟧East, 🟦nQuery | 🟥Rpact, 🟨bbssr | 🟢 DONE |
| Binary | Fixed | Unpooled | 🟧East, 🟦nQuery |  | 🟡 IN PROGRESS |
| Binary | Group-Sequential | Pooled | 🟧East | 🟥Rpact | 🟡 IN PROGRESS |
