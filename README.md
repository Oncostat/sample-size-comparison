

<!-- README.md is generated from README.Qmd. Please edit that file -->

# Oncostat’s Sample Size Comparison <a href='https://github.com/Oncostat/sample-size-comparison'><img src='man\figures\SCC-HexSticker.png' align="right" height="175" /></a>

<!-- badges: start -->

[![](https://img.shields.io/badge/Oncostat-green.svg)](https://oncostat.github.io/)

## R as an alternative to East and nQuery for computing sample size.

The article of the comparison is available
[here](https://oncostat.github.io/sample-size-comparison/).

### Designs & compared method

| Endpoints | Design Type | Computation | Software | R package |
|:---|:---|---:|:---|:---|
| Survival | Two-arm Fixed |  | 🟧East, 🟦nQuery | 🟥Rpact, 🟪Rashnu, 🟫gsDesign2 |
| Survival | One-arm Fixed |  | 🟧East, 🟦nQuery | 🟥Rpact, 🟪Rashnu |
| Survival | Two-arm Group-Sequential |  | 🟧East, 🟦nQuery | 🟥Rpact, 🟫gsDesign2 |
| Binary | Two-arm Fixed | Exact | 🟧East, 🟦nQuery | 🟨bbssr |
| Binary | Two-arm Fixed | Pooled | 🟧East, 🟦nQuery | 🟥Rpact, 🟨bbssr |
| Binary | Two-arm Fixed | Unpooled | 🟧East, 🟦nQuery |  |
| Binary | One-arm Fixed | Exact | 🟧East | A’Hern(no package) |
| Binary | One-arm Fixed | Non_exact? | 🟧East, 🟦nQuery | 🟥Rpact |
| Binary | Two-arm Group-Sequential | Pooled | 🟧East | 🟥Rpact |
