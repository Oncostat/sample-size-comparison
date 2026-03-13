

<!-- README.md is generated from README.Qmd. Please edit that file -->

# Oncostat’s Sample Size Comparison <a href='https://github.com/Oncostat/sample-size-comparison'><img src='man\figures\SSC-HexSticker.png' align="right" height="175" /></a>

<!-- badges: start -->

[![Oncostat-affiliation](https://img.shields.io/badge/Oncostat-green.svg)](https://oncostat.github.io/)
[![CC BY
4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)

This repository presents a systematic comparison of **sample size
calculations implemented in R** with those from two widely used
commercial tools: **East** and **nQuery**.

## R as an alternative to East and nQuery.

Below is a concise overview of the main findings from the *Sample Size
Comparison* project.  
The table compares, for each endpoint and design type, the statistical
test used, and how well the implemented R calculations matched the
software(East or nQuery) reference values.

| Endpoints | Design type                   | Test         | R/Software matching |
|:----------|:------------------------------|:-------------|:--------------------|
| survival  | 2‑Arm fixed design            | Log‑rank     | 🟢 Perfect          |
| survival  | 2‑Arm group‑sequential design | Log‑rank     | 🟡 Good             |
| survival  | 1‑Arm fixed design            | Log‑rank     | 🔵 Poor             |
| Binary    | 2‑Arm fixed design            | Exact        | 🟡 Good             |
| Binary    | 2‑Arm fixed design            | Z‑Pooled     | 🟢 Perfect          |
| Binary    | 2‑Arm group‑sequential design | Z‑Pooled     | 🟢 Perfect          |
| Binary    | 1‑Arm fixed design            | Exact        | 🟡 Good             |
| Binary    | 1‑Arm fixed design            | 1‑Arm Z test | 🟢 Perfect          |

Legend:  
🟢 **Perfect** = identical or extremely close values  
🟡 **Good** = small but acceptable discrepancies  
🔵 **Poor** = substantial differences requiring further investigation

------------------------------------------------------------------------

## [Website](https://oncostat.github.io/sample-size-comparison/)

Browse the interactive website to explore results, methodology, and
reproducible R code.

<p align="center">

<a href='https://oncostat.github.io/sample-size-comparison/'>
<img src='man\figures\ssc-website-screenshot.png' height="400" /> </a>
</p>

------------------------------------------------------------------------

## [Slides (in french)](https://oncostat.github.io/sample-size-comparison/presentation/presentation-fr)

Slides from the 25/02/2026 presentation at Gustave Roussy

<p align="center">

<a href='https://oncostat.github.io/sample-size-comparison/presentation/presentation-fr'>
<img src='man\figures\presentation.png' height="400" /> </a>
</p>

------------------------------------------------------------------------

## R directory Structure

Quick overview of how the entire SSC project is organised:

- **a central `main.R` file**, from which the full pipeline is
  executed;  
- **modular comparison scripts**, one per design;  
- **dedicated `functions/` and `wrappers/` directory** for all helpers
  and wrappers;  
- **a small S7 layer** holding the two core classes (`ssc_design` and
  `ssc_results`).

``` ascii
R/
├── init.R
├── main.R
├── comparison/
│   ├── comp_bin_fixed_exact.R
│   ├── comp_bin_fixed_pooled.R
│   ├── comp_bin_gs.R
│   ├── comp_bin_one_arm_exact.R
│   ├── comp_bin_one_arm.R
│   ├── comp_surv_fixed.R
│   ├── comp_surv_gs.R
│   └── comp_surv_one_arm.R
├── functions/
│   ├── checks.R
│   ├── e_ratio.R
│   ├── evaluate_relevancy.R
│   ├── helpers.R
│   ├── n_ratio.R
│   └── exact_wrapper.R
├── S7/
│   ├── generics.R
│   ├── ssc_design.R
│   └── ssc_results.R
├── side-analysis/
│   ├── bin2arms_exact.R
│   └── lakatos_n_lan.R
└── wrappers/
    ├── ahern_wrapper.R
    ├── bbssr_wrapper.R
    ├── exact_wrapper.R
    ├── gsdesign2_wrapper.R
    ├── oa2s_wrapper.R
    ├── rashnu_wrapper.R
    ├── rpact_wrapper.R
    └── sssas_wrapper.R
```

## License

[Sample Size
Comparison](https://github.com/Oncostat/sample-size-comparison) © 2026
by [Oncostat](https://oncostat.github.io/) is licensed under [CC BY
4.0](http://creativecommons.org/licenses/by/4.0/)

[![](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)
