# Effect of Environmental Regulation on Air Pollution and Infant Mortality in India

## Project Overview

This repository contains the replication and extension of:
**"Environmental Regulations, Air and Water Pollution, and Infant Mortality in India"** by **Michael Greenstone and Rema Hanna** (*American Economic Review*, 2014).

This work is part of the **M2 program at ENSAE Paris**, for the course **"Empirical Environmental Economics"** taught by Marion Leroutier (Topic 5).

### Authors (Group 5)
- **Gabriel Orsatti**
- **Matis Bruneau**

---

## Research Questions

1. **Critique of the original TWFE design:** What problems arise from using Two-Way Fixed Effects in a staggered adoption setting? We draw on the recent literature (de Chaisemartin & D'Haultfoeuille, 2023; Goodman-Bacon, 2021) and apply the Bacon decomposition to diagnose problematic comparisons.

2. **Alternative estimator:** We implement the Callaway & Sant'Anna (2021) group-time ATT estimator and the Sun & Abraham (2021) interaction-weighted estimator to produce heterogeneity-robust estimates of the catalytic converter policy and the Supreme Court Action Plan on air pollution and infant mortality.

---

## Key Results

### Air Pollution (Catalytic Converter Policy)

| Outcome | TWFE 5yr Effect | C&S Simple ATT | Sun & Abraham ATT |
|---------|----------------|-----------------|-------------------|
| PM (SPM) | -21.41 (22.50), p=0.342 | Estimation failed | -7.48 (16.90) |
| SO2 | -16.40 (5.90), p=0.006 | **-4.72 (1.90), p=0.013** | -1.86 (4.12) |
| NO2 | 1.22 (7.13), p=0.864 | -0.64 (2.16), p=0.767 | 5.04 (6.29) |

*Standard errors in parentheses. C&S uses never-treated cities as controls with population weights.*

### Infant Mortality (Catalytic Converter Policy)

| Estimator | ATT | SE | p-value |
|-----------|-----|-----|---------|
| C&S Simple | -2.62 | 5.85 | 0.655 |
| C&S Dynamic | -5.75 | 4.03 | 0.154 |

### Summary

- **SO2:** The only pollutant with a statistically significant reduction under heterogeneity-robust estimation. The C&S ATT (-4.72) is two to three times smaller in magnitude than the TWFE estimate (-16.40), consistent with TWFE overstating effects under staggered adoption.
- **PM (SPM):** The C&S estimator fails to produce a reliable estimate due to insufficient within-group variation. The TWFE estimate is already insignificant.
- **NO2:** No significant effect detected by any method.
- **Infant mortality:** Point estimates are negative but statistically insignificant, in contrast to the original paper which reports significant health improvements.
- **Overall:** The original conclusions of Greenstone & Hanna (2014) are partially supported (SO2) but considerably weakened for other outcomes when evaluated with modern DiD methods.

---

## Data Description

The analysis relies on two city-year panel datasets:
- `combined.dta`: 2,940 observations, 140 cities, 1987-2007. Air pollution outcomes (`e_spm_mean`, `e_so2_mean`, `e_no2_mean`) and policy indicators (`catconverter`, `actionplan_sc`).
- `im_air.dta`: 2,331 observations, 111 cities, 1987-2007. Infant mortality (`c_IM`, deaths per thousand births) and the same policy indicators.

---

## Repository Structure

```text
├── Data/                        # Datasets (.dta files)
├── R/                           # R scripts (run in order)
│   ├── 00_install_packages.R    # Install all required packages
│   ├── 01_data_loading_preparation.R  # Load .dta files, create treatment variables
│   ├── 02_descriptive_statistics.R    # Summary statistics and trend figures
│   ├── 03_replication_twfe.R          # Replicate TWFE event-study and trend-break
│   ├── 04_twfe_diagnostics.R          # Goodman-Bacon decomposition
│   ├── 05_alternative_estimator.R     # Callaway & Sant'Anna + Sun & Abraham
│   └── 06_comparison_visualization.R  # TWFE vs C&S comparison figures and tables
├── Output/                      # All generated outputs
│   ├── figures/                 # PDF figures (event studies, comparisons, etc.)
│   ├── *.RData                  # Intermediate R results
│   └── *.csv                    # Summary tables
├── paper/                       # Reference papers (Greenstone & Hanna, de Chaisemartin)
├── main.tex                     # LaTeX source for the final report
├── Topic_5_report.pdf           # Compiled final report
└── README.md
```

## How to Run

1. Place the `.dta` datasets in the `Data/` folder.
2. Open RStudio and set the working directory to the project root.
3. Run the scripts in order: `00` through `06`.

All outputs (figures, tables, RData files) are saved automatically in `Output/`.

The final paper is compiled from `main.tex` (requires LaTeX with `booktabs`, `natbib`, `setspace`, `float` packages).

## References

- Greenstone, M. and Hanna, R. (2014). "Environmental Regulations, Air and Water Pollution, and Infant Mortality in India." *American Economic Review*, 104(10):3038-3072.
- Callaway, B. and Sant'Anna, P.H.C. (2021). "Difference-in-Differences with Multiple Time Periods." *Journal of Econometrics*, 225(2):200-230.
- de Chaisemartin, C. and D'Haultfoeuille, X. (2023). "Two-Way Fixed Effects and Differences-in-Differences with Heterogeneous Treatment Effects: A Survey." *The Econometrics Journal*, 26(3):C1-C30.
- Goodman-Bacon, A. (2021). "Difference-in-Differences with Variation in Treatment Timing." *Journal of Econometrics*, 225(2):254-277.
- Sun, L. and Abraham, S. (2021). "Estimating Dynamic Treatment Effects in Event Studies with Heterogeneous Treatment Effects." *Journal of Econometrics*, 225(2):175-199.
