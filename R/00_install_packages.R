# ==============================================================================
# 00_install_packages.R
# Installs all R packages required for the project
# Run this script ONCE before running the other scripts
# ==============================================================================

packages_needed <- c(
  # Utilities
  "here",
  
  # Data manipulation
  "haven",          # Read .dta (Stata) files
  "dplyr",         # Data wrangling
  "tidyr",         # Reshape data
  "data.table",    # Fast data operations


  # Econometrics - Standard
  "fixest",        # Fast fixed effects estimation (feols, sunab)
  "lmtest",        # Coefficient tests (coeftest, waldtest)
  "sandwich",      # Robust standard errors
  "plm",           # Panel data models

  # Econometrics - DiD (heterogeneity-robust)
  "did",           # Callaway & Sant'Anna (2021) estimator
  "did2s",         # Gardner (2021) two-stage DiD
  "bacondecomp",   # Goodman-Bacon decomposition

  # Visualization
  "ggplot2",       # Plotting
  "patchwork",     # Combine ggplots
  "ggthemes",      # Clean themes

  # Tables and output
  "modelsummary",  # Regression tables
  "knitr",         # kable for tables
  "stargazer",     # Alternative regression tables
  "kableExtra"     # Enhanced tables
)

installed <- installed.packages()[, "Package"]
to_install <- packages_needed[!packages_needed %in% installed]

if (length(to_install) > 0) {
  cat("Installing", length(to_install), "packages:\n")
  cat(paste(" -", to_install, collapse = "\n"), "\n\n")
  install.packages(to_install, repos = "https://cran.r-project.org")
} else {
  cat("All packages are already installed.\n")
}

cat("\nVerification:\n")
for (pkg in packages_needed) {
  status <- if (require(pkg, character.only = TRUE, quietly = TRUE)) "OK" else "FAILED"
  cat(sprintf("  %-15s : %s\n", pkg, status))
}
