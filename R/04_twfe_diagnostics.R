# ==============================================================================
# 04_twfe_diagnostics.R
# Diagnoses the TWFE problem in Greenstone & Hanna (2014)
# - Goodman-Bacon decomposition (identifies forbidden comparisons)
# - Visualizes weight distribution across 2x2 DID components
# This script directly supports the "Empirical Design Critique" section of our paper
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)
library(bacondecomp)
library(patchwork)

# --- Load prepared data -------------------------------------------------------
project_root <- here::here()
if (!file.exists(file.path(project_root, "output", "prepared_air.RData"))) {
  project_root <- getwd()
}
load(file.path(project_root, "output", "prepared_air.RData"))
load(file.path(project_root, "output", "prepared_im.RData"))

output_dir <- file.path(project_root, "output")
fig_dir <- file.path(output_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. GOODMAN-BACON DECOMPOSITION
# ==============================================================================

# The bacondecomp package requires a balanced-ish panel with a binary treatment
# that switches on (staggered adoption). We use the catalytic converter policy.

cat("\n========== Goodman-Bacon Decomposition ==========\n")
cat("Decomposing the static TWFE estimator into its 2x2 DID components\n\n")

# --- 1.1 Prepare data for Bacon decomposition --------------------------------
# bacondecomp needs: balanced panel (or at least strongly balanced), binary treatment

prep_bacon <- function(data, pollutant_var) {
  df <- data %>%
    filter(!is.na(.data[[pollutant_var]])) %>%
    # Keep only the treatment indicator (binary: catalytic converter in force)
    mutate(treated = as.integer(catconverter == 1)) %>%
    select(city_id, year, outcome = all_of(pollutant_var), treated) %>%
    # bacondecomp works best with a balanced panel
    group_by(city_id) %>%
    mutate(n_years = n()) %>%
    ungroup()

  # Report balance
  cat(sprintf("  %s: %d obs, %d cities, years per city: min=%d, max=%d, median=%d\n",
              pollutant_var,
              nrow(df), n_distinct(df$city_id),
              min(df$n_years), max(df$n_years), median(df$n_years)))
  df
}

bacon_spm <- prep_bacon(air, "e_spm_mean")
bacon_so2 <- prep_bacon(air, "e_so2_mean")
bacon_no2 <- prep_bacon(air, "e_no2_mean")

# --- 1.2 Run Bacon decomposition ----------------------------------------------

run_bacon <- function(data, pollutant_name) {
  cat(sprintf("\n--- Bacon Decomposition: %s ---\n", pollutant_name))
  tryCatch({
    df_bacon <- bacon(
      formula = outcome ~ treated,
      data = data,
      id_var = "city_id",
      time_var = "year"
    )

    cat(sprintf("  Overall TWFE estimate: %.3f\n", sum(df_bacon$estimate * df_bacon$weight)))
    cat(sprintf("  Number of 2x2 DID comparisons: %d\n", nrow(df_bacon)))

    # Summarize by type
    type_summary <- df_bacon %>%
      group_by(type) %>%
      summarise(
        n = n(),
        weight_sum = sum(weight),
        avg_estimate = weighted.mean(estimate, weight),
        .groups = "drop"
      )

    cat("\n  Decomposition by comparison type:\n")
    print(type_summary)

    # Flag forbidden comparisons (Later vs Earlier treated)
    forbidden_weight <- type_summary %>%
      filter(grepl("Later|Earlier", type, ignore.case = TRUE)) %>%
      pull(weight_sum) %>%
      sum()
    cat(sprintf("\n  Weight on 'forbidden' comparisons (treated vs treated): %.3f\n", forbidden_weight))

    df_bacon$pollutant <- pollutant_name
    return(df_bacon)

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    cat("  Note: bacondecomp requires a strongly balanced panel. Trying with balanced subset...\n")

    # Try with balanced subset (cities observed every year in their range)
    year_range <- range(data$year)
    all_years <- seq(year_range[1], year_range[2])
    balanced <- data %>%
      group_by(city_id) %>%
      filter(n() == length(all_years)) %>%
      ungroup()

    if (nrow(balanced) > 0 && n_distinct(balanced$city_id) >= 3) {
      cat(sprintf("  Balanced subset: %d cities, %d obs\n", n_distinct(balanced$city_id), nrow(balanced)))
      df_bacon <- bacon(
        formula = outcome ~ treated,
        data = balanced,
        id_var = "city_id",
        time_var = "year"
      )
      df_bacon$pollutant <- pollutant_name
      return(df_bacon)
    } else {
      cat("  Cannot construct balanced panel. Skipping.\n")
      return(NULL)
    }
  })
}

bacon_result_spm <- run_bacon(bacon_spm, "PM")
bacon_result_so2 <- run_bacon(bacon_so2, "SO2")
bacon_result_no2 <- run_bacon(bacon_no2, "NO2")

# --- 1.3 Visualization of Bacon decomposition ---------------------------------

plot_bacon <- function(bacon_df, title) {
  if (is.null(bacon_df)) return(ggplot() + labs(title = paste(title, "- No data")))

  ggplot(bacon_df, aes(x = weight, y = estimate, color = type, shape = type)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(
      yintercept = sum(bacon_df$estimate * bacon_df$weight),
      linetype = "solid", color = "black", linewidth = 0.5
    ) +
    labs(
      title = title,
      x = "Weight",
      y = "2x2 DID Estimate",
      color = "Comparison Type",
      shape = "Comparison Type"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

p_bacon_spm <- plot_bacon(bacon_result_spm, "Bacon Decomposition: PM")
p_bacon_so2 <- plot_bacon(bacon_result_so2, "Bacon Decomposition: SO2")
p_bacon_no2 <- plot_bacon(bacon_result_no2, "Bacon Decomposition: NO2")

p_bacon_combined <- p_bacon_spm / p_bacon_so2 / p_bacon_no2 +
  plot_annotation(
    title = "Goodman-Bacon Decomposition of TWFE Estimator",
    subtitle = "Each point is a 2x2 DID comparison. 'Later vs Earlier' are the forbidden comparisons."
  )

ggsave(file.path(fig_dir, "fig_bacon_decomposition.pdf"), p_bacon_combined, width = 9, height = 12)

# ==============================================================================
# 2. DISCUSSION OF RESULTS FOR THE PAPER
# ==============================================================================

cat("\n\n========== Summary for Paper Discussion ==========\n\n")
cat("Key findings from TWFE diagnostics:\n\n")
cat("1. HETEROGENEOUS TREATMENT EFFECTS ARE LIKELY:\n")
cat("   - Catalytic converter adopted in 1995 (4 metros) and 1998 (45 cities)\n")
cat("   - These cohorts differ systematically (city size, traffic, enforcement)\n")
cat("   - Treatment effect likely grows over time (gradual car stock turnover)\n\n")

cat("2. STAGGERED ADOPTION CREATES FORBIDDEN COMPARISONS:\n")
cat("   - After 1998, the 1995-cohort cities serve as implicit controls\n")
cat("   - Their growing treatment effect contaminates late-adopter estimates\n")
cat("   - Bacon decomposition quantifies the weight on these comparisons\n\n")

cat("3. EVENT-STUDY PRE-TREND TESTS ARE UNRELIABLE (Sun & Abraham 2021):\n")
cat("   - Standard event-study coefficients may show 'pre-trends' even under\n")
cat("     true parallel trends, due to contamination from heterogeneous effects\n")
cat("   - Conversely, true pre-trends could be masked by contamination\n")
cat("   - Greenstone & Hanna's visible pre-trends could be either genuine or artifacts\n\n")

cat("4. IMPLICATIONS:\n")
cat("   - The original TWFE results may underestimate or overestimate the true ATT\n")
cat("   - We cannot determine direction without heterogeneity-robust estimators\n")
cat("   - Next step: implement Callaway & Sant'Anna (2021) as alternative\n")

# ==============================================================================
# 3. SAVE DIAGNOSTICS
# ==============================================================================

save(bacon_result_spm, bacon_result_so2, bacon_result_no2,
     file = file.path(output_dir, "diagnostics_results.RData"))

cat("\n=== TWFE diagnostics complete ===\n")
