# ==============================================================================
# 06_comparison_visualization.R
# Compares TWFE results with heterogeneity-robust estimators
# Produces publication-quality figures and comparison tables for the paper
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(did)
library(patchwork)
library(modelsummary)

# --- Load all results ---------------------------------------------------------
project_root <- here::here()
if (!file.exists(file.path(project_root, "output", "twfe_results.RData"))) {
  project_root <- getwd()
}
load(file.path(project_root, "output", "twfe_results.RData"))
load(file.path(project_root, "output", "alternative_estimator_results.RData"))

output_dir <- file.path(project_root, "output")
fig_dir <- file.path(output_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. SIDE-BY-SIDE EVENT STUDY COMPARISON (TWFE vs Callaway & Sant'Anna)
# ==============================================================================

cat("\n========== Comparison: TWFE vs Callaway & Sant'Anna ==========\n\n")

# --- 1.1 Extract C&S event-study coefficients ---------------------------------
extract_cs_es <- function(cs_agg, pollutant_name) {
  if (is.null(cs_agg)) return(NULL)
  es <- cs_agg$event_study
  tibble(
    tau = es$egt,
    estimate = es$att.egt,
    se = es$se.egt,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    pollutant = pollutant_name,
    method = "Callaway & Sant'Anna (2021)"
  )
}

cs_es_all <- bind_rows(
  extract_cs_es(cs_agg_spm, "PM (SPM)"),
  extract_cs_es(cs_agg_so2, "SO2"),
  extract_cs_es(cs_agg_no2, "NO2")
)

# Add TWFE results (already extracted in script 03)
twfe_es_all <- es_results %>%
  mutate(method = "TWFE (Greenstone & Hanna)")

# Combine
comparison_es <- bind_rows(twfe_es_all, cs_es_all) %>%
  filter(tau >= -7, tau <= 9)

# --- 1.2 Comparison plot ------------------------------------------------------

p_comparison <- ggplot(comparison_es, aes(x = tau, y = estimate, color = method, fill = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray70") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~pollutant, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("TWFE (Greenstone & Hanna)" = "steelblue",
                                 "Callaway & Sant'Anna (2021)" = "darkgreen")) +
  scale_fill_manual(values = c("TWFE (Greenstone & Hanna)" = "steelblue",
                                "Callaway & Sant'Anna (2021)" = "darkgreen")) +
  labs(
    title = "Comparison: Standard TWFE vs. Heterogeneity-Robust Estimator",
    subtitle = "Effect of Catalytic Converter Policy on Air Pollution",
    x = "Years since policy adoption",
    y = "Estimated ATT (ug/m3)",
    color = "Method", fill = "Method"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(fig_dir, "fig_comparison_twfe_vs_cs.pdf"), p_comparison, width = 12, height = 5)
cat("Comparison plot saved.\n")

# ==============================================================================
# 2. COMPARISON TABLE: OVERALL ATT ESTIMATES
# ==============================================================================

cat("\n========== Overall ATT Comparison ==========\n\n")

# TWFE 5-year effects (from script 03)
compute_5yr_effect <- function(fit) {
  coefs <- coef(fit)
  vcov_mat <- vcov(fit)
  effect <- coefs["post_cat"] + 5 * coefs["post_trend_cat"]
  se <- sqrt(
    vcov_mat["post_cat", "post_cat"] +
    25 * vcov_mat["post_trend_cat", "post_trend_cat"] +
    10 * vcov_mat["post_cat", "post_trend_cat"]
  )
  n <- nobs(fit)
  pval <- 2 * pt(abs(effect/se), df = n - length(coefs), lower.tail = FALSE)
  c(estimate = unname(effect), se = unname(se), pvalue = unname(pval))
}

# Build comparison table
build_comparison_row <- function(pollutant, twfe_fit, cs_agg) {
  twfe <- compute_5yr_effect(twfe_fit)
  cs_simple <- if (!is.null(cs_agg)) {
    s <- cs_agg$simple
    c(estimate = s$overall.att, se = s$overall.se,
      pvalue = 2 * pnorm(abs(s$overall.att / s$overall.se), lower.tail = FALSE))
  } else {
    c(estimate = NA, se = NA, pvalue = NA)
  }

  tibble(
    Pollutant = pollutant,
    `TWFE 5yr Effect` = sprintf("%.2f (%.2f)", twfe["estimate"], twfe["se"]),
    `TWFE p-value` = sprintf("%.3f", twfe["pvalue"]),
    `C&S ATT` = sprintf("%.2f (%.2f)", cs_simple["estimate"], cs_simple["se"]),
    `C&S p-value` = sprintf("%.3f", cs_simple["pvalue"])
  )
}

comparison_table <- bind_rows(
  build_comparison_row("PM (SPM)", fit_2c_spm, cs_agg_spm),
  build_comparison_row("SO2", fit_2c_so2, cs_agg_so2),
  build_comparison_row("NO2", fit_2c_no2, cs_agg_no2)
)

cat("Comparison of Treatment Effect Estimates:\n\n")
print(knitr::kable(comparison_table, format = "simple"))

write.csv(comparison_table, file.path(output_dir, "table_comparison_methods.csv"), row.names = FALSE)

# ==============================================================================
# 3. PRE-TRENDS ANALYSIS
# ==============================================================================

cat("\n\n========== Pre-Trends Assessment ==========\n\n")

# C&S pre-trends test (built into the att_gt output)
test_pretrends <- function(cs_agg, pollutant_name) {
  if (is.null(cs_agg)) {
    cat(sprintf("  %s: estimation failed, cannot test pre-trends\n", pollutant_name))
    return(NULL)
  }
  es <- cs_agg$event_study
  pre_idx <- es$egt < 0
  pre_atts <- es$att.egt[pre_idx]
  pre_ses <- es$se.egt[pre_idx]
  pre_taus <- es$egt[pre_idx]

  # Joint test: are all pre-treatment ATTs jointly zero?
  # Simple Wald-type test
  if (length(pre_atts) > 0 && all(!is.na(pre_ses)) && all(pre_ses > 0)) {
    chi2_stat <- sum((pre_atts / pre_ses)^2)
    df <- length(pre_atts)
    p_joint <- pchisq(chi2_stat, df = df, lower.tail = FALSE)
    cat(sprintf("  %s: Pre-trend joint test chi2(%d) = %.2f, p = %.4f %s\n",
                pollutant_name, df, chi2_stat, p_joint,
                ifelse(p_joint < 0.05, "[REJECT: pre-trends detected]", "[PASS]")))

    # Individual coefficients
    cat(sprintf("    Individual pre-trend coefficients:\n"))
    for (i in seq_along(pre_taus)) {
      sig <- ifelse(abs(pre_atts[i] / pre_ses[i]) > 1.96, "*", " ")
      cat(sprintf("      tau=%2d: %7.2f (SE: %5.2f) %s\n", pre_taus[i], pre_atts[i], pre_ses[i], sig))
    }
  }
}

test_pretrends(cs_agg_spm, "PM")
test_pretrends(cs_agg_so2, "SO2")
test_pretrends(cs_agg_no2, "NO2")

# ==============================================================================
# 4. SUN & ABRAHAM COMPARISON (as additional robustness)
# ==============================================================================

cat("\n\n========== Sun & Abraham ATT Summary ==========\n\n")

# Extract aggregated ATT from Sun & Abraham 
sa_summary <- function(fit, pollutant_name) {
  agg <- summary(fit, agg = "ATT")
  coefs <- coeftable(agg)
  
  # L'ajout de drop = FALSE empêche R de transformer la ligne unique en vecteur
  att_row <- coefs[grepl("ATT", rownames(coefs)), , drop = FALSE]
  
  if (nrow(att_row) > 0) {
    cat(sprintf("  %s: ATT = %.3f (SE = %.3f, p = %.4f)\n",
                pollutant_name, att_row[1,1], att_row[1,2],
                2 * pnorm(abs(att_row[1,1]/att_row[1,2]), lower.tail = FALSE)))
  }
}

sa_summary(sa_fit_spm, "PM")
sa_summary(sa_fit_so2, "SO2")
sa_summary(sa_fit_no2, "NO2")

# ==============================================================================
# 5. FINAL SUMMARY FIGURE FOR THE PAPER
# ==============================================================================

# A 2-panel figure: (A) TWFE event study, (B) C&S event study, side by side

p_twfe_panel <- ggplot(
  twfe_es_all %>% filter(tau >= -7, tau <= 9),
  aes(x = tau, y = estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~pollutant, scales = "free_y", ncol = 3) +
  labs(title = "(A) Standard TWFE Event Study", x = "Event time", y = "Effect (ug/m3)") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

p_cs_panel <- ggplot(
  cs_es_all %>% filter(tau >= -7, tau <= 9),
  aes(x = tau, y = estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, fill = "darkgreen") +
  geom_line(color = "darkgreen", linewidth = 0.7) +
  geom_point(color = "darkgreen", size = 1.5) +
  facet_wrap(~pollutant, scales = "free_y", ncol = 3) +
  labs(title = "(B) Callaway & Sant'Anna (2021)", x = "Event time", y = "ATT (ug/m3)") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

p_final <- p_twfe_panel / p_cs_panel +
  plot_annotation(
    title = "Catalytic Converter Policy: TWFE vs. Heterogeneity-Robust Estimator",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(file.path(fig_dir, "fig_final_comparison.pdf"), p_final, width = 12, height = 8)

cat("\n=== Comparison and visualization complete ===\n")
cat(sprintf("All outputs in: %s\n", output_dir))
