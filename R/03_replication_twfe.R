# ==============================================================================
# 03_replication_twfe.R
# Replicates the original TWFE approach of Greenstone & Hanna (2014)
# - Event-study regressions (Equation 1)
# - One-step trend-break regressions (Equation 2C one-step version)
# Produces event-study plots (Figure 5 equivalent) and regression tables (Table 3)
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)
library(modelsummary)
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
# 1. EVENT-STUDY REGRESSIONS (Equation 1) - Catalytic Converter on Air Pollution
# ==============================================================================

# We use fixest::feols with i(tau, ref = -1) for the event-study specification
# This directly estimates sigma_tau from Equation 1

# --- 1.1 Prepare event-study data for catalytic converter ---------------------

prepare_es_data <- function(data, pollutant_var) {
  data %>%
    filter(!is.na(.data[[pollutant_var]])) %>%
    group_by(city_id) %>%
    mutate(
      n_obs = n(),
      has_pre3_cat = any(tau_cat <= -3, na.rm = TRUE),
      has_post3_cat = any(tau_cat >= 3, na.rm = TRUE),
      use_cat = ifelse(never_adopt_cat, n_obs > 1, has_pre3_cat & has_post3_cat & n_obs > 1)
    ) %>%
    ungroup() %>%
    filter(use_cat) %>%
    mutate(
      # For event study: never-adopters get tau = NA (excluded from event-time dummies)
      # Bin extreme values
      tau_cat_binned = case_when(
        never_adopt_cat ~ NA_real_,
        tau_cat < -7 ~ -7,
        tau_cat > 9 ~ 9,
        TRUE ~ tau_cat
      )
    ) %>%
    select(-n_obs, -has_pre3_cat, -has_post3_cat, -use_cat)
}

es_spm <- prepare_es_data(air, "e_spm_mean")
es_so2 <- prepare_es_data(air, "e_so2_mean")
es_no2 <- prepare_es_data(air, "e_no2_mean")

# --- 1.2 Estimate event-study regressions -------------------------------------

# Catalytic converter event study for each pollutant
# Including SCAP event-time dummies as controls (as in the original paper)
# Weighted by urban population

cat("\n========== Event-Study: Catalytic Converter on Air Pollution ==========\n\n")

es_fit_spm <- feols(
  e_spm_mean ~ i(tau_cat_binned, ref = -1) | city_id + year,
  data = es_spm,
  weights = ~pop_urban,
  cluster = ~city_id
)

es_fit_so2 <- feols(
  e_so2_mean ~ i(tau_cat_binned, ref = -1) | city_id + year,
  data = es_so2,
  weights = ~pop_urban,
  cluster = ~city_id
)

es_fit_no2 <- feols(
  e_no2_mean ~ i(tau_cat_binned, ref = -1) | city_id + year,
  data = es_no2,
  weights = ~pop_urban,
  cluster = ~city_id
)

# --- 1.3 Event-study plots (Figure 5 equivalent) -----------------------------

p_es_spm <- iplot(es_fit_spm, main = "PM (SPM)", xlab = "Years since catalytic converters mandated",
                   ylab = "Effect on PM (ug/m3)")

p_es_so2 <- iplot(es_fit_so2, main = "SO2", xlab = "Years since catalytic converters mandated",
                   ylab = "Effect on SO2 (ug/m3)")

p_es_no2 <- iplot(es_fit_no2, main = "NO2", xlab = "Years since catalytic converters mandated",
                   ylab = "Effect on NO2 (ug/m3)")

# Combined plot using ggplot from fixest coefficients
extract_es_coefs <- function(fit, pollutant_name) {
  coefs <- coeftable(fit)
  coef_names <- rownames(coefs)
  tau_rows <- grepl("tau_cat_binned", coef_names)
  tibble(
    tau = as.numeric(gsub("tau_cat_binned::", "", coef_names[tau_rows])),
    estimate = coefs[tau_rows, "Estimate"],
    se = coefs[tau_rows, "Std. Error"],
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    pollutant = pollutant_name
  )
}

es_results <- bind_rows(
  extract_es_coefs(es_fit_spm, "PM (SPM)"),
  extract_es_coefs(es_fit_so2, "SO2"),
  extract_es_coefs(es_fit_no2, "NO2")
) %>%
  # Add the reference point (tau = -1, effect = 0)
  bind_rows(
    tibble(tau = -1, estimate = 0, se = 0, ci_low = 0, ci_high = 0,
           pollutant = c("PM (SPM)", "SO2", "NO2"))
  )

p_event_study <- ggplot(es_results, aes(x = tau, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~pollutant, scales = "free_y", ncol = 3) +
  labs(
    title = "Event Study: Effect of Catalytic Converter Policy on Air Pollution",
    subtitle = "TWFE with city and year FE, weighted by urban population. Reference: tau = -1.",
    x = "Years since policy adoption (tau)",
    y = "Estimated effect (ug/m3)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "fig_event_study_catconv_twfe.pdf"), p_event_study, width = 11, height = 4)
cat("Event-study plot saved.\n")

# ==============================================================================
# 2. ONE-STEP TREND-BREAK REGRESSIONS (Equation 2C - One-Step Version)
# ==============================================================================

cat("\n========== One-Step Trend-Break Regressions (Table 3 equivalent) ==========\n\n")

# Prepare data with trend variables
prep_onestep <- function(data, pollutant_var) {
  data %>%
    filter(!is.na(.data[[pollutant_var]])) %>%
    group_by(city_id) %>%
    mutate(
      n_obs = n(),
      has_pre3 = any(tau_cat <= -3, na.rm = TRUE),
      has_post3 = any(tau_cat >= 3, na.rm = TRUE),
      use = ifelse(never_adopt_cat, n_obs > 1, has_pre3 & has_post3 & n_obs > 1)
    ) %>%
    ungroup() %>%
    filter(use) %>%
    mutate(
      # Post-treatment indicator
      post_cat = ifelse(!never_adopt_cat & !is.na(tau_cat) & tau_cat >= 0, 1, 0),
      # Linear trend in event time (0 for never-adopters)
      trend_cat = ifelse(!never_adopt_cat & !is.na(tau_cat), tau_cat, 0),
      # Post * trend interaction (trend break)
      post_trend_cat = post_cat * ifelse(!is.na(tau_cat), tau_cat, 0)
    ) %>%
    select(-n_obs, -has_pre3, -has_post3, -use)
}

spm_os <- prep_onestep(air, "e_spm_mean")
so2_os <- prep_onestep(air, "e_so2_mean")
no2_os <- prep_onestep(air, "e_no2_mean")

# --- Model 2A (Mean Shift) ---
fit_2a_spm <- feols(e_spm_mean ~ post_cat | city_id + year, data = spm_os, weights = ~pop_urban, cluster = ~city_id)
fit_2a_so2 <- feols(e_so2_mean ~ post_cat | city_id + year, data = so2_os, weights = ~pop_urban, cluster = ~city_id)
fit_2a_no2 <- feols(e_no2_mean ~ post_cat | city_id + year, data = no2_os, weights = ~pop_urban, cluster = ~city_id)

# --- Model 2B (Mean Shift + Trend) ---
fit_2b_spm <- feols(e_spm_mean ~ post_cat + trend_cat | city_id + year, data = spm_os, weights = ~pop_urban, cluster = ~city_id)
fit_2b_so2 <- feols(e_so2_mean ~ post_cat + trend_cat | city_id + year, data = so2_os, weights = ~pop_urban, cluster = ~city_id)
fit_2b_no2 <- feols(e_no2_mean ~ post_cat + trend_cat | city_id + year, data = no2_os, weights = ~pop_urban, cluster = ~city_id)

# --- Model 2C (Trend Break - PREFERRED) ---
fit_2c_spm <- feols(e_spm_mean ~ post_cat + trend_cat + post_trend_cat | city_id + year, data = spm_os, weights = ~pop_urban, cluster = ~city_id)
fit_2c_so2 <- feols(e_so2_mean ~ post_cat + trend_cat + post_trend_cat | city_id + year, data = so2_os, weights = ~pop_urban, cluster = ~city_id)
fit_2c_no2 <- feols(e_no2_mean ~ post_cat + trend_cat + post_trend_cat | city_id + year, data = no2_os, weights = ~pop_urban, cluster = ~city_id)

# --- Compute 5-year effects for Model 2C  ---
compute_5yr_effect <- function(fit, post_var = "post_cat", trend_var = "post_trend_cat") {
  coefs <- coef(fit)
  vcov_mat <- vcov(fit)
  
  # 5-year effect = post + 5 * post_trend
  effect <- coefs[post_var] + 5 * coefs[trend_var]
  
  # SE via delta method: Var(a + 5b) = Var(a) + 25*Var(b) + 10*Cov(a,b)
  se <- sqrt(
    vcov_mat[post_var, post_var] +
      25 * vcov_mat[trend_var, trend_var] +
      10 * vcov_mat[post_var, trend_var]
  )
  
  tstat <- effect / se
  n <- nobs(fit)
  pval <- 2 * pt(abs(tstat), df = n - length(coefs), lower.tail = FALSE)
  c(effect = unname(effect), se = unname(se), t = unname(tstat), p = unname(pval))
}

cat("5-Year Effects (Model 2C, Catalytic Converter):\n")
cat(sprintf("  PM:  %.2f (p = %.3f)\n", compute_5yr_effect(fit_2c_spm)["effect"], compute_5yr_effect(fit_2c_spm)["p"]))
cat(sprintf("  SO2: %.2f (p = %.3f)\n", compute_5yr_effect(fit_2c_so2)["effect"], compute_5yr_effect(fit_2c_so2)["p"]))
cat(sprintf("  NO2: %.2f (p = %.3f)\n", compute_5yr_effect(fit_2c_no2)["effect"], compute_5yr_effect(fit_2c_no2)["p"]))

# --- Regression table ---------------------------------------------------------
models_spm <- list("2A" = fit_2a_spm, "2B" = fit_2b_spm, "2C" = fit_2c_spm)
models_so2 <- list("2A" = fit_2a_so2, "2B" = fit_2b_so2, "2C" = fit_2c_so2)
models_no2 <- list("2A" = fit_2a_no2, "2B" = fit_2b_no2, "2C" = fit_2c_no2)

cat("\n--- Panel A: PM ---\n")
msummary(models_spm, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = "AIC|BIC|Log|R2 W")
cat("\n--- Panel B: SO2 ---\n")
msummary(models_so2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = "AIC|BIC|Log|R2 W")
cat("\n--- Panel C: NO2 ---\n")
msummary(models_no2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = "AIC|BIC|Log|R2 W")

# ==============================================================================
# 3. INFANT MORTALITY REGRESSION (Table 6 equivalent)
# ==============================================================================

cat("\n========== Catalytic Converter on Infant Mortality (Table 6) ==========\n\n")

im_es <- im %>%
  mutate(
    post_cat_im = ifelse(!never_adopt_cat & !is.na(tau_cat) & tau_cat >= 0, 1, 0),
    trend_cat_im = ifelse(!never_adopt_cat & !is.na(tau_cat), tau_cat, 0),
    post_trend_cat_im = post_cat_im * ifelse(!is.na(tau_cat), tau_cat, 0),
    tau_cat_binned = case_when(
      never_adopt_cat ~ NA_real_,
      tau_cat < -10 ~ -10,
      tau_cat > 5 ~ 5,
      TRUE ~ tau_cat
    )
  )

# Event study
fit_im_es <- feols(
  c_IM ~ i(tau_cat_binned, ref = -1) | city_id + year,
  data = im_es,
  weights = ~c_birth,
  cluster = ~city_id
)

# Trend-break (Model 2C)
fit_im_2c <- feols(
  c_IM ~ post_cat_im + trend_cat_im + post_trend_cat_im | city_id + year,
  data = im_es,
  weights = ~c_birth,
  cluster = ~city_id
)

cat("Infant Mortality - Model 2C:\n")
summary(fit_im_2c)
cat(sprintf("\n5-Year Effect: %.3f (p = %.3f)\n",
            compute_5yr_effect(fit_im_2c, post_var = "post_cat_im", trend_var = "post_trend_cat_im")["effect"],
            compute_5yr_effect(fit_im_2c, post_var = "post_cat_im", trend_var = "post_trend_cat_im")["p"]))

# Event study plot for IM
im_coefs <- extract_es_coefs(fit_im_es, "Infant Mortality")
im_coefs <- bind_rows(im_coefs, tibble(tau = -1, estimate = 0, se = 0, ci_low = 0, ci_high = 0, pollutant = "Infant Mortality"))

p_im_es <- ggplot(im_coefs, aes(x = tau, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "darkred") +
  geom_line(color = "darkred", linewidth = 0.7) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Event Study: Catalytic Converter Policy and Infant Mortality",
    subtitle = "TWFE with city and year FE, weighted by births. Reference: tau = -1.",
    x = "Years since policy adoption", y = "Effect on IM rate (deaths/1000)"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(fig_dir, "fig_event_study_catconv_IM.pdf"), p_im_es, width = 7, height = 5)

# ==============================================================================
# 4. SAVE ALL RESULTS
# ==============================================================================

save(es_fit_spm, es_fit_so2, es_fit_no2,
     fit_2a_spm, fit_2a_so2, fit_2a_no2,
     fit_2b_spm, fit_2b_so2, fit_2b_no2,
     fit_2c_spm, fit_2c_so2, fit_2c_no2,
     fit_im_es, fit_im_2c,
     es_results, im_coefs,
     file = file.path(output_dir, "twfe_results.RData"))

cat("\n=== TWFE replication complete ===\n")
