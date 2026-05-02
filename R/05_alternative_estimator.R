# ==============================================================================
# 05_alternative_estimator.R
# Implements heterogeneity-robust DiD estimators as alternatives to TWFE
# Primary: Callaway & Sant'Anna (2021) via the `did` package
# Secondary: Sun & Abraham (2021) via fixest::sunab()
# ==============================================================================

library(dplyr)
library(did)
library(fixest)
library(ggplot2)
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
# 1. CALLAWAY & SANT'ANNA (2021) - CATALYTIC CONVERTER ON AIR POLLUTION
# ==============================================================================

cat("\n========== Callaway & Sant'Anna (2021) Estimator ==========\n\n")

# --- 1.1 Prepare data for the `did` package -----------------------------------
# Requirements:
#   - gname: first treatment period (0 for never-treated)
#   - tname: time variable
#   - idname: panel unit id (numeric)
#   - yname: outcome variable

prep_cs_data <- function(data, pollutant_var) {
  data %>%
    filter(!is.na(.data[[pollutant_var]])) %>%
    mutate(
      # Cohort variable: first year of catalytic converter (0 = never-treated)
      G = ifelse(never_adopt_cat, 0, cat_first_year)
    ) %>%
    # Ensure city_id is numeric integer
    mutate(city_id = as.integer(factor(city_id))) %>%
    select(city_id, year, G, outcome = all_of(pollutant_var), pop_urban)
}

cs_spm <- prep_cs_data(air, "e_spm_mean")
cs_so2 <- prep_cs_data(air, "e_so2_mean")
cs_no2 <- prep_cs_data(air, "e_no2_mean")

cat("Cohort distribution (Catalytic Converter):\n")
cs_spm %>% distinct(city_id, G) %>% count(G) %>% print()

# --- 1.2 Estimate group-time ATTs ---------------------------------------------

estimate_cs <- function(data, pollutant_name, control_group = "nevertreated") {
  cat(sprintf("\n--- Callaway & Sant'Anna: %s (control: %s) ---\n", pollutant_name, control_group))

  tryCatch({
    att_result <- att_gt(
      yname = "outcome",
      tname = "year",
      idname = "city_id",
      gname = "G",
      data = as.data.frame(data),
      control_group = control_group,
      anticipation = 0,
      base_period = "varying",
      weightsname = "pop_urban",
      print_details = FALSE
    )

    cat(sprintf("  Number of group-time ATT(g,t) estimated: %d\n", length(att_result$att)))
    cat(sprintf("  Cohorts: %s\n", paste(unique(att_result$group), collapse = ", ")))

    return(att_result)
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    return(NULL)
  })
}

# Estimate with never-treated as controls (preferred)
cs_fit_spm <- estimate_cs(cs_spm, "PM (SPM)", "nevertreated")
cs_fit_so2 <- estimate_cs(cs_so2, "SO2", "nevertreated")
cs_fit_no2 <- estimate_cs(cs_no2, "NO2", "nevertreated")

# Also try not-yet-treated for robustness
cs_fit_spm_nyt <- estimate_cs(cs_spm, "PM (SPM)", "notyettreated")
cs_fit_so2_nyt <- estimate_cs(cs_so2, "SO2", "notyettreated")
cs_fit_no2_nyt <- estimate_cs(cs_no2, "NO2", "notyettreated")

# --- 1.3 Aggregate into event-study (dynamic effects) -------------------------

aggregate_cs <- function(att_result, pollutant_name) {
  if (is.null(att_result)) return(NULL)
  
  es <- aggte(att_result, type = "dynamic", min_e = -7, max_e = 9, na.rm = TRUE)
  cat(sprintf("\n  %s - Event-study aggregation:\n", pollutant_name))
  cat(sprintf("    Overall ATT: %.3f (SE: %.3f)\n", es$overall.att, es$overall.se))
  
  simple <- aggte(att_result, type = "simple", na.rm = TRUE)
  cat(sprintf("    Simple ATT:  %.3f (SE: %.3f, p: %.3f)\n",
              simple$overall.att, simple$overall.se,
              2 * pnorm(abs(simple$overall.att / simple$overall.se), lower.tail = FALSE)))
  
  return(list(event_study = es, simple = simple, att_gt = att_result))
}

cs_agg_spm <- aggregate_cs(cs_fit_spm, "PM")
if (is.null(cs_agg_spm)) {
  cat("  PM never-treated failed, trying not-yet-treated...\n")
  cs_agg_spm <- aggregate_cs(cs_fit_spm_nyt, "PM")
}
cs_agg_so2 <- aggregate_cs(cs_fit_so2, "SO2")
if (is.null(cs_agg_so2)) {
  cat("  SO2 never-treated failed, trying not-yet-treated...\n")
  cs_agg_so2 <- aggregate_cs(cs_fit_so2_nyt, "SO2")
}
cs_agg_no2 <- aggregate_cs(cs_fit_no2, "NO2")

# --- 1.4 Event-study plots (C&S version) -------------------------------------

plot_cs_es <- function(agg_result, pollutant_name, color = "darkgreen", ylab = "ATT (ug/m3)") {
  if (is.null(agg_result)) {
    return(ggplot() + labs(title = paste(pollutant_name, "- estimation failed")))
  }
  es <- agg_result$event_study
  df <- tibble(
    tau = es$egt,
    estimate = es$att.egt,
    se = es$se.egt,
    ci_low = es$att.egt - 1.96 * es$se.egt,
    ci_high = es$att.egt + 1.96 * es$se.egt
  )

  ggplot(df, aes(x = tau, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, fill = color) +
    geom_line(color = color, linewidth = 0.7) +
    geom_point(color = color, size = 2) +
    labs(
      title = pollutant_name,
      x = "Event time (years since adoption)",
      y = ylab
    ) +
    theme_minimal(base_size = 10)
}

p_cs_spm <- plot_cs_es(cs_agg_spm, "PM (SPM)")
p_cs_so2 <- plot_cs_es(cs_agg_so2, "SO2")
p_cs_no2 <- plot_cs_es(cs_agg_no2, "NO2")

p_cs_combined <- (p_cs_spm | p_cs_so2 | p_cs_no2) +
  plot_annotation(
    title = "Callaway & Sant'Anna (2021): Catalytic Converter on Air Pollution",
    subtitle = "Heterogeneity-robust event study. Control group: never-treated cities."
  )

ggsave(file.path(fig_dir, "fig_callaway_santanna_catconv.pdf"), p_cs_combined, width = 12, height = 4.5)

# ==============================================================================
# 2. SUN & ABRAHAM (2021) VIA fixest::sunab()
# ==============================================================================

cat("\n\n========== Sun & Abraham (2021) via fixest::sunab() ==========\n\n")

# --- 2.1 Prepare data for Sun & Abraham --------------------------------------
# sunab() needs: a cohort variable (first treatment year, Inf for never-treated)

prep_sunab <- function(data, pollutant_var) {
  data %>%
    filter(!is.na(.data[[pollutant_var]])) %>%
    mutate(
      cohort_sunab = ifelse(never_adopt_cat, Inf, cat_first_year)
    )
}

sa_spm <- prep_sunab(air, "e_spm_mean")
sa_so2 <- prep_sunab(air, "e_so2_mean")
sa_no2 <- prep_sunab(air, "e_no2_mean")

# --- 2.2 Estimate Sun & Abraham event-study -----------------------------------

sa_fit_spm <- feols(
  e_spm_mean ~ sunab(cohort_sunab, year) | city_id + year,
  data = sa_spm,
  weights = ~pop_urban,
  cluster = ~city_id
)

sa_fit_so2 <- feols(
  e_so2_mean ~ sunab(cohort_sunab, year) | city_id + year,
  data = sa_so2,
  weights = ~pop_urban,
  cluster = ~city_id
)

sa_fit_no2 <- feols(
  e_no2_mean ~ sunab(cohort_sunab, year) | city_id + year,
  data = sa_no2,
  weights = ~pop_urban,
  cluster = ~city_id
)

cat("Sun & Abraham results:\n")
cat("\n--- PM ---\n")
summary(sa_fit_spm, agg = "ATT")
cat("\n--- SO2 ---\n")
summary(sa_fit_so2, agg = "ATT")
cat("\n--- NO2 ---\n")
summary(sa_fit_no2, agg = "ATT")

# --- 2.3 Sun & Abraham event-study plots --------------------------------------

p_sa_spm <- iplot(sa_fit_spm, main = "PM - Sun & Abraham (2021)")
p_sa_so2 <- iplot(sa_fit_so2, main = "SO2 - Sun & Abraham (2021)")
p_sa_no2 <- iplot(sa_fit_no2, main = "NO2 - Sun & Abraham (2021)")

# ==============================================================================
# 3. CALLAWAY & SANT'ANNA - INFANT MORTALITY
# ==============================================================================

cat("\n\n========== C&S: Catalytic Converter on Infant Mortality ==========\n\n")

cs_im <- im %>%
  mutate(
    G = ifelse(never_adopt_cat, 0, cat_first_year),
    city_id = as.integer(factor(city_id))
  ) %>%
  filter(!is.na(c_IM)) %>%
  select(city_id, year, G, outcome = c_IM, c_birth)

cat("IM cohort distribution:\n")
cs_im %>% distinct(city_id, G) %>% count(G) %>% print()

cs_fit_im <- tryCatch({
  att_gt(
    yname = "outcome",
    tname = "year",
    idname = "city_id",
    gname = "G",
    data = as.data.frame(cs_im),
    control_group = "nevertreated",
    anticipation = 0,
    base_period = "varying",
    weightsname = "c_birth",
    print_details = FALSE
  )
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", e$message))
  # Try not-yet-treated
  cat("  Trying not-yet-treated controls...\n")
  tryCatch(
    att_gt(
      yname = "outcome",
      tname = "year",
      idname = "city_id",
      gname = "G",
      data = as.data.frame(cs_im),
      control_group = "notyettreated",
      anticipation = 0,
      base_period = "varying",
      print_details = FALSE
    ),
    error = function(e2) { cat(sprintf("  Also failed: %s\n", e2$message)); NULL }
  )
})

if (!is.null(cs_fit_im)) {
  cs_agg_im <- aggregate_cs(cs_fit_im, "Infant Mortality")

  p_cs_im <- plot_cs_es(cs_agg_im, "Infant Mortality (Callaway & Sant'Anna)", color = "darkred", ylab = "ATT (deaths/1000)")
  ggsave(file.path(fig_dir, "fig_callaway_santanna_IM.pdf"), p_cs_im, width = 7, height = 5)
}

# ==============================================================================
# 4. SAVE ALL ALTERNATIVE ESTIMATOR RESULTS
# ==============================================================================

save(cs_fit_spm, cs_fit_so2, cs_fit_no2,
     cs_fit_spm_nyt, cs_fit_so2_nyt, cs_fit_no2_nyt,
     cs_agg_spm, cs_agg_so2, cs_agg_no2,
     sa_fit_spm, sa_fit_so2, sa_fit_no2,
     sa_spm, sa_so2, sa_no2,
     cs_fit_im, cs_agg_im,
     file = file.path(output_dir, "alternative_estimator_results.RData"))

cat("\n=== Alternative estimator analysis complete ===\n")

save(
  cs_agg_spm, cs_agg_so2, cs_agg_no2, cs_agg_im,
  file = file.path(output_dir, "cs_aggregated_results.RData")
)

print(cs_agg_spm$simple)
print(cs_agg_so2$simple)
print(cs_agg_no2$simple)
print(cs_agg_im$simple)

print(cs_agg_spm$event_study)
print(cs_agg_so2$event_study)
print(cs_agg_no2$event_study)
print(cs_agg_im$event_study)
