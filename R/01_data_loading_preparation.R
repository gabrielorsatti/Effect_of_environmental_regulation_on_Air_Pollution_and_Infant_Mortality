# ==============================================================================
# 01_data_loading_preparation.R
# Load .dta datasets and construct all variables needed for analysis
# Replicates the Stata data prep logic from Table1_01102014.do & Table3_01102014.do
# ==============================================================================

library(haven)
library(dplyr)
library(tidyr)

# --- Paths (adjust if needed) ------------------------------------------------
project_root <- here::here()
if (!file.exists(file.path(project_root, "data", "combined.dta"))) {
  project_root <- getwd()
}
data_dir <- file.path(project_root, "data")
output_dir <- file.path(project_root, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. LOAD RAW DATA
# ==============================================================================

cat("Loading combined.dta (air pollution panel)...\n")
combined_raw <- read_dta(file.path(data_dir, "combined.dta"))
cat(sprintf("  Dimensions: %d rows x %d cols\n", nrow(combined_raw), ncol(combined_raw)))

cat("Loading im_air.dta (infant mortality panel)...\n")
im_air_raw <- read_dta(file.path(data_dir, "im_air.dta"))
cat(sprintf("  Dimensions: %d rows x %d cols\n", nrow(im_air_raw), ncol(im_air_raw)))

cat("\nVariables in combined.dta:\n")
cat(paste(" ", names(combined_raw), collapse = "\n"), "\n")
cat("\nVariables in im_air.dta:\n")
cat(paste(" ", names(im_air_raw), collapse = "\n"), "\n")

# ==============================================================================
# 2. AIR POLLUTION DATA PREPARATION (combined.dta)
# ==============================================================================

air <- combined_raw %>%
  mutate(
    city_id = as.integer(factor(city))
  )

# --- 2.1 Compute first treatment year for each policy per city ----------------

# Catalytic Converter: first year where catconverter == 1
cat_first <- air %>%
  filter(catconverter == 1) %>%
  group_by(city_id, city) %>%
  summarise(cat_first_year = min(year), .groups = "drop")

# SCAP: first year where actionplan_sc == 1
scap_first <- air %>%
  filter(actionplan_sc == 1) %>%
  group_by(city_id, city) %>%
  summarise(scap_first_year = min(year), .groups = "drop")

# Merge back
air <- air %>%
  left_join(cat_first, by = c("city_id", "city")) %>%
  left_join(scap_first, by = c("city_id", "city"))

# --- 2.2 Compute event time (tau) --------------------------------------------

# First year the city appears in the data
city_first_year <- air %>%
  group_by(city_id) %>%
  summarise(first_obs_year = min(year), .groups = "drop")

air <- air %>%
  left_join(city_first_year, by = "city_id") %>%
  mutate(
    # Event time for catalytic converter
    tau_cat = ifelse(
      !is.na(cat_first_year) & cat_first_year > first_obs_year,
      year - cat_first_year,
      NA_real_
    ),
    # Event time for SCAP
    tau_scap = ifelse(
      !is.na(scap_first_year) & scap_first_year > first_obs_year,
      year - scap_first_year,
      NA_real_
    ),
    # Never-adopters flags
    never_adopt_cat = is.na(cat_first_year),
    never_adopt_scap = is.na(scap_first_year)
  )

# --- 2.3 Handle cities that adopted but have no post-policy pollution data ----

# For catalytic converter: check if city has ANY non-missing pollution data after adoption
cat_post_check <- air %>%
  filter(!is.na(cat_first_year), year >= cat_first_year) %>%
  group_by(city_id) %>%
  summarise(
    has_post_spm = any(!is.na(e_spm_mean)),
    has_post_so2 = any(!is.na(e_so2_mean)),
    has_post_no2 = any(!is.na(e_no2_mean)),
    .groups = "drop"
  )

air <- air %>%
  left_join(cat_post_check, by = "city_id") %>%
  mutate(
    # If city adopted but has no post-policy data for ANY pollutant, treat as never-adopter
    never_adopt_cat = ifelse(
      !is.na(cat_first_year) & !has_post_spm & !has_post_so2 & !has_post_no2,
      TRUE, never_adopt_cat
    ),
    tau_cat = ifelse(never_adopt_cat & !is.na(cat_first_year), NA_real_, tau_cat)
  ) %>%
  select(-has_post_spm, -has_post_so2, -has_post_no2)

# --- 2.4 Sample inclusion criteria (per pollutant) ----------------------------
# Adopters: need at least 1 obs at tau <= -3 AND at least 1 obs at tau >= 3
# Non-adopters: need at least 2 observations of the given pollutant

build_analysis_sample <- function(data, pollutant_var, tau_var, never_adopt_var) {
  data %>%
    filter(!is.na(.data[[pollutant_var]])) %>%
    group_by(city_id) %>%
    mutate(
      n_obs = n(),
      has_pre3 = any(.data[[tau_var]] <= -3, na.rm = TRUE),
      has_post3 = any(.data[[tau_var]] >= 3, na.rm = TRUE),
      use_sample = case_when(
        .data[[never_adopt_var]] ~ n_obs > 1,
        TRUE ~ has_pre3 & has_post3 & n_obs > 1
      )
    ) %>%
    ungroup() %>%
    filter(use_sample) %>%
    select(-n_obs, -has_pre3, -has_post3, -use_sample)
}

# Build pollutant-specific analysis samples for catalytic converter
air_spm_cat <- build_analysis_sample(air, "e_spm_mean", "tau_cat", "never_adopt_cat")
air_so2_cat <- build_analysis_sample(air, "e_so2_mean", "tau_cat", "never_adopt_cat")
air_no2_cat <- build_analysis_sample(air, "e_no2_mean", "tau_cat", "never_adopt_cat")

cat(sprintf("\nAnalysis samples (Catalytic Converter):\n"))
cat(sprintf("  SPM: %d obs, %d cities\n", nrow(air_spm_cat), n_distinct(air_spm_cat$city_id)))
cat(sprintf("  SO2: %d obs, %d cities\n", nrow(air_so2_cat), n_distinct(air_so2_cat$city_id)))
cat(sprintf("  NO2: %d obs, %d cities\n", nrow(air_no2_cat), n_distinct(air_no2_cat$city_id)))

# --- 2.5 Construct variables for Callaway & Sant'Anna -------------------------
# The `did` package needs: gname = first treatment year (0 for never-treated)

air <- air %>%
  mutate(
    # For C&S: cohort variable (0 = never treated)
    cohort_cat = ifelse(never_adopt_cat, 0, cat_first_year),
    cohort_scap = ifelse(never_adopt_scap, 0, scap_first_year),
    # Post-treatment indicators (for standard TWFE)
    post_cat = ifelse(!never_adopt_cat & year >= cat_first_year, 1, 0),
    post_scap = ifelse(!never_adopt_scap & year >= scap_first_year, 1, 0)
  )

# --- 2.6 Construct variables for TWFE replication -----------------------------
# One-step approach needs: post*policy, trend, post*trend

air <- air %>%
  mutate(
    # Trend in event time (for catalytic converter cities in analysis window)
    cat_in_range = (!is.na(tau_cat) & tau_cat >= -7 & tau_cat <= 9) | never_adopt_cat,
    scap_in_range = (!is.na(tau_scap) & tau_scap >= -7 & tau_scap <= 3) | never_adopt_scap,
    # Post-treatment * in-range interaction
    cat_policy = as.integer(post_cat == 1 & cat_in_range),
    scap_policy = as.integer(post_scap == 1 & scap_in_range),
    # Event-time trends
    cat_tau_range = ifelse(cat_in_range & !never_adopt_cat, tau_cat, 0),
    scap_tau_range = ifelse(scap_in_range & !never_adopt_scap, tau_scap, 0),
    # Post * trend break (for Model 2C one-step)
    cat_trend_break = ifelse(cat_policy == 1, tau_cat, 0),
    scap_trend_break = ifelse(scap_policy == 1, tau_scap, 0)
  )

# ==============================================================================
# 3. INFANT MORTALITY DATA PREPARATION (im_air.dta)
# ==============================================================================

im <- im_air_raw %>%
  filter(!is.na(c_IM)) %>%
  mutate(city_id = as.integer(factor(city)))

# Compute first treatment year for catalytic converter
im_cat_first <- im %>%
  filter(catconverter == 1) %>%
  group_by(city_id) %>%
  summarise(cat_first_year = min(year), .groups = "drop")

city_first_year_im <- im %>%
  group_by(city_id) %>%
  summarise(first_obs_year = min(year), last_obs_year = max(year), .groups = "drop")

im <- im %>%
  left_join(im_cat_first, by = "city_id") %>%
  left_join(city_first_year_im, by = "city_id") %>%
  mutate(
    # Use catyear if available (pre-computed in the .dta), otherwise use computed
    cat_first_year = coalesce(catyear, cat_first_year),
    never_adopt_cat = is.na(cat_first_year) | last_obs_year < cat_first_year,
    tau_cat = ifelse(
      !never_adopt_cat & cat_first_year > first_obs_year,
      year - cat_first_year,
      NA_real_
    ),
    cohort_cat = ifelse(never_adopt_cat, 0, cat_first_year),
    post_cat = ifelse(!never_adopt_cat & year >= cat_first_year, 1, 0)
  )

# Sample filter: adopters need tau <= -3 and tau >= 3; non-adopters need 2+ obs
im <- im %>%
  group_by(city_id) %>%
  mutate(
    n_obs = n(),
    has_pre3 = any(tau_cat <= -3, na.rm = TRUE),
    has_post3 = any(tau_cat >= 3, na.rm = TRUE),
    use_sample = case_when(
      never_adopt_cat ~ n_obs > 1,
      TRUE ~ has_pre3 & has_post3 & n_obs > 1
    )
  ) %>%
  ungroup() %>%
  filter(use_sample) %>%
  select(-n_obs, -has_pre3, -has_post3, -use_sample)

cat(sprintf("\nInfant Mortality sample: %d obs, %d cities\n", nrow(im), n_distinct(im$city_id)))
cat(sprintf("  Adopters: %d cities\n", sum(!im$never_adopt_cat[!duplicated(im$city_id)])))
cat(sprintf("  Never-adopters: %d cities\n", sum(im$never_adopt_cat[!duplicated(im$city_id)])))

# ==============================================================================
# 4. SAVE PREPARED DATASETS
# ==============================================================================

save(air, air_spm_cat, air_so2_cat, air_no2_cat,
     file = file.path(output_dir, "prepared_air.RData"))
save(im, file = file.path(output_dir, "prepared_im.RData"))

cat("\n=== Data preparation complete ===\n")
cat(sprintf("Saved: %s\n", file.path(output_dir, "prepared_air.RData")))
cat(sprintf("Saved: %s\n", file.path(output_dir, "prepared_im.RData")))
