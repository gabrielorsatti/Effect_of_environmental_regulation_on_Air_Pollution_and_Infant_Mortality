# ==============================================================================
# 02_descriptive_statistics.R
# Produces summary statistics tables (replicating Table 2 of Greenstone & Hanna)
# and descriptive visualizations of the panel data
# ==============================================================================

#install.packages("ggplot2")
#install.packages("patchwork")

library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
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
# 1. SUMMARY STATISTICS TABLE (Table 2 equivalent)
# ==============================================================================

# --- 1.1 Air Pollution Statistics ---------------------------------------------
compute_stats <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    Mean = mean(x),
    SD = sd(x),
    N = length(x),
    P10 = quantile(x, 0.10),
    P90 = quantile(x, 0.90)
  )
}

air_stats_full <- bind_rows(
  compute_stats(air$e_spm_mean) %>% mutate(Variable = "PM (ug/m3)", Period = "Full"),
  compute_stats(air$e_so2_mean) %>% mutate(Variable = "SO2 (ug/m3)", Period = "Full"),
  compute_stats(air$e_no2_mean) %>% mutate(Variable = "NO2 (ug/m3)", Period = "Full"),
  compute_stats(im$c_IM) %>% mutate(Variable = "IM (deaths/1000)", Period = "Full")
)

air_stats_early <- bind_rows(
  compute_stats(air$e_spm_mean[air$year <= 1990]) %>% mutate(Variable = "PM (ug/m3)", Period = "1987-1990"),
  compute_stats(air$e_so2_mean[air$year <= 1990]) %>% mutate(Variable = "SO2 (ug/m3)", Period = "1987-1990"),
  compute_stats(air$e_no2_mean[air$year <= 1990]) %>% mutate(Variable = "NO2 (ug/m3)", Period = "1987-1990"),
  compute_stats(im$c_IM[im$year <= 1990]) %>% mutate(Variable = "IM (deaths/1000)", Period = "1987-1990")
)

air_stats_late <- bind_rows(
  compute_stats(air$e_spm_mean[air$year >= 2004]) %>% mutate(Variable = "PM (ug/m3)", Period = "2004-2007"),
  compute_stats(air$e_so2_mean[air$year >= 2004]) %>% mutate(Variable = "SO2 (ug/m3)", Period = "2004-2007"),
  compute_stats(air$e_no2_mean[air$year >= 2004]) %>% mutate(Variable = "NO2 (ug/m3)", Period = "2004-2007"),
  compute_stats(im$c_IM[im$year >= 2001]) %>% mutate(Variable = "IM (deaths/1000)", Period = "2001-2004")
)

summary_table <- bind_rows(air_stats_full, air_stats_early, air_stats_late) %>%
  select(Variable, Period, Mean, SD, N, P10, P90) %>%
  mutate(across(c(Mean, SD, P10, P90), ~ round(.x, 1)))

cat("\n========== TABLE 2: Summary Statistics ==========\n\n")
print(kable(summary_table, format = "simple", align = "lcrrrr"))

write.csv(summary_table, file.path(output_dir, "table2_summary_stats.csv"), row.names = FALSE)

# ==============================================================================
# 2. POLICY PREVALENCE TABLE (Table 1 equivalent)
# ==============================================================================

prevalence <- air %>%
  group_by(year) %>%
  summarise(
    N_cities = n_distinct(city_id),
    N_SCAP = n_distinct(city_id[actionplan_sc == 1]),
    N_CatConv = n_distinct(city_id[catconverter == 1]),
    .groups = "drop"
  ) %>%
  arrange(year)

cat("\n========== TABLE 1: Policy Prevalence by Year ==========\n\n")
print(kable(prevalence, format = "simple"))

write.csv(prevalence, file.path(output_dir, "table1_policy_prevalence.csv"), row.names = FALSE)

# ==============================================================================
# 3. TREATMENT COHORT DESCRIPTION
# ==============================================================================

cat("\n========== Treatment Cohorts ==========\n\n")

cat("--- Catalytic Converter ---\n")
air %>%
  filter(!never_adopt_cat) %>%
  distinct(city_id, cat_first_year, city) %>%
  count(cat_first_year, name = "n_cities") %>%
  arrange(cat_first_year) %>%
  print()

cat("\n--- Supreme Court Action Plan ---\n")
air %>%
  filter(!never_adopt_scap) %>%
  distinct(city_id, scap_first_year, city) %>%
  count(scap_first_year, name = "n_cities") %>%
  arrange(scap_first_year) %>%
  print()

# ==============================================================================
# 4. VISUALIZATIONS
# ==============================================================================

# --- 4.1 National trends in air pollution (Figure 4A equivalent) --------------
trends_air <- air %>%
  group_by(year) %>%
  summarise(
    PM = mean(e_spm_mean, na.rm = TRUE),
    SO2 = mean(e_so2_mean, na.rm = TRUE),
    NO2 = mean(e_no2_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(PM, SO2, NO2), names_to = "Pollutant", values_to = "Concentration")

p_trends <- ggplot(trends_air, aes(x = year, y = Concentration)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~Pollutant, scales = "free_y", ncol = 3) +
  labs(
    title = "National Trends in Air Pollution (Annual City Means)",
    x = "Year", y = "Mean concentration (ug/m3)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "fig_trends_air_pollution.pdf"), p_trends, width = 10, height = 4)

# --- 4.2 Infant mortality trend (Figure 4C equivalent) ------------------------
trends_im <- im %>%
  group_by(year) %>%
  summarise(IM = mean(c_IM, na.rm = TRUE), .groups = "drop")

p_im <- ggplot(trends_im, aes(x = year, y = IM)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Infant Mortality Rate Over Time",
    x = "Year", y = "Deaths per 1,000 births"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(fig_dir, "fig_trends_infant_mortality.pdf"), p_im, width = 6, height = 4)

# --- 4.3 Treatment timing visualization --------------------------------------
treatment_timing <- air %>%
  filter(!never_adopt_cat) %>%
  distinct(city, cat_first_year) %>%
  arrange(cat_first_year, city) %>%
  mutate(city_label = factor(city, levels = city))

p_timing <- ggplot(treatment_timing, aes(x = cat_first_year, y = city_label)) +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Catalytic Converter Policy: Staggered Adoption Timing",
    x = "Year of Policy Adoption",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_text(size = 6))

ggsave(file.path(fig_dir, "fig_treatment_timing_catconv.pdf"), p_timing, width = 8, height = 10)

cat("\n=== Descriptive statistics complete ===\n")
cat(sprintf("Tables saved to: %s\n", output_dir))
cat(sprintf("Figures saved to: %s\n", fig_dir))
