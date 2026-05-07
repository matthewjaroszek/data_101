
# Z-test: Data Center Hotspot States vs National Average
# -----------------------------------------------------
# Hypothesis: Do residential electricity prices differ in data-center
# hotspot states (Virginia, Texas, California) compared with the
# national average?
#
# H0: The average residential electricity price in VA, TX, and CA is
#     equal to the national average (no systematic effect).
# H1: The average residential electricity price in VA, TX, and CA is
#     different from the national average.
#
# This script:
#   1. Loads a state-month electricity dataset.
#   2. Cleans to real states + DC, residential sector only.
#   3. Focuses on a modern AI / data-center period (default: 2016–2024).
#   4. Computes national average residential price and hotspot-state averages.
#   5. Performs a large-sample Z-test comparing hotspot mean vs national mean.
#   6. Produces several visualizations.
#
# EXPECTED INPUT
# --------------
# CSV file with at least the following columns:
#   - year (int)
#   - month (int, 1–12)
#   - stateDescription (state or region name)
#   - sectorName (e.g., "Residential")
#   - price (numeric, residential price in cents per kWh)
#   - sales (numeric, millions of kWh) — used for weighting
#
# You must set DATA_PATH below to your CSV file path.

# -----------------
# 0. Load libraries
# -----------------

library(tidyverse)
library(lubridate)
library(broom)

# -------------
# 1. Read data
# -------------

# Update this path to your actual CSV file location
DATA_PATH <- "og_data.csv"

raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)

cat("Columns in input data:
")
print(names(raw))

# -----------------------------
# 2. Basic cleaning and setup
# -----------------------------

# Keep only true US states + DC (drop aggregates like "U.S. Total", regions)
valid_states <- c(state.name, "District of Columbia")

# Residential sector, create date and year_month

elec_res <- raw %>%
  filter(
    sectorName == "residential",
    stateDescription %in% valid_states
  ) %>%
  mutate(
    date       = make_date(year, month, 1),
    year_month = floor_date(date, unit = "month")
  )

cat("Number of states after filtering:", length(unique(elec_res$stateDescription)), "
")

# --------------------------------------------
# 3. Focus period and hotspot state selection
# --------------------------------------------

# Define AI / data-center boom period
PERIOD_START <- as.Date("2016-01-01")
PERIOD_END   <- as.Date("2024-12-31")

hotspot_states <- c("Virginia", "Texas", "California")

panel <- elec_res %>%
  filter(date >= PERIOD_START, date <= PERIOD_END) %>%
  mutate(
    hotspot = stateDescription %in% hotspot_states
  )

cat("Observation counts by hotspot status:
")
print(panel %>% count(hotspot))

# ---------------------------------
# 4. Compute national and hotspot means
# ---------------------------------

# Monthly US sales-weighted average residential price
# Monthly hotspot (VA, TX, CA) sales-weighted average price
hotspot_monthly <- panel %>%
  filter(hotspot) %>%
  group_by(year_month) %>%
  summarise(
    hotspot_price = weighted.mean(price, sales, na.rm = TRUE),
    .groups = "drop"
  )

# Monthly rest-of-states sales-weighted average price
rest_monthly <- panel %>%
  filter(!hotspot) %>%
  group_by(year_month) %>%
  summarise(
    rest_price = weighted.mean(price, sales, na.rm = TRUE),
    .groups = "drop"
  )

# Merge the two series
comparison_monthly <- hotspot_monthly %>%
  left_join(rest_monthly, by = "year_month")

# Merge the two series
comparison_monthly <- hotspot_monthly %>%
  left_join(rest_monthly, by = "year_month")

cat("First few rows of monthly comparison:
")
print(head(comparison_monthly))

# Overall means across the period

# We'll use the monthly series as independent observations for the Z-test.

mu_hotspot <- mean(comparison_monthly$hotspot_price, na.rm = TRUE)
mu_rest    <- mean(comparison_monthly$rest_price, na.rm = TRUE)

sd_hotspot <- sd(comparison_monthly$hotspot_price, na.rm = TRUE)
sd_rest    <- sd(comparison_monthly$rest_price, na.rm = TRUE)

n_hotspot <- sum(!is.na(comparison_monthly$hotspot_price))
n_rest    <- sum(!is.na(comparison_monthly$rest_price))

# -----------------------------------
# 5. Large-sample Z-test for mean diff
# -----------------------------------

# Assuming the monthly averages are approximately independent
# and using the usual standard error for difference in means:
# standard error for difference in means
se_diff <- sqrt(sd_hotspot^2 / n_hotspot + sd_rest^2 / n_rest)

# test statistic for H1: hotspot > rest
z_stat <- (mu_hotspot - mu_rest) / se_diff

# one-sided p-value (right tail)
p_value <- pnorm(z_stat, lower.tail = FALSE)

cat("Hotspot mean price:", mu_hotspot, "cents/kWh\n")
cat("Rest-of-states mean price:", mu_rest, "cents/kWh\n")
cat("Z statistic (hotspot - rest):", z_stat, "\n")
cat("One-sided p-value:", p_value, "\n")

# Tidy results for easy printing or reporting

z_results <- tibble(
  metric         = c("mean_us", "mean_hotspot", "sd_us", "sd_hotspot", "n_us", "n_hotspot", "z_stat", "p_value"),
  value          = c(mu_us, mu_hotspot, sd_us, sd_hotspot, n_us, n_hotspot, z_stat, p_value)
)

print(z_results)

# ------------------------
# 6. Visualizations
# ------------------------

if (!dir.exists("output")) dir.create("output")

# (a) Time series: US vs hotspot monthly prices

plot_ts <- comparison_monthly %>%
  pivot_longer(cols = c(rest_price, hotspot_price),
               names_to = "series", values_to = "price") %>%
  mutate(
    series = recode(series,
                    rest_price = "US average",
                    hotspot_price = "VA+TX+CA (hotspots)")
  ) %>%
  ggplot(aes(x = year_month, y = price, color = series)) +
  geom_line(linewidth = 0.7) +
  labs(
    x = NULL,
    y = "Residential price (cents per kWh)",
    color = "Series",
    title = "Monthly residential electricity prices: hotspot states vs US average",
    subtitle = paste0("Period: ", PERIOD_START, " to ", PERIOD_END)
  ) +
  theme_minimal()

ggsave("output/ztest_ts_hotspots_vs_us.png", plot_ts, width = 8, height = 5, dpi = 300)

# (b) Distribution of monthly prices

plot_hist <- comparison_monthly %>%
  pivot_longer(cols = c(rest_price, hotspot_price),
               names_to = "series", values_to = "price") %>%
  mutate(
    series = recode(series,
                    rest_price = "US average",
                    hotspot_price = "VA+TX+CA (hotspots)")
  ) %>%
  ggplot(aes(x = price, fill = series)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(
    x = "Monthly residential price (cents per kWh)",
    y = "Count of months",
    fill = "Series",
    title = "Distribution of monthly residential prices",
    subtitle = "Comparing hotspot states vs national average"
  ) +
  theme_minimal()

ggsave("output/ztest_hist_hotspots_vs_us.png", plot_hist, width = 8, height = 5, dpi = 300)

# (c) Mean comparison with error bars (approximate 95% CI)

ci_us_lower <- mu_us - 1.96 * sd_us / sqrt(n_us)
ci_us_upper <- mu_us + 1.96 * sd_us / sqrt(n_us)

ci_hot_lower <- mu_hotspot - 1.96 * sd_hotspot / sqrt(n_hotspot)
ci_hot_upper <- mu_hotspot + 1.96 * sd_hotspot / sqrt(n_hotspot)

summary_means <- tibble(
  group = c("US average", "VA+TX+CA (hotspots)"),
  mean_price = c(mu_us, mu_hotspot),
  ci_lower   = c(ci_us_lower, ci_hot_lower),
  ci_upper   = c(ci_us_upper, ci_hot_upper)
)

plot_means <- ggplot(summary_means, aes(x = group, y = mean_price)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    x = "",
    y = "Average residential price (cents per kWh)",
    title = "Average residential electricity price: hotspots vs national",
    subtitle = "Bars show mean across months; error bars = approximate 95% CI"
  ) +
  theme_minimal()

ggsave("output/ztest_means_hotspots_vs_us.png", plot_means, width = 6, height = 4, dpi = 300)

# End of script
