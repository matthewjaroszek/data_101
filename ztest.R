
# Z-test: Data Center Hotspot States vs National Average
#
# H0: The average residential electricity price in VA, TX, and CA is
#     equal to the national average (no systematic effect).
# H1: The average residential electricity price in VA, TX, and CA is
#     different from the national average.

library(tidyverse)
library(lubridate)
library(broom)

DATA_PATH <- "og_data.csv"

raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)

cat("Columns in input data:")
print(names(raw))

valid_states <- c(state.name, "District of Columbia")

elec_res <- raw %>%
  filter(
    sectorName == "residential",
    stateDescription %in% valid_states
  ) %>%
  mutate(
    date       = make_date(year, month, 1),
    year_month = floor_date(date, unit = "month")
  )

cat("Number of states after filtering:", length(unique(elec_res$stateDescription)), "")

#AI / data-center boom period
PERIOD_START <- as.Date("2016-01-01")
PERIOD_END   <- as.Date("2024-12-31")

hotspot_states <- c("Virginia", "Texas", "California")

panel <- elec_res %>%
  filter(date >= PERIOD_START, date <= PERIOD_END) %>%
  mutate(
    hotspot = stateDescription %in% hotspot_states
  )

cat("Observation counts by hotspot status:")
print(panel %>% count(hotspot))


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

comparison_monthly <- hotspot_monthly %>%
  left_join(rest_monthly, by = "year_month")

comparison_monthly <- hotspot_monthly %>%
  left_join(rest_monthly, by = "year_month")

cat("First few rows of monthly comparison:
")
print(head(comparison_monthly))

mu_hotspot <- mean(comparison_monthly$hotspot_price, na.rm = TRUE)
mu_rest    <- mean(comparison_monthly$rest_price, na.rm = TRUE)

sd_hotspot <- sd(comparison_monthly$hotspot_price, na.rm = TRUE)
sd_rest    <- sd(comparison_monthly$rest_price, na.rm = TRUE)

n_hotspot <- sum(!is.na(comparison_monthly$hotspot_price))
n_rest    <- sum(!is.na(comparison_monthly$rest_price))

se_diff <- sqrt(sd_hotspot^2 / n_hotspot + sd_rest^2 / n_rest)

# test statistic for H1: hotspot > rest
z_stat <- (mu_hotspot - mu_rest) / se_diff

# one-sided p-value (right tail)
p_value <- pnorm(z_stat, lower.tail = FALSE)

cat("Hotspot mean price:", mu_hotspot, "cents/kWh\n")
cat("Rest-of-states mean price:", mu_rest, "cents/kWh\n")
cat("Z statistic (hotspot - rest):", z_stat, "\n")
cat("One-sided p-value:", p_value, "\n")

z_results <- tibble(
  metric         = c("mean_us", "mean_hotspot", "sd_us", "sd_hotspot", "n_us", "n_hotspot", "z_stat", "p_value"),
  value          = c(mu_rest, mu_hotspot, sd_rest, sd_hotspot, n_rest, n_hotspot, z_stat, p_value)
)

print(z_results)

if (!dir.exists("output")) dir.create("output")

#Time series: US vs hotspot monthly prices

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

ggsave("output/ztest_ts.png", plot_ts, width = 8, height = 5, dpi = 300)