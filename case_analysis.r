
# AI Data Centers and Residential Electricity Prices
# -------------------------------------------------
# This script implements a panel difference-in-differences / event-study
# analysis of how large AI-oriented data centers may affect residential
# electricity prices at the US state level.
#
# EXPECTED INPUT
# --------------
# CSV file with at least the following columns:
#   - year: integer, year of observation (e.g., 2001–2024)
#   - month: integer, month of observation (1–12)
#   - stateDescription: state name string (e.g., "Iowa", "Georgia")
#   - sectorName: sector name (e.g., "Residential", "Industrial", etc.)
#   - customers: number of customers (can be missing)
#   - price: average price in cents per kWh (numeric)
#   - revenue: total revenue in millions of dollars (numeric)
#   - sales: total sales in millions of kWh (numeric)
#
# You should set DATA_PATH below to point to your CSV file.

# -----------------
# 0. Load libraries
# -----------------


library(tidyverse)
library(lubridate)
library(fixest)    # for fixed-effects regressions and event-study
library(broom)     # for tidying model outputs

# -------------
# 1. Read data
# -------------

# Update this path to your actual CSV file
DATA_PATH <- "og_data.csv"

raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
colnames(raw)

# Keep a copy of original names for reference
print(names(raw))

# -----------------------------
# 2. Basic cleaning and setup
# -----------------------------

# Focus on residential sector for the main outcome
# We also create a date variable and a year_month index

elec_res <- raw %>%
  filter(sectorName == "residential") %>%
  mutate(
    date       = make_date(year, month, 1),
    year_month = floor_date(date, unit = "month")
  )

# Optional: quick sanity checks
summary(elec_res$price)
length(unique(elec_res$stateDescription))

# ---------------------------------------------------------
# 3. Define AI/hyperscale data center events by state
# ---------------------------------------------------------

# We select five large, AI-relevant data center projects
# and approximate their "operational" start dates.
# Adjust event_start dates if you have more precise information.

events_raw <- tribble(
  ~stateDescription, ~event_name,                                        ~event_start,
  "Iowa",           "Microsoft Azure AI supercomputer campus (West Des Moines)", as.Date("2019-01-01"),
  "Georgia",        "Meta Newton / Stanton Springs Data Center",                as.Date("2021-01-01"),
  "Oklahoma",       "Google Pryor / MidAmerica Industrial Park expansion",     as.Date("2016-10-01"),
  "Nevada",         "Switch Tahoe Reno 1 (The Citadel Campus)",                as.Date("2017-02-01"),
  "Virginia",       "PowerHouse ABX-1 hyperscale campus (Ashburn)",            as.Date("2023-01-01")
)

# One (earliest) event per state

events_state <- events_raw %>%
  group_by(stateDescription) %>%
  summarise(
    first_event = min(event_start),
    .groups = "drop"
  )

print(events_state)

# ----------------------------------------------
# 4. Merge events and build treatment variables
# ----------------------------------------------

panel <- elec_res %>%
  left_join(events_state, by = "stateDescription") %>%
  mutate(
    treated_state = !is.na(first_event),
    # relative month (for treated states); untreated states get NA
    rel_month = if_else(
      treated_state,
      12L * (year - year(first_event)) + (month - month(first_event)),
      NA_integer_
    ),
    post = treated_state & rel_month >= 0
  )

# Quick check
panel %>%
  count(stateDescription, treated_state) %>%
  print(n = Inf)

# ----------------------------------
# 5. Baseline diff-in-diff regression
# ----------------------------------

# We estimate:
#   price_{s,t} = alpha_s + gamma_t + beta * post_{s,t} + eps_{s,t}
# where:
#   - alpha_s: state fixed effects
#   - gamma_t: time (year_month) fixed effects
#   - post_{s,t}: 1 for treated states after their first event
# Cluster standard errors at the state level.

model_did <- feols(
  price ~ post | stateDescription + year_month,
  data    = panel,
  cluster = ~ stateDescription
)

summary(model_did)

did_tidy <- broom::tidy(model_did)
print(did_tidy)

# ---------------------------------------------
# 6. Event-study around opening (treated only)
# ---------------------------------------------

# Restrict to treated states and a window around opening, e.g. -24 to +24 months

panel_es <- panel %>%
  filter(treated_state, !is.na(rel_month)) %>%
  filter(rel_month >= -24, rel_month <= 24)

# Estimate event-study with month -1 as reference

model_es <- feols(
  price ~ i(rel_month, ref = -1) | stateDescription + year_month,
  data    = panel_es,
  cluster = ~ stateDescription
)

summary(model_es)

# Tidy coefficients for plotting

es_tidy <- broom::tidy(model_es) %>%
  filter(str_starts(term, "rel_month::")) %>%
  separate(term, into = c("var", "k"), sep = "::", remove = FALSE) %>%
  mutate(k = as.integer(k)) %>%
  arrange(k)

print(es_tidy)

# ------------------------
# 7. Event-study plot
# ------------------------

# Save plots to an output directory

if (!dir.exists("output")) dir.create("output")

library(ggplot2)

p_es <- ggplot(es_tidy, aes(x = k, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_ribbon(aes(ymin = estimate - 1.96 * std.error,
                  ymax = estimate + 1.96 * std.error),
              alpha = 0.15, fill = "steelblue") +
  geom_line(color = "steelblue") +
  geom_point(size = 1.5, color = "steelblue4") +
  labs(
    x = "Months relative to first large AI data center opening",
    y = "Effect on residential price (cents per kWh, vs month -1)",
    title = "Event-study: Residential electricity prices around AI data center openings"
  ) +
  theme_minimal()

# Save PNG

ggsave("output/event_study_ai_datacenters.png", p_es, width = 8, height = 5, dpi = 300)

# -----------------------------------------------
# 8. State vs US average price time-series plots
# -----------------------------------------------

# Compute US average residential price by month (sales-weighted)

us_avg <- elec_res %>%
  group_by(year_month) %>%
  summarise(
    us_price = weighted.mean(price, sales, na.rm = TRUE),
    .groups = "drop"
  )

# Loop over treated states and create a plot for each

for (s in unique(events_state$stateDescription)) {
  plot_data <- elec_res %>%
    filter(stateDescription == s) %>%
    select(stateDescription, year_month, state_price = price) %>%
    left_join(us_avg, by = "year_month") %>%
    left_join(events_state, by = "stateDescription")

  p_state <- ggplot(plot_data, aes(x = year_month)) +
    geom_line(aes(y = state_price, color = "State"), linewidth = 0.7) +
    geom_line(aes(y = us_price,    color = "US average"), linewidth = 0.7) +
    geom_vline(aes(xintercept = first_event), linetype = "dashed", color = "black") +
    scale_color_manual(values = c("State" = "steelblue", "US average" = "firebrick")) +
    labs(
      x = NULL,
      y = "Residential price (cents per kWh)",
      color = "",
      title = paste("Residential electricity price:", s, "vs US average"),
      subtitle = "Dashed line = first major AI/hyperscale data center opening"
    ) +
    theme_minimal()

  fname <- paste0("output/state_vs_us_", gsub(" ", "_", tolower(s)), ".png")
  ggsave(fname, p_state, width = 8, height = 5, dpi = 300)
}

# End of script
