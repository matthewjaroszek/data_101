
# Predicting Residential Electricity Prices in Data Center Hotspot States
# ----------------------------------------------------------------------
# This script builds and evaluates simple forecasting models for
# residential electricity prices in data-center hotspot states
# (Virginia, Texas, California), using a train/test split based on
# a user-controlled cutoff date.
#
# MODELS
# ------
# 1. Naive last-value model (baseline)
# 2. Linear regression with time trend + month seasonality
# 3. ARIMA time series model (auto.arima)
#
# For each model, we:
#   - Train on data up to TRAIN_END_DATE
#   - Predict on the remaining test period
#   - Compute RMSE on the test period
#   - Plot actual vs predicted prices over time
#
# EXPECTED INPUT
# --------------
# CSV file with at least columns:
#   - year (int)
#   - month (int, 1-12)
#   - stateDescription (state or region name)
#   - sectorName (e.g., "Residential")
#   - price (numeric, cents per kWh)
#   - sales (numeric, millions of kWh)
#
# ----------------------------------------------------------------------

# -----------------
# 0. User parameters
# -----------------

# Path to your CSV data file
DATA_PATH <- "og_data.csv"

# Train/test split cutoff: all observations with date <= TRAIN_END_DATE
# are used to train the models; later observations are test data.
TRAIN_END_DATE <- as.Date("2021-12-31")  # <-- change this as needed

# -----------------
# 1. Load libraries
# -----------------

library(tidyverse)
library(lubridate)
library(forecast)   # for auto.arima and forecasting

# -------------
# 2. Read data
# -------------

raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)

cat("Columns in input data:
")
print(names(raw))

# -----------------------------
# 3. Basic cleaning and setup
# -----------------------------

# Keep only true US states + DC (drop regional aggregates)
valid_states <- c(state.name, "District of Columbia")

hotspot_states <- c("Virginia", "Texas", "California")

# Filter to residential sector and valid states, create date and year_month

elec_res <- raw %>%
  filter(
    sectorName == "residential",
    stateDescription %in% valid_states
  ) %>%
  mutate(
    date       = make_date(year, month, 1),
    year_month = floor_date(date, unit = "month"),
    hotspot    = stateDescription %in% hotspot_states
  )

cat("Number of states after filtering:", length(unique(elec_res$stateDescription)), "
")

# Restrict to a reasonable modern period (optional)
PERIOD_START <- as.Date("2001-01-01")
PERIOD_END   <- as.Date("2023-12-31")

panel <- elec_res %>%
  filter(date >= PERIOD_START, date <= PERIOD_END)

# ----------------------------------------------
# 4. Build a target series: hotspot average price
# ----------------------------------------------

# We aggregate to a single monthly series: sales-weighted average
# residential price across VA, TX, and CA.

hotspot_monthly <- panel %>%
  filter(hotspot) %>%
  group_by(year_month) %>%
  summarise(
    hotspot_price = weighted.mean(price, sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_month)

cat("First few rows of hotspot monthly series:
")
print(head(hotspot_monthly))

# ----------------------------
# 5. Train/test split by date
# ----------------------------

train_data <- hotspot_monthly %>%
  filter(year_month <= TRAIN_END_DATE)

test_data <- hotspot_monthly %>%
  filter(year_month > TRAIN_END_DATE)

cat("Training period:", min(train_data$year_month), "to", max(train_data$year_month), "
")
cat("Test period:", ifelse(nrow(test_data) > 0, paste(min(test_data$year_month), "to", max(test_data$year_month)), "<no test data>"), "
")

if (nrow(test_data) == 0) {
  stop("No test data: choose a TRAIN_END_DATE earlier than the last date in the data.")
}

# Create ts objects (monthly frequency) for models that need them

# Determine start and frequency
start_year  <- year(min(hotspot_monthly$year_month))
start_month <- month(min(hotspot_monthly$year_month))

# Full series
hotspot_ts <- ts(hotspot_monthly$hotspot_price,
                 start = c(start_year, start_month),
                 frequency = 12)

# Identify indices for train/test in the ts vector
train_last_index <- which(hotspot_monthly$year_month == max(train_data$year_month))

hotspot_ts_train <- window(hotspot_ts, end = c(year(max(train_data$year_month)), month(max(train_data$year_month))))

h <- nrow(test_data)  # forecast horizon = number of test months

# ------------------------
# 6. Model 1: Naive (last value)
# ------------------------

# Forecast: all future values equal to last training observation

last_value <- as.numeric(tail(train_data$hotspot_price, 1))

pred_naive <- rep(last_value, h)

# ------------------------
# 7. Model 2: Linear regression
# ------------------------

# Regression on time index + month as factor (seasonality)

train_reg <- train_data %>%
  mutate(
    t_index = row_number(),
    month_factor = factor(month(year_month))
  )

# Fit model

model_lm <- lm(hotspot_price ~ t_index + month_factor, data = train_reg)

summary(model_lm)

# Build test frame with the same structure

# Continue time index into the test period

test_reg <- test_data %>%
  mutate(
    t_index = nrow(train_reg) + row_number(),
    month_factor = factor(month(year_month), levels = levels(train_reg$month_factor))
  )

pred_lm <- predict(model_lm, newdata = test_reg)

# ------------------------
# 8. Model 3: ARIMA (auto.arima)
# ------------------------

model_arima <- auto.arima(hotspot_ts_train)

summary(model_arima)

fc_arima <- forecast(model_arima, h = h)

pred_arima <- as.numeric(fc_arima$mean)

# -----------------------------------
# 9. Combine predictions and evaluate
# -----------------------------------

results <- test_data %>%
  mutate(
    pred_naive = pred_naive,
    pred_lm    = pred_lm,
    pred_arima = pred_arima
  )

# Compute RMSE for each model

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

rmse_naive <- rmse(results$hotspot_price, results$pred_naive)
rmse_lm    <- rmse(results$hotspot_price, results$pred_lm)
rmse_arima <- rmse(results$hotspot_price, results$pred_arima)

rmse_table <- tibble(
  model = c("Naive last value", "Linear trend + month dummies", "ARIMA (auto.arima)"),
  RMSE  = c(rmse_naive, rmse_lm, rmse_arima)
)

cat("
Test-period RMSE by model (lower is better):
")
print(rmse_table)

# ------------------------
# 10. Visualization: actual vs predicted
# ------------------------

if (!dir.exists("output")) dir.create("output")

# Long format for ggplot

plot_data <- results %>%
  select(year_month, actual = hotspot_price, pred_naive, pred_lm, pred_arima) %>%
  pivot_longer(cols = c(actual, pred_naive, pred_lm, pred_arima),
               names_to = "series", values_to = "price") %>%
  mutate(
    series = recode(series,
                    actual = "Actual",
                    pred_naive = "Naive",
                    pred_lm = "Linear model",
                    pred_arima = "ARIMA")
  )

library(ggplot2)

p <- ggplot(plot_data, aes(x = year_month, y = price, color = series)) +
  geom_line(linewidth = 0.7) +
  labs(
    x = NULL,
    y = "Residential price (cents per kWh)",
    color = "Series",
    title = "Test-period prediction: hotspot residential prices",
    subtitle = paste0("Training up to ", TRAIN_END_DATE, "; test period only")
  ) +
  theme_minimal()

# Save plot

ggsave("output/prediction_hotspot_vs_models.png", p, width = 8, height = 5, dpi = 300)

# Also print RMSE table in a clean way

cat("
==============================
")
cat("Model comparison (RMSE):
")
print(rmse_table)
cat("==============================
")

# End of script
