# Regime-aware forecasting for hotspot residential electricity prices
# -----------------------------------------------------------------
# Purpose:
#   1. Build a prediction setup better suited to a late sharp increase.
#   2. Diagnose why simple models struggle.
#
# Main idea:
#   - Use hotspot monthly price series for Virginia, Texas, California.
#   - Compare several models, including one that allows a structural break.
#   - Produce charts and a short printed analysis.
#
# Models:
#   1. Naive last value
#   2. Linear trend + month seasonality
#   3. ETS exponential smoothing
#   4. Piecewise linear trend + month seasonality with one break chosen on train data
#
# User controls:
#   - DATA_PATH
#   - TRAIN_END_DATE
#   - PERIOD_START / PERIOD_END

DATA_PATH <- "og_data.csv"
TRAIN_END_DATE <- as.Date("2021-12-31")
PERIOD_START <- as.Date("2001-01-01")
PERIOD_END   <- as.Date("2023-12-31")

library(tidyverse)
library(lubridate)
library(forecast)

# -----------------
# Read and clean
# -----------------
raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
raw <- raw %>% mutate(stateDescription = str_trim(stateDescription))
valid_states <- c(state.name, "District of Columbia")
hotspot_states <- c("Virginia", "Texas", "California")

elec_res <- raw %>%
  filter(sectorName == "residential") %>%
  filter(stateDescription %in% valid_states) %>%
  mutate(
    date = make_date(year, month, 1),
    year_month = floor_date(date, unit = "month")
  )

panel <- elec_res %>%
  filter(date >= PERIOD_START, date <= PERIOD_END) %>%
  mutate(hotspot = stateDescription %in% hotspot_states)

missing_hotspots <- setdiff(hotspot_states, unique(panel$stateDescription))
if (length(missing_hotspots) > 0) {
  stop(paste("Missing hotspot states in filtered data:", paste(missing_hotspots, collapse = ", ")))
}

hotspot_monthly <- panel %>%
  filter(hotspot) %>%
  group_by(year_month) %>%
  summarise(
    hotspot_price = weighted.mean(price, sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_month)

if (nrow(hotspot_monthly) == 0) stop("Hotspot monthly series is empty after filtering.")

train_data <- hotspot_monthly %>% filter(year_month <= TRAIN_END_DATE)
test_data  <- hotspot_monthly %>% filter(year_month > TRAIN_END_DATE)

if (nrow(train_data) < 24) stop("Not enough training data; choose a later start period or later split.")
if (nrow(test_data) == 0) stop("No test data; choose an earlier TRAIN_END_DATE.")

# Monthly ts object
start_year <- year(min(hotspot_monthly$year_month))
start_month <- month(min(hotspot_monthly$year_month))
hotspot_ts <- ts(hotspot_monthly$hotspot_price, start = c(start_year, start_month), frequency = 12)
hotspot_ts_train <- window(hotspot_ts, end = c(year(max(train_data$year_month)), month(max(train_data$year_month))))
h <- nrow(test_data)

# -----------------
# Helper metrics
# -----------------
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))
mae  <- function(actual, predicted) mean(abs(actual - predicted), na.rm = TRUE)

# -----------------
# Model 1: naive
# -----------------
last_value <- as.numeric(tail(train_data$hotspot_price, 1))
pred_naive <- rep(last_value, h)

# -----------------
# Model 2: linear trend + seasonality
# -----------------
train_reg <- train_data %>%
  mutate(
    t_index = row_number(),
    month_factor = factor(month(year_month))
  )

model_lm <- lm(hotspot_price ~ t_index + month_factor, data = train_reg)

test_reg <- test_data %>%
  mutate(
    t_index = nrow(train_reg) + row_number(),
    month_factor = factor(month(year_month), levels = levels(train_reg$month_factor))
  )

pred_lm <- as.numeric(predict(model_lm, newdata = test_reg))

# -----------------
# Model 3: ETS
# -----------------
model_ets <- ets(hotspot_ts_train)
pred_ets <- as.numeric(forecast(model_ets, h = h)$mean)

# -----------------
# Model 4: piecewise linear with structural break chosen on training data
# -----------------
# Search for one break in the middle 50% of the training sample.
# This lets the model adapt to a changed slope instead of forcing one global line.

find_best_break <- function(df) {
  n <- nrow(df)
  candidates <- seq(max(18, floor(n * 0.25)), min(n - 18, ceiling(n * 0.75)))
  best_sse <- Inf
  best_b <- NA_integer_
  best_mod <- NULL

  for (b in candidates) {
    tmp <- df %>%
      mutate(
        post_break = pmax(0, t_index - b)
      )
    mod <- lm(hotspot_price ~ t_index + post_break + month_factor, data = tmp)
    sse <- sum(resid(mod)^2, na.rm = TRUE)
    if (is.finite(sse) && sse < best_sse) {
      best_sse <- sse
      best_b <- b
      best_mod <- mod
    }
  }

  list(best_break = best_b, model = best_mod)
}

break_fit <- find_best_break(train_reg)
best_break <- break_fit$best_break
model_piecewise <- break_fit$model

test_piece <- test_reg %>%
  mutate(post_break = pmax(0, t_index - best_break))

pred_piecewise <- as.numeric(predict(model_piecewise, newdata = test_piece))

# -----------------
# Collect predictions
# -----------------
results <- test_data %>%
  mutate(
    pred_naive = pred_naive,
    pred_lm = pred_lm,
    pred_ets = pred_ets,
    pred_piecewise = pred_piecewise
  )

metrics <- tibble(
  model = c("Naive", "Linear + seasonality", "ETS", "Piecewise linear + seasonality"),
  RMSE = c(
    rmse(results$hotspot_price, results$pred_naive),
    rmse(results$hotspot_price, results$pred_lm),
    rmse(results$hotspot_price, results$pred_ets),
    rmse(results$hotspot_price, results$pred_piecewise)
  ),
  MAE = c(
    mae(results$hotspot_price, results$pred_naive),
    mae(results$hotspot_price, results$pred_lm),
    mae(results$hotspot_price, results$pred_ets),
    mae(results$hotspot_price, results$pred_piecewise)
  )
) %>% arrange(RMSE)

print(metrics)

# -----------------
# Diagnose why error spikes
# -----------------
# Compare errors in early test vs late test to show that the sharp end increase is the main problem.

results <- results %>%
  mutate(
    test_order = row_number(),
    late_test = test_order > floor(0.75 * n())
  )

error_diagnostics <- tibble(
  segment = c("Early/Middle test", "Late test"),
  RMSE_piecewise = c(
    rmse(results$hotspot_price[!results$late_test], results$pred_piecewise[!results$late_test]),
    rmse(results$hotspot_price[results$late_test], results$pred_piecewise[results$late_test])
  ),
  RMSE_lm = c(
    rmse(results$hotspot_price[!results$late_test], results$pred_lm[!results$late_test]),
    rmse(results$hotspot_price[results$late_test], results$pred_lm[results$late_test])
  )
)

print(error_diagnostics)

# -----------------
# Plots
# -----------------
if (!dir.exists("output")) dir.create("output")

# Plot 1: test-period actual vs predictions
plot_test <- results %>%
  select(year_month, actual = hotspot_price, pred_naive, pred_lm, pred_ets, pred_piecewise) %>%
  pivot_longer(-year_month, names_to = "series", values_to = "price") %>%
  mutate(series = recode(series,
                         actual = "Actual",
                         pred_naive = "Naive",
                         pred_lm = "Linear",
                         pred_ets = "ETS",
                         pred_piecewise = "Piecewise")) %>%
  ggplot(aes(x = year_month, y = price, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = NULL,
    y = "Residential price (cents per kWh)",
    color = "Series",
    title = "Test-period forecasts vs actual hotspot prices",
    subtitle = paste0("Training through ", TRAIN_END_DATE)
  ) +
  theme_minimal()

ggsave("output/regime_test_forecasts.png", plot_test, width = 9, height = 5, dpi = 300)

# Plot 2: full history with train/test split and actual series
plot_history <- hotspot_monthly %>%
  mutate(sample = if_else(year_month <= TRAIN_END_DATE, "Train", "Test")) %>%
  ggplot(aes(x = year_month, y = hotspot_price, color = sample)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = TRAIN_END_DATE, linetype = "dashed") +
  labs(
    x = NULL,
    y = "Residential price (cents per kWh)",
    color = "Sample",
    title = "Hotspot price history with train/test split"
  ) +
  theme_minimal()

ggsave("output/regime_history_split.png", plot_history, width = 9, height = 5, dpi = 300)

# Plot 3: residuals for best two models
best_two <- metrics$model[1:2]
resid_df <- results %>%
  transmute(
    year_month,
    `Linear + seasonality` = hotspot_price - pred_lm,
    `Piecewise linear + seasonality` = hotspot_price - pred_piecewise,
    ETS = hotspot_price - pred_ets,
    Naive = hotspot_price - pred_naive
  ) %>%
  pivot_longer(-year_month, names_to = "model", values_to = "error") %>%
  filter(model %in% best_two)

plot_resid <- ggplot(resid_df, aes(x = year_month, y = error, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  labs(
    x = NULL,
    y = "Prediction error (actual - predicted)",
    color = "Model",
    title = "Prediction errors in the test period"
  ) +
  theme_minimal()

ggsave("output/regime_prediction_errors.png", plot_resid, width = 9, height = 5, dpi = 300)

# -----------------
# Printed interpretation
# -----------------
cat("
================ ANALYSIS ================
")
cat("Why simple prediction struggles:
")
cat("1. The series is mostly smooth, then accelerates sharply near the end of the test period.
")
cat("2. A single linear trend spreads that late jump across the whole sample, so it underpredicts the end spike.
")
cat("3. Naive and ETS can also lag when the level changes quickly.
")
cat("4. A piecewise trend helps because it allows slope to change after a break in the training data.

")
cat("Best break month index in training data:", best_break, "
")
cat("Best model by RMSE:
")
print(metrics %>% slice(1))
cat("
Error concentration check:
")
print(error_diagnostics)
cat("==========================================
")

plot_data <- results %>%
  select(
    year_month,
    actual = hotspot_price,
    pred_naive,
    pred_lm,
    pred_ets,
    pred_piecewise
  ) %>%
  pivot_longer(
    cols = c(actual, pred_naive, pred_lm, pred_ets, pred_piecewise),
    names_to = "series",
    values_to = "price"
  ) %>%
  mutate(
    series = recode(
      series,
      actual = "Actual",
      pred_naive = "Naive",
      pred_lm = "Linear",
      pred_ets = "ETS",
      pred_piecewise = "Piecewise"
    )
  )

p <- ggplot(plot_data, aes(x = year_month, y = price, color = series)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = NULL,
    y = "Residential price (cents per kWh)",
    color = "Series",
    title = "Actual vs predicted hotspot prices",
    subtitle = paste0("Training through ", TRAIN_END_DATE)
  ) +
  theme_minimal()

ggsave("output/actual_vs_predictions_with_piecewise.png", p, width = 9, height = 5, dpi = 300)