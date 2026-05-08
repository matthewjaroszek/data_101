DATA_PATH <- "og_data.csv"
TRAIN_END_DATE <- as.Date("2021-12-31")
PERIOD_START <- as.Date("2001-01-01")
PERIOD_END   <- as.Date("2023-12-31")

library(tidyverse)
library(lubridate)
library(forecast)

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

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))
mae  <- function(actual, predicted) mean(abs(actual - predicted), na.rm = TRUE)

last_value <- as.numeric(tail(train_data$hotspot_price, 1))
pred_naive <- rep(last_value, h)

# Model 2: linear trend + seasonality
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

# Model 5: piecewise linear with structural break chosen on training data
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
    pred_piecewise = pred_piecewise
  )

metrics <- tibble(
  model = c("Naive", "Linear + seasonality", "Piecewise linear + seasonality"),
  RMSE = c(
    rmse(results$hotspot_price, results$pred_naive),
    rmse(results$hotspot_price, results$pred_lm),
    rmse(results$hotspot_price, results$pred_piecewise)
  ),
  MAE = c(
    mae(results$hotspot_price, results$pred_naive),
    mae(results$hotspot_price, results$pred_lm),
    mae(results$hotspot_price, results$pred_piecewise)
  )
) %>% arrange(RMSE)

print(metrics)

results <- results %>%
  mutate(
    test_order = row_number(),
    late_test = test_order > floor(0.75 * n())
  )

# Plot 1: test-period actual vs predictions
plot_test <- results %>%
  select(year_month, actual = hotspot_price, pred_naive, pred_lm, pred_piecewise) %>%
  pivot_longer(-year_month, names_to = "series", values_to = "price") %>%
  mutate(series = recode(series,
                         actual = "Actual",
                         pred_naive = "Naive",
                         pred_lm = "Linear",
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

ggsave("output/failed_predictions.png", plot_test, width = 9, height = 5, dpi = 300)

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

ggsave("output/split.png", plot_history, width = 9, height = 5, dpi = 300)