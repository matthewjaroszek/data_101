# Predict US residential electricity price
# using total customers + total sales

DATA_PATH <- "og_data.csv"
TRAIN_END_DATE <- as.Date("2016-12-31")
OUTPUT_DIR <- "output"

library(tidyverse)
library(lubridate)

raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)

required_cols <- c(
  "year", "month", "stateDescription", "sectorName",
  "customers", "price", "revenue", "sales"
)

missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

valid_states <- c(state.name, "District of Columbia")

data_clean <- raw %>%
  mutate(
    stateDescription = trimws(as.character(stateDescription)),
    sectorName = tolower(trimws(as.character(sectorName))),
    year = as.integer(year),
    month = as.integer(month),
    price = as.numeric(price),
    sales = as.numeric(sales),
    customers = suppressWarnings(as.numeric(customers)),
    date = make_date(year, month, 1)
  ) %>%
  filter(
    stateDescription %in% valid_states,
    sectorName == "residential",
    !is.na(date),
    !is.na(price),
    !is.na(sales),
    !is.na(customers)
  )

if (nrow(data_clean) == 0) {
  stop("No valid residential state rows found.")
}

us_monthly <- data_clean %>%
  group_by(date) %>%
  summarise(
    us_price = weighted.mean(price, sales, na.rm = TRUE),
    sales_total = sum(sales, na.rm = TRUE),
    customers_total = sum(customers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  mutate(
    month_factor = factor(month(date), levels = 1:12),
    log_sales = log(sales_total),
    log_customers = log(customers_total)
  )

if (nrow(us_monthly) < 24) {
  stop("Not enough monthly data.")
}

train_df <- us_monthly %>%
  filter(date <= TRAIN_END_DATE)

test_df <- us_monthly %>%
  filter(date > TRAIN_END_DATE)

if (nrow(train_df) < 12) {
  stop("Training set too small.")
}

if (nrow(test_df) == 0) {
  stop("No test data. Pick an earlier TRAIN_END_DATE.")
}

model_cs <- lm(
  us_price ~ log_sales + log_customers + month_factor,
  data = train_df
)

cat("\nMODEL SUMMARY:\n")
print(summary(model_cs))

test_df <- test_df %>%
  mutate(
    pred_price = predict(model_cs, newdata = test_df)
  )

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

test_rmse <- rmse(test_df$us_price, test_df$pred_price)
cat("\nTEST RMSE:", round(test_rmse, 4), "\n")

full_df <- us_monthly %>%
  mutate(
    pred_price = predict(model_cs, newdata = us_monthly)
  )

plot_df <- full_df %>%
  select(date, actual = us_price, model = pred_price) %>%
  pivot_longer(
    cols = c(actual, model),
    names_to = "series",
    values_to = "price"
  ) %>%
  mutate(
    series = recode(series,
      actual = "Actual",
      model = "Customers + sales model"
    )
  )

p <- ggplot(plot_df, aes(x = date, y = price, color = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = TRAIN_END_DATE, linetype = "dashed", color = "gray40") +
  labs(
    x = NULL,
    y = "Residential price (cents per kWh)",
    color = "",
    title = "US residential price: actual vs customers+sales model",
    subtitle = paste0("Dashed line = train/test split at ", TRAIN_END_DATE)
  ) +
  scale_color_manual(values = c(
    "Actual" = "black",
    "Customers + sales model" = "steelblue"
  )) +
  theme_minimal()

print(p)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

ggsave(
  file.path(OUTPUT_DIR, "customers_sales_model_full_period.png"),
  plot = p,
  width = 9,
  height = 5,
  dpi = 300
)
