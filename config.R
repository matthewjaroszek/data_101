#Leave comments if you edit anything in this file

options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("httpgd")
library(httpgd)
install.packages("dplyr")
library(dplyr)

df <- read.csv("us_power_data.csv")
names(df) <- trimws(names(df))
df$date <- as.Date(sprintf("%d-%02d-01", df$year, df$month))
df <- df[, c("date", "year", "month", "sector", "state", "price", "sales")]
df <- subset(df, tolower(trimws(df$sector)) %in% "residential")
df <- subset(df, !is.na(df$date) & !is.na(df$price))

avgs_month <- df |>
  group_by(year, month, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))

avgs_3_month <- df |>
  mutate(period_3mo = floor((month - 1) / 3) + 1) |>
  group_by(year, period_3mo, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mid_month = c(2, 5, 8, 11)[period_3mo],
    date = as.Date(sprintf("%d-%02d-01", year, mid_month))
  )

avgs_6_month <- df |>
  mutate(period_6mo = floor((month - 1) / 6) + 1) |>
  group_by(year, period_6mo, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mid_month = c(3, 9)[period_6mo],
    date = as.Date(sprintf("%d-%02d-01", year, mid_month))
  )

avgs_year <- df |>
  group_by(year, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(sprintf("%d-07-01", year)))

sub_state <- function(df, states){
  df <- subset(df, tolower(trimws(df$state)) %in% tolower(trimws(states)))
  df <- subset(df, !is.na(df$date) & !is.na(df$price))
  return(df)
}

line <- function(df, color){
  lines(df$date, df$price, type = "b", col = color, pch = 20, lwd = 1)
}

point <- function(df, dates, color, label){
  dates <- as.Date(dates)
  i <- which.min(abs(df$date - dates))
  x <- df$date[i]
  y <- df$price[i]
  points(x, y,
       col = color,
       pch = 19,
       cex = 1)
  text(x, y,
     labels = label,
     pos = 3,
     col = color)
}

ablin <- function(date, color){
  abline(v = as.Date(date), col = color, lty = 2, lwd = 2)
}