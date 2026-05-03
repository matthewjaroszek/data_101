installs <- function(){
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("httpgd")
  library(httpgd)
  install.packages("dplyr")
  library(dplyr)
}
installs()

setup <- function(){
  df <- read.csv("us_power_data.csv")
  names(df) <- trimws(names(df))
  df$date <- as.Date(sprintf("%d-%02d-01", df$year, df$month))
  df <- df[, c("date", "year", "month", "sector", "state", "price", "sales")]
  df <- subset(df, tolower(trimws(df$sector)) %in% "residential")
  df <- subset(df, !is.na(df$date) & !is.na(df$price))
}
setup()

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

#Edit graph from here down
start <- as.Date("2001-01-01") #2001/01
end   <- as.Date("2024-12-01") #2024/01
x_ticks <- seq(start, end, by = "6 months")

png("plot.png", width = 1500, height = 700)
plot(avgs_year$date, avgs_year$price,
     type = "b",
     col = "black",
     pch = 20,
     lwd = 1,
     xlab = "Date",
     ylab = "Price",
     main = "Date vs Price",
     xlim = c(start, end),
     xaxt = "n")

line(avgs_6_month, "red")
line(avgs_month, "blue")

point(avgs_year, "2012-01-29", "black", "TESTING")
ablin("2012-07-1", "black")

axis(1, at = x_ticks, labels = format(x_ticks, "%Y"))

legend("topleft",
       legend = c("Year", "6 Months", "3 Months", "Month"),
       col = c("black", "red", "green", "blue"),
       lty = 1,
       pch = 19,
       lwd = 2)

#Dont delete this
dev.off()