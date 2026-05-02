options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("httpgd")
df <- read.csv("us_power_data.csv")
#2001/01 - 2024/01

avgs <- df |>
  group_by(year, month, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    revenue = mean(revenue, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = paste(year, month, sep = "/"))

# make sure BOTH data frames have real Date columns
avgs$date <- as.Date(paste0(avgs$date, "/1"), format = "%Y/%m/%d")
df$date   <- as.Date(paste(df$year, df$month, "1", sep = "-"))

chosen_sector <- "residential"

us_avg <- subset(avgs, tolower(trimws(sector)) == chosen_sector)
us_avg <- subset(us_avg, !is.na(date) & !is.na(price))

nj <- subset(df,
             tolower(trimws(sector)) == chosen_sector &
             state == "New Jersey")
nj <- subset(nj, !is.na(date) & !is.na(price))

mn <- subset(df,
             tolower(trimws(sector)) == chosen_sector &
             state == "Minnesota")
mn <- subset(mn, !is.na(date) & !is.na(price))

start_date <- as.Date("2001-01-01")
end_date   <- as.Date("2024-12-01")
x_ticks <- seq(start_date, end_date, by = "1 year")

plot(us_avg$date, us_avg$price,
     type = "b",
     col = "black",
     pch = 19,
     lwd = 2,
     xlab = "Date",
     ylab = "Price",
     main = "Date vs Price",
     xlim = c(start_date, end_date),
     xaxt = "n")

lines(nj$date, nj$price, type = "b", col = "red", pch = 19, lwd = 2)
lines(mn$date, mn$price, type = "b", col = "blue", pch = 19, lwd = 2)

axis(1, at = x_ticks, labels = format(x_ticks, "%Y"))

legend("topleft",
       legend = c("US Avg", "New Jersey", "Minnesota"),
       col = c("black", "red", "blue"),
       lty = 1,
       pch = 19,
       lwd = 2)

event1d <- as.Date("2006-09-01")

i <- which.min(abs(nj$date - event1d))
event1x <- nj$date[i]
event1y <- nj$price[i]

points(event1x, event1y,
       col = "darkred",
       pch = 19,
       cex = 1.8)

text(event1x, event1y,
     labels = "Data center opened",
     pos = 3,
     col = "darkred")

abline(v = event1d, col = "red", lty = 2, lwd = 2)