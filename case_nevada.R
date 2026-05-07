source("config.R")

date = as.Date("2017-02-01")
start = as.Date("2016-02-01")
end = as.Date("2018-02-01")
state = "Nevada"

avgs_state = read.csv("us_power_data.csv")
sub_state(avgs_state, state)
avgs_state <- subset(avgs_state, tolower(trimws(avgs_state$sector)) %in% "residential")

avgs_state <- avgs_state |>
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

png("plot_case.png", width = 1000, height = 700)
x_ticks <- seq(start, end, by = "6 months") # "x month(s)/year(s)"

plot(avgs_6_month$date, avgs_6_month$price,
     type = "b",
     col = "black",
     pch = 20,
     lwd = 2,
     xlab = "Date",
     ylab = "Price per kWh in Cents",
     main = "Mean Price of Power in the US",
     xlim = c(start, end),
     xaxt = "n")


line(avgs_state, "red")
axis(1, at = x_ticks, labels = format(x_ticks, "%Y"))

legend("topleft",
       legend = c("US Mean", "State Mean"),
       col = c("black", "red"),
       lty = 1,
       pch = 19,
       lwd = 2)

#Dont delete pls
dev.off()