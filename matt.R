options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("httpgd")
df <- read.csv("us_power_data.csv")

avgs <- df |>
  group_by(year, month, sectorName) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    revenue = mean(revenue, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = paste(year, month, sep = "/"))

avgs <- avgs |>
  select(date, year, month, sectorName, everything())
print(colnames(avgs))

avgs$date <- as.Date(paste0(avgs$date, "/1"), format = "%Y/%m/%d")
sectors <- avgs[avgs$sectorName == "residential", ]

start_date <- as.Date("2022-01-01")
end_date   <- as.Date("2024-12-01")

x_ticks <- seq(start_date, end_date, by = "1 months")

plot(sectors$date, sectors$price,
     type = "b",
     xlab = "Date",
     ylab = "Price",
     main = "Date vs Price",
     pch = 19,
     cex = 0.8,
     xlim = c(start_date, end_date),
     xaxt = "n")

axis(1, at = x_ticks, labels = format(x_ticks, "%Y/%m"))