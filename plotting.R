#Dont delete pls
source("config.R")


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
ablin("2010-07-1", "black")

axis(1, at = x_ticks, labels = format(x_ticks, "%Y"))

legend("topleft",
       legend = c("Year", "6 Months", "3 Months", "Month"),
       col = c("black", "red", "green", "blue"),
       lty = 1,
       pch = 19,
       lwd = 2)

#Dont delete pls
dev.off()