#Dont delete pls
#Used for creating graphs for slides
source("config.R")

#Change x in file name
png("plotx.png", width = 1000, height = 700)

start <- as.Date("2001-01-01") # min : 2001/01
end   <- as.Date("2024-12-01") # max : 2024/01
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

#line(avgs_year, "black")
#line(avgs_6_month, "red")
#line(avgs_3_month, "green")
line(avgs_3_month, "red")

#point(avgs_year, "2012-01-29", "black", "TESTING")
#ablin("2010-07-1", "black")

axis(1, at = x_ticks, labels = format(x_ticks, "%Y"))

legend("topleft",
       legend = c("3 Month Mean", "6 Month Mean"),
       col = c("red", "black"),
       lty = 1,
       pch = 19,
       lwd = 2)

#Dont delete pls
dev.off()